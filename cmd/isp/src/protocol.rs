// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{anyhow, Result};
use byteorder::LittleEndian;
use crc_any::CRCu16;
use num_derive::FromPrimitive;
use std::convert::TryInto;
use strum_macros::EnumString;
use zerocopy::{AsBytes, FromBytes};

#[repr(u8)]
#[derive(Debug)]
enum PacketType {
    Ack = 0xA1,
    //Nak = 0xA2,
    AckAbort = 0xA3,
    Command = 0xA4,
    Data = 0xA5,
    Ping = 0xA6,
    PingResponse = 0xA7,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum ResponseCode {
    Generic = 0xA0,
    ReadMemory = 0xA3,
    GetProperty = 0xA7,
    //FlashReadOnce = 0xAF,
    KeyProvision = 0xB5,
}

#[repr(u32)]
#[derive(Copy, Clone, Debug)]
pub enum KeyType {
    // Secureboot Key Encryption Key
    // SBKEK = 0x3,
    // Prince = 7 - 9
    // USER is available to use for whatever
    // Wish there were more than one user :(
    // USERKEK = 0xB,
    // UDS used in DICE
    Uds = 0xC,
}

#[repr(u32)]
#[derive(Copy, Clone, Debug)]
pub enum KeyProvisionCmds {
    Enroll = 0x0,
    //SetUserKey = 0x1,
    SetIntrinsicKey = 0x2,
    WriteNonVolatile = 0x3,
    //ReadNonVolatile = 0x4,
    WriteKeyStore = 0x5,
    //ReadKeyStore = 0x6,
}

// Commands are abridged right now for what we care about
#[repr(u8)]
#[derive(Debug)]
pub enum CommandTag {
    FlashEraseAll = 0x1,
    ReadMemory = 0x3,
    WriteMemory = 0x4,
    GetProperty = 0x7,
    KeyProvision = 0x15,
}

#[repr(C)]
#[derive(Debug, AsBytes, FromBytes)]
pub struct PacketHeader {
    start_byte: u8,
    packet_type: u8,
}

impl PacketHeader {
    fn new(ptype: PacketType) -> PacketHeader {
        PacketHeader { start_byte: 0x5A_u8, packet_type: ptype as u8 }
    }
}

#[repr(C)]
#[derive(Debug, EnumString, FromPrimitive, Clone, Copy)]
pub enum BootloaderProperty {
    BootloaderVersion = 1,
    AvailablePeripherals = 2,
    FlashStart = 3,
    FlashSize = 4,
    FlashSectorSize = 5,
    AvailableCommands = 7,
    CRCStatus = 8,
    VerifyWrites = 10,
    MaxPacketSize = 11,
    ReservedRegions = 12,
    RAMStart = 14,
    RAMSize = 15,
    SystemDeviceID = 16,
    SecurityState = 17,
    UniqueID = 18,
    TargetVersion = 24,
    FlashPageSize = 27,
    IRQPinStatus = 28,
    FFRKeyStoreStatus = 29,
}

#[repr(C)]
#[derive(Debug, AsBytes, FromBytes)]
pub struct PingResponse {
    header: PacketHeader,
    protocol_bugfix: u8,
    protocol_minor: u8,
    protocol_major: u8,
    protocol_name: u8,
    options: zerocopy::byteorder::U16<LittleEndian>,
    crc16: zerocopy::byteorder::U16<LittleEndian>,
}

#[repr(C)]
#[derive(Debug, AsBytes, FromBytes)]
pub struct FramingPacket {
    header: PacketHeader,
    length: zerocopy::byteorder::U16<LittleEndian>,
    crc16: zerocopy::byteorder::U16<LittleEndian>,
}

impl FramingPacket {
    fn new(ptype: PacketType, length: u16) -> FramingPacket {
        FramingPacket {
            header: PacketHeader::new(ptype),
            length: zerocopy::U16::new(length),
            crc16: zerocopy::U16::ZERO,
        }
    }
}

#[derive(Debug, Default, AsBytes, FromBytes)]
#[repr(C)]
pub struct RawCommand {
    tag: u8,
    flags: u8,
    reserved: u8,
    parameter_count: u8,
}

impl RawCommand {
    fn new(c: CommandTag, count: usize) -> RawCommand {
        RawCommand {
            tag: c as u8,
            flags: 0,
            reserved: 0,
            parameter_count: count as u8,
        }
    }
}

// Command packets can take a variable number
// of arguments. This is unfortunately a pain to serialize
// in a structure. So we cheat and later append the
// arguments manually
#[derive(Debug, AsBytes)]
#[repr(C)]
pub struct VariablePacket {
    packet: FramingPacket,
    raw_command: RawCommand,
}

pub struct CommandPacket<'a> {
    packet: VariablePacket,
    params: &'a [u32],
}

impl<'a> CommandPacket<'a> {
    fn new_command(
        c: CommandTag,
        args: &'a [u32],
    ) -> Result<CommandPacket<'a>> {
        let arg_bytes = args.len() * 4;
        // Total length of the command packet. the 4 bytes are for
        // the fixed fields
        let len: u16 = (4 + arg_bytes) as u16;

        let mut v = VariablePacket {
            packet: FramingPacket::new(PacketType::Command, len),
            raw_command: RawCommand::new(c, args.len()),
        };

        let mut crc = CRCu16::crc16xmodem();

        let bytes = v.as_bytes();

        // CRC over everything except the CRC field, this includes the framing
        // header as well as the rest of the argument
        crc.digest(&bytes[..0x4]);
        crc.digest(&bytes[0x6..]);

        for e in args.iter() {
            crc.digest(&e.to_le_bytes());
        }

        let digest = crc.get_crc();

        v.packet.crc16 = zerocopy::U16::new(digest);

        Ok(CommandPacket { packet: v, params: args })
    }

    fn to_bytes(&self) -> Result<Vec<u8>> {
        let mut v = Vec::new();

        v.extend_from_slice(self.packet.as_bytes());

        v.extend(self.params.iter().flat_map(|e| e.to_le_bytes()));

        Ok(v)
    }
}

pub struct DataPacket {
    packet: FramingPacket,
    data: Vec<u8>,
}

impl DataPacket {
    fn new_data(args: Vec<u8>) -> Result<DataPacket> {
        let arg_len: u16 = args.len() as u16;

        let mut f = FramingPacket::new(PacketType::Data, arg_len);

        let mut crc = CRCu16::crc16xmodem();

        let bytes = f.as_bytes();

        crc.digest(&bytes[..0x4]);
        crc.digest(&bytes[0x6..]);
        crc.digest(&args);

        let digest = crc.get_crc();

        f.crc16 = zerocopy::U16::new(digest);

        Ok(DataPacket { packet: f, data: args })
    }

    fn to_bytes(&self) -> Result<Vec<u8>> {
        let mut v = Vec::new();

        v.extend_from_slice(self.packet.as_bytes());
        v.extend_from_slice(&self.data);

        Ok(v)
    }
}

pub fn process_ping(port: &mut dyn serialport::SerialPort) -> Result<()> {
    let ping = PacketHeader::new(PacketType::Ping);

    port.write_all(ping.as_bytes())?;

    port.flush()?;

    let mut response_bytes: [u8; 10] = [0; 10];

    port.read_exact(&mut response_bytes)?;

    let response = PingResponse::read_from(&response_bytes[..])
        .ok_or_else(|| anyhow!("Ping Response Failure"))?;

    if response.header.packet_type != (PacketType::PingResponse as u8) {
        return Err(anyhow!(
            "Incorrect ACK byte from ping {:x}",
            response.header.packet_type
        ));
    }

    Ok(())
}

fn send_ack(port: &mut dyn serialport::SerialPort) -> Result<()> {
    let packet = PacketHeader::new(PacketType::Ack);
    port.write_all(packet.as_bytes())?;
    port.flush()?;

    Ok(())
}

fn read_ack(port: &mut dyn serialport::SerialPort) -> Result<()> {
    let mut ack_bytes: [u8; 2] = [0; 2];

    port.read_exact(&mut ack_bytes)?;

    let ack = PacketHeader::read_from(ack_bytes.as_slice())
        .ok_or_else(|| anyhow!("parse err"))?;

    // Ack abort comes with a response packet explaining why
    if ack.packet_type == (PacketType::AckAbort as u8) {
        match read_response(port, ResponseCode::Generic) {
            Ok(p) => {
                if p.is_empty() {
                    return Err(anyhow!(
                        "Response returned an unknown error code?"
                    ));
                }

                match p[0] {
                    10203 => {
                        return Err(anyhow!(
                            "Did you forget to erase the flash? (err 10203)"
                        ))
                    }
                    10101 => {
                        return Err(anyhow!(
            "Incorrect signature. Is the SBKEK set correctly? (err 10101)"
        ))
                    }
                    retval => {
                        return Err(anyhow!("ISP error returned: {}", retval))
                    }
                }
            }
            Err(e) => return Err(e),
        }
    }

    if ack.packet_type != (PacketType::Ack as u8) {
        return Err(anyhow!("Incorrect ACK byte {:x}", ack.packet_type));
    }

    Ok(())
}

fn check_crc(
    frame_bytes: &[u8],
    response: &[u8],
    frame: &FramingPacket,
) -> Result<()> {
    let mut crc = CRCu16::crc16xmodem();
    crc.digest(&frame_bytes[..0x4]);
    crc.digest(&frame_bytes[0x6..]);
    crc.digest(&response);

    let digest = crc.get_crc();

    if digest != frame.crc16.get() {
        return Err(anyhow!(
            "CRC failure on packet expect {:x} got {:x}",
            frame.crc16.get(),
            digest
        ));
    }

    Ok(())
}

pub fn read_data(port: &mut dyn serialport::SerialPort) -> Result<Vec<u8>> {
    let mut frame_bytes = vec![0; core::mem::size_of::<FramingPacket>()];

    port.read_exact(&mut frame_bytes)?;

    let frame = FramingPacket::read_from(frame_bytes.as_slice())
        .ok_or_else(|| anyhow!("parse_error"))?;

    if frame.header.packet_type != (PacketType::Data as u8) {
        return Err(anyhow!(
            "Expected a data packet, got {:x} instead",
            frame.header.packet_type
        ));
    }

    let length = frame.length.get() as usize;
    let mut response = vec![0; length];

    port.read_exact(&mut response)?;

    check_crc(&frame_bytes, &response, &frame)?;

    Ok(response)
}

pub fn read_response(
    port: &mut dyn serialport::SerialPort,
    response_type: ResponseCode,
) -> Result<Vec<u32>> {
    let mut frame_bytes = vec![0; core::mem::size_of::<FramingPacket>()];

    port.read_exact(&mut frame_bytes)?;

    let frame = FramingPacket::read_from(frame_bytes.as_slice())
        .ok_or_else(|| anyhow!("Parse_err"))?;

    // A response packet is a specific type of command packet.
    if frame.header.packet_type != (PacketType::Command as u8) {
        return Err(anyhow!(
            "Expected a command, got {:x}",
            frame.header.packet_type
        ));
    }

    let length = frame.length.get() as usize;
    let mut response = vec![0; length];

    port.read_exact(&mut response)?;

    check_crc(&frame_bytes, &response, &frame)?;

    let command =
        RawCommand::read_from(&response[..core::mem::size_of::<RawCommand>()])
            .ok_or_else(|| anyhow!("Parse error"))?;

    if command.tag != (response_type as u8) {
        return Err(anyhow!(
            "Expected a response type of {:x}, got {:x}",
            response_type as u8,
            command.tag
        ));
    }

    let mut params: Vec<u32> = Vec::new();
    let mut index = core::mem::size_of::<RawCommand>();

    while params.len() < command.parameter_count as usize {
        params.push(u32::from_le_bytes(response[index..index + 4].try_into()?));
        index += 4;
    }

    send_ack(port)?;

    // First paramter is always the return code;
    match params[0] {
        0 => Ok(params),
        10203 => Err(anyhow!("Did you forget to erase the flash? (err 10203)")),
        10101 => Err(anyhow!(
            "Incorrect signature. Is the SBKEK set correctly? (err 10101)"
        )),
        retval => Err(anyhow!("ISP error returned: {}", retval)),
    }
}

pub fn send_command(
    port: &mut dyn serialport::SerialPort,
    cmd: CommandTag,
    args: &[u32],
) -> Result<()> {
    let command = CommandPacket::new_command(cmd, args)?;

    let command_bytes = command.to_bytes()?;

    port.write_all(&command_bytes)?;
    port.flush()?;

    read_ack(port)?;

    Ok(())
}

pub fn send_data(
    port: &mut dyn serialport::SerialPort,
    data: &[u8],
) -> Result<()> {
    // Target doesn't like it when we send an entire binary in one pass
    // so break it down into 512 byte chunks which is what the existing
    // tools seem to use
    for chunk in data.chunks(512) {
        let data_packet = DataPacket::new_data(chunk.to_vec())?;

        let data_bytes = data_packet.to_bytes()?;

        port.write_all(&data_bytes)?;
        port.flush()?;

        read_ack(port)?;
    }

    Ok(())
}

pub fn recv_data(
    port: &mut dyn serialport::SerialPort,
    cnt: u32,
) -> Result<Vec<u8>> {
    let mut data = Vec::new();

    while data.len() < (cnt as usize) {
        let d = read_data(port)?;
        data.extend_from_slice(&d);

        send_ack(port)?;
    }

    Ok(data)
}
