/*
 * Copyright 2020 Oxide Computer Company
 */

use std::error::Error;

pub trait Core {
    fn read_word_32(&mut self, addr: u32) -> Result<u32, Box<dyn Error>>;
    fn read_8(&mut self, addr: u32, data: &mut [u8]) ->
        Result<(), Box<dyn Error>>;
    fn read_swv(&mut self) -> Result<Vec<u8>, Box<dyn Error>>;
    fn write_word_32(&mut self, addr: u32, data: u32) ->
        Result<(), Box<dyn Error>>;
    fn halt(&mut self) -> Result<(), Box<dyn Error>>;
    fn run(&mut self) -> Result<(), Box<dyn Error>>;
}

pub struct ProbeCore {
    pub core: probe_rs::Core,
    pub session: probe_rs::Session
}

impl Core for ProbeCore {
    fn read_word_32(&mut self, addr: u32) -> Result<u32, Box<dyn Error>> {
        Ok(self.core.read_word_32(addr)?)
    }

    fn read_8(&mut self,
        addr: u32,
        data: &mut [u8]
    ) -> Result<(), Box<dyn Error>> {
        Ok(self.core.read_8(addr, data)?)
    }

    fn read_swv(&mut self) -> Result<Vec<u8>, Box<dyn Error>> {
        Ok(self.session.read_swv()?)
    }

    fn write_word_32(&mut self,
        addr: u32,
        data: u32
    ) -> Result<(), Box<dyn Error>> {
        Ok(self.core.write_word_32(addr, data)?)
    }

    fn halt(&mut self) -> Result<(), Box<dyn Error>> {
        self.core.halt()?;
        Ok(())
    }

    fn run(&mut self) -> Result<(), Box<dyn Error>> {
        Ok(self.core.run()?)
    }
}
