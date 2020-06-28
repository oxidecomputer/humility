/*
 * Copyright 2020 Oxide Computer Company
 */

use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub struct HumilityError {
    errmsg: String,
}

impl<'a> From<&'a str> for HumilityError {
    fn from(msg: &'a str) -> HumilityError {
        msg.to_string().into()
    }
}

impl From<String> for HumilityError {
    fn from(errmsg: String) -> HumilityError {
        HumilityError { errmsg }
    }
}

impl fmt::Display for HumilityError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.errmsg)
    }
}

impl Error for HumilityError {
    fn description(&self) -> &str {
        &self.errmsg
    }
}

pub fn err<S: ToString>(msg: S) -> Box<dyn Error> {
    Box::new(HumilityError::from(msg.to_string()))
}

#[macro_export]
macro_rules! err {
    ($fmt:expr) => (
        Err(err($fmt))
    );
    ($fmt:expr, $($arg:tt)*) => (
        Err(err(&format!($fmt, $($arg)*)))
    )
}

