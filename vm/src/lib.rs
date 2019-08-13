extern crate num;
#[macro_use]
extern crate num_derive;
#[macro_use]
extern crate scopeguard;
#[macro_use]
extern crate lazy_static;

pub mod chunk;
pub mod compiler;
pub mod value;
pub mod vm;
pub mod context;

pub(crate) const DEBUG: bool = true;
