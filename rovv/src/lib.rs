pub use rovv_derive::{dyn_row, row};

pub trait Empty {}
impl<T> Empty for T {}

use lens_rs;

include!(concat!(env!("OUT_DIR"), "/dyn_row.rs"));
