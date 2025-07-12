use libc::{c_double, c_int};

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct ReactorState {
    pub temperature: c_double,
    pub pressure: c_double,
    pub flow: c_double,
    pub pump_on: c_int,
    pub valve_open: c_int,
    pub failure: c_int,
}

extern "C" {
    pub fn reactor_init();
    pub fn reactor_step();
    pub fn reactor_get_state() -> ReactorState;
    pub fn reactor_set_valve(open: c_int);
    pub fn reactor_set_pump(on: c_int);
    pub fn reactor_inject_failure(f: c_int);
}
