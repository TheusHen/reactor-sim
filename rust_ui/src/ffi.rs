use std::ffi::CStr;
use std::os::raw::{c_char, c_float, c_int};

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ReactorState {
    pub core_temperature: c_float,
    pub pressure: c_float,
    pub coolant_flow: c_float,
    pub pump_on: bool,
    pub valve_position: c_float,
    pub control_rods: c_float,
    pub alert_flag: bool,
    pub fault_message: [c_char; 100],
}

extern "C" {
    pub fn reactor_initialize();
    pub fn reactor_step();
    pub fn reactor_set_pump_speed(speed: c_float);
    pub fn reactor_set_valve_position(pos: c_float);
    pub fn reactor_set_control_rods(pos: c_float);
    pub fn reactor_inject_fault(fault_id: c_int);
    pub fn reactor_get_state() -> ReactorState;
}

impl ReactorState {
    pub fn fault_message(&self) -> String {
        let cstr = unsafe { CStr::from_ptr(self.fault_message.as_ptr()) };
        cstr.to_string_lossy().trim().to_string()
    }
}
