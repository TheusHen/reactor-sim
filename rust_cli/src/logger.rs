use crate::ffi::ReactorState;
use std::fs::OpenOptions;
use std::io::Write;
use std::sync::Mutex;
use once_cell::sync::Lazy;

pub static LOGS: Lazy<Mutex<Vec<ReactorState>>> = Lazy::new(|| Mutex::new(Vec::new()));

pub fn log_state(state: &ReactorState) {
    let mut logs = LOGS.lock().unwrap();
    logs.push(*state);
}

pub fn export_csv(filename: &str) {
    let logs = LOGS.lock().unwrap();
    let mut wtr = csv::Writer::from_path(filename).unwrap();
    wtr.write_record(&["temperature", "pressure", "flow", "pump_on", "valve_open", "failure"]).unwrap();
    for s in logs.iter() {
        wtr.write_record(&[
            s.temperature.to_string(),
            s.pressure.to_string(),
            s.flow.to_string(),
            s.pump_on.to_string(),
            s.valve_open.to_string(),
            s.failure.to_string(),
        ]).unwrap();
    }
    wtr.flush().unwrap();
}