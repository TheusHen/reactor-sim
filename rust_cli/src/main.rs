mod ffi;
mod ui;
mod controls;
mod logger;

use crossterm::event::{self, Event, KeyEvent};
use std::{thread, time::Duration};

fn main() {
    unsafe { ffi::reactor_init(); }
    let mut blink = false;

    loop {
        unsafe { ffi::reactor_step(); }
        let state = unsafe { ffi::reactor_get_state() };
        logger::log_state(&state);

        ui::draw_ui(&state, blink);
        blink = !blink;

        if event::poll(Duration::from_millis(100)).unwrap() {
            if let Event::Key(key) = event::read().unwrap() {
                use controls::UserAction::*;
                match controls::interpret_key(key) {
                    OpenValve => unsafe { ffi::reactor_set_valve(1); },
                    CloseValve => unsafe { ffi::reactor_set_valve(0); },
                    StartPump => unsafe { ffi::reactor_set_pump(1); },
                    StopPump => unsafe { ffi::reactor_set_pump(0); },
                    InjectFailure(f) => unsafe { ffi::reactor_inject_failure(f as i32); },
                    ExportLogs => logger::export_csv("logs.csv"),
                    Exit => break,
                    None => {}
                }
            }
        }

        thread::sleep(Duration::from_millis(200));
    }

    println!("\nLogs exported to logs.csv");
}