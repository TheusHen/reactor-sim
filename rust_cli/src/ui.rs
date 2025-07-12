use crate::ffi::ReactorState;
use crate::controls::{FAILURE_CATEGORIES, FailureItem};
use crossterm::{
    cursor,
    execute,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::{Clear, ClearType},
};
use std::io::{stdout, Write};

fn get_failure_name(code: usize) -> &'static str {
    if code == 0 { return "None"; }
    for cat in FAILURE_CATEGORIES {
        for f in cat.failures {
            if f.code == code { return f.name; }
        }
    }
    "Unknown"
}

pub fn draw_ui(state: &ReactorState, blink: bool) {
    let mut stdout = stdout();
    execute!(stdout, cursor::MoveTo(0, 0), Clear(ClearType::All)).unwrap();

    execute!(stdout, SetForegroundColor(Color::White), Print("== Nuclear Failure Simulator ==\n\n")).unwrap();

    execute!(stdout, SetForegroundColor(Color::Cyan), Print("Temperature: ")).unwrap();
    bar(state.temperature, 250.0, 1200.0, 60, Color::Red, blink && state.temperature > 800.0);
    execute!(stdout, Print(format!(" {:.1} °C\n", state.temperature))).unwrap();

    execute!(stdout, SetForegroundColor(Color::Cyan), Print("Pressure:    ")).unwrap();
    bar(state.pressure, 50.0, 300.0, 60, Color::Yellow, blink && state.pressure > 200.0);
    execute!(stdout, Print(format!(" {:.1} atm\n", state.pressure))).unwrap();

    execute!(stdout, SetForegroundColor(Color::Cyan), Print("Flow:        ")).unwrap();
    bar(state.flow, 0.0, 200.0, 60, Color::Green, false);
    execute!(stdout, Print(format!(" {:.1} L/s\n", state.flow))).unwrap();

    execute!(stdout, SetForegroundColor(Color::Magenta), Print("\nPumps: ")).unwrap();
    execute!(
        stdout,
        SetForegroundColor(if state.pump_on == 1 { Color::Green } else { Color::Red }),
        Print(if state.pump_on == 1 { "On" } else { "Off" })
    )
    .unwrap();

    execute!(stdout, SetForegroundColor(Color::Magenta), Print("  |  Valve: ")).unwrap();
    execute!(
        stdout,
        SetForegroundColor(if state.valve_open == 1 { Color::Green } else { Color::Red }),
        Print(if state.valve_open == 1 { "Open" } else { "Closed" })
    )
    .unwrap();

    if state.failure != 0 {
        execute!(
            stdout,
            SetForegroundColor(Color::Red),
            Print(format!("\n\n[! WARNING: Active Failure: {} !]\n", get_failure_name(state.failure as usize)))
        )
        .unwrap();
    }

    if blink && (state.temperature > 1000.0 || state.pressure > 250.0) {
        execute!(
            stdout,
            SetForegroundColor(Color::Red),
            Print("\n*** CRITICAL: CORE MELTDOWN RISK ***\n")
        )
        .unwrap();
        print!("\x07");
    }

    execute!(
        stdout,
        SetForegroundColor(Color::White),
        Print("\n\n[o] Open valve  [c] Close valve  [p] Start pump  [s] Stop pump\n"),
        Print("[f] Inject failure  [e] Export logs  [q] Quit\n")
    )
    .unwrap();

    execute!(stdout, ResetColor).unwrap();
}

fn bar(val: f64, min: f64, max: f64, width: usize, color: Color, blink: bool) {
    let mut stdout = stdout();
    let percent = ((val - min) / (max - min)).clamp(0.0, 1.0);
    let filled = (percent * width as f64) as usize;

    let style = if blink { Color::Red } else { color };
    execute!(stdout, SetForegroundColor(style)).unwrap();
    for i in 0..width {
        if i < filled {
            print!("█");
        } else {
            print!(" ");
        }
    }
    execute!(stdout, ResetColor).unwrap();
}