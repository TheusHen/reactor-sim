use crossterm::event::{KeyCode, KeyEvent};
use std::io::{stdout, Write};
use crossterm::{execute, cursor, terminal::{Clear, ClearType}, style::{Print, SetForegroundColor, Color, ResetColor}};

pub enum UserAction {
    None,
    OpenValve,
    CloseValve,
    StartPump,
    StopPump,
    InjectFailure(usize),
    ExportLogs,
    Exit,
}

#[derive(Debug, Copy, Clone)]
pub struct FailureItem {
    pub code: usize,
    pub name: &'static str,
    pub description: &'static str,
}

#[derive(Debug)]
pub struct FailureCategory {
    pub name: &'static str,
    pub failures: &'static [FailureItem],
}

pub const FAILURE_CATEGORIES: &[FailureCategory] = &[
    FailureCategory {
        name: "Mechanical",
        failures: &[
            FailureItem { code: 1, name: "Leak", description: "Leak in primary piping" },
            FailureItem { code: 2, name: "Pump failure", description: "Main pump failed" },
            FailureItem { code: 3, name: "Stuck valve", description: "Circuit valve stuck" },
            FailureItem { code: 10, name: "Stuck control rod", description: "Control rod stuck" },
        ],
    },
    FailureCategory {
        name: "Hydraulic",
        failures: &[
            FailureItem { code: 8, name: "Flooding", description: "Flooding in machine room" },
            FailureItem { code: 9, name: "Pipe blockage", description: "Partial blockage in flow" },
            FailureItem { code: 11, name: "Loss of cooling", description: "Cooling system failed" },
            FailureItem { code: 12, name: "Air ingress", description: "Air entered closed circuit" },
        ],
    },
    FailureCategory {
        name: "Electrical",
        failures: &[
            FailureItem { code: 4, name: "Power loss", description: "Main power source lost" },
        ],
    },
    FailureCategory {
        name: "Sensor",
        failures: &[
            FailureItem { code: 5, name: "Sensor failure", description: "Temperature sensor offline" },
        ],
    },
    FailureCategory {
        name: "Operational",
        failures: &[
            FailureItem { code: 6, name: "Overheating", description: "Reaction rate raised temperature" },
            FailureItem { code: 7, name: "SCRAM failure", description: "SCRAM did not activate correctly" },
        ],
    },
];

pub fn interpret_key(ev: KeyEvent) -> UserAction {
    match ev.code {
        KeyCode::Char('q') => UserAction::Exit,
        KeyCode::Char('o') => UserAction::OpenValve,
        KeyCode::Char('c') => UserAction::CloseValve,
        KeyCode::Char('p') => UserAction::StartPump,
        KeyCode::Char('s') => UserAction::StopPump,
        KeyCode::Char('f') => UserAction::InjectFailure(choose_failure()),
        KeyCode::Char('e') => UserAction::ExportLogs,
        _ => UserAction::None,
    }
}

pub fn choose_failure() -> usize {
    let mut stdout = stdout();
    let mut cat_idx = 0;
    let mut fail_idx = 0;
    let n_cat = FAILURE_CATEGORIES.len();

    loop {
        execute!(stdout, cursor::MoveTo(0, 0), Clear(ClearType::All)).unwrap();
        execute!(stdout, SetForegroundColor(Color::White), Print("Choose a failure category:\n")).unwrap();

        for (i, cat) in FAILURE_CATEGORIES.iter().enumerate() {
            if i == cat_idx {
                execute!(stdout, SetForegroundColor(Color::Green), Print(format!("> {}\n", cat.name))).unwrap();
            } else {
                execute!(stdout, SetForegroundColor(Color::White), Print(format!("  {}\n", cat.name))).unwrap();
            }
        }

        execute!(stdout, ResetColor).unwrap();
        execute!(stdout, Print("\n↑/↓ navigate, Enter select, Esc cancel\n")).unwrap();

        if let crossterm::event::Event::Key(ev) = crossterm::event::read().unwrap() {
            match ev.code {
                KeyCode::Up => {
                    if cat_idx == 0 { cat_idx = n_cat - 1; } else { cat_idx -= 1; }
                }
                KeyCode::Down => {
                    cat_idx = (cat_idx + 1) % n_cat;
                }
                KeyCode::Enter => {
                    let failures = FAILURE_CATEGORIES[cat_idx].failures;
                    let n_fail = failures.len();
                    loop {
                        execute!(stdout, cursor::MoveTo(0, 0), Clear(ClearType::All)).unwrap();
                        execute!(stdout, SetForegroundColor(Color::White),
                            Print(format!("Category: {}\n", FAILURE_CATEGORIES[cat_idx].name))).unwrap();
                        for (j, f) in failures.iter().enumerate() {
                            if j == fail_idx {
                                execute!(stdout, SetForegroundColor(Color::Green),
                                    Print(format!("> {} - {}\n", f.name, f.description))).unwrap();
                            } else {
                                execute!(stdout, SetForegroundColor(Color::White),
                                    Print(format!("  {} - {}\n", f.name, f.description))).unwrap();
                            }
                        }
                        execute!(stdout, ResetColor).unwrap();
                        execute!(stdout, Print("\n↑/↓ navigate, Enter inject failure, Esc back\n")).unwrap();

                        if let crossterm::event::Event::Key(ev2) = crossterm::event::read().unwrap() {
                            match ev2.code {
                                KeyCode::Up => {
                                    if fail_idx == 0 { fail_idx = n_fail - 1; } else { fail_idx -= 1; }
                                }
                                KeyCode::Down => {
                                    fail_idx = (fail_idx + 1) % n_fail;
                                }
                                KeyCode::Enter => {
                                    return failures[fail_idx].code;
                                }
                                KeyCode::Esc => break,
                                _ => {}
                            }
                        }
                    }
                }
                KeyCode::Esc => return 0,
                _ => {}
            }
        }
    }
}