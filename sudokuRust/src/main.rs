use std::env;

mod bitset;
mod board;
mod input;
mod state;
mod converters;
mod timed;

use state::State;

fn main() {
    let args: Vec<String> = env::args().collect();

    let board = input::read(&args[1]);

    match board {
        Ok(b) => {
            let mut s = state::State::from(&b);
            let (t, solved) = timed::timed(&mut || s.solve());
//            let solved = solver();
//            let t = 0;
            if solved {
                fn to_option(value: i32) -> Option<u32> {
                    if value < 0 {
                        None
                    } else {
                        Some(value as u32)
                    }
                }
                let result = board::Board {
                    kernel_size: s.kernel_size,
                    cells: s.cells.into_iter().map(|r| r.into_iter().map(to_option).collect()).collect(),
                    char_encoding: b.char_encoding
                };
                println!("Solution found in {} milliseconds", t);
                println!("{}", result.render());
            } else {
                println!("No solutions found (in {} milliseconds)", t);
            }
        },
        Err(e) => {
            println!("Error: {}", e);
        },
    };
}
