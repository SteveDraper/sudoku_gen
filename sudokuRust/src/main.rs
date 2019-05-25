use std::env;

mod bitset;
mod board;
mod input;
mod state;
mod converters;
mod timed;

fn main() {
    let args: Vec<String> = env::args().collect();

    let board = input::read(&args[1]);

    match board {
        Ok(b) => {
            let mut s = state::State::from(&b);
            let (t, solved) = timed::timed(&mut || s.solve());
            if solved {
                fn to_option(value: i32) -> Option<u32> {
                    if value < 0 {
                        None
                    } else {
                        Some(value as u32)
                    }
                }
                let mut cells = Vec::new();
                for row in 0..s.board_size {
                    let mut row_vec = Vec::new();
                    for col in 0..s.board_size {
                        row_vec.push(to_option(s.cells[s.cell_index(row, col)]));
                    }
                    cells.push(row_vec);
                }
                let result = board::Board {
                    kernel_size: s.kernel_size,
                    cells: cells,
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
