use super::board;
use super::state;


impl From<&board::Board> for state::State {
    fn from(b: &board::Board) -> Self {
        let my_cells = b.cells.clone();
        state::State::new(my_cells)
    }
}
