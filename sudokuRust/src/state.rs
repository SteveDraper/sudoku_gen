
use crate::bitset::BitSet;
use crate::bitset::BitSet32;

#[derive(Debug)]
pub struct State {
    row_sets: Vec<BitSet32>,
    column_sets: Vec<BitSet32>,
    square_sets: Vec<BitSet32>,
    pub cells: Vec<i32>,
    pub kernel_size: usize,
    pub board_size: usize,
    open: u32,
    seed_idx: usize
}

impl State {
    pub fn new(board_cells: Vec<Vec<Option<u32>>>) -> State {
        let board_size = board_cells.len();
        let mut row_sets = State::full_choice_set(board_size as u32);
        let mut column_sets = State::full_choice_set(board_size as u32);
        let mut square_sets = State::full_choice_set(board_size as u32);
        let mut cells = vec![-1; board_size*board_size];
        let kernel_size: usize = match board_size {
            4 => 2,
            9 => 3,
            16 => 4,
            25 => 5,
            _ => panic!("Illegal board size"),
        };
        let mut open: u32 = 0;

        for (row, row_vec) in board_cells.iter().enumerate() {
            if row_vec.len() != board_size as usize {
                panic!(format!("Row {} has length {} which does not match board size of {}",
                              row,
                              row_vec.len(),
                              board_size));
            }
            for (col, cell) in row_vec.iter().enumerate() {
                match cell {
                    Some(cell_val) => {
                        let square_index = State::square_index(kernel_size, row, col);
                        if *cell_val >= board_size as u32 {
                            panic!(format!("Cell ({},{}) has illegal value {}",
                                          row,
                                          col,
                                           cell_val));
                        } else if !row_sets[row].has_member(*cell_val as u32) ||
                                  !column_sets[col].has_member(*cell_val as u32) ||
                                  !square_sets[square_index].has_member(*cell_val as u32) {
                            panic!("Illegal initial board position");
                        } else {
                            row_sets[row].remove(*cell_val as u32);
                            column_sets[col].remove(*cell_val as u32);
                            square_sets[square_index].remove(*cell_val as u32);
                            cells[State::cell_idx(board_size, row, col)] = *cell_val as i32;
                        }
                    },
                    None => {
                        open = open + 1;
                    }
                }
            }
        }

        State { row_sets, column_sets, square_sets, cells, kernel_size, board_size, open, seed_idx: 0 }
    }

    fn set_cell(&mut self, row: usize, col: usize, value: u32) {
        unsafe {
            let s_idx = self.sq_idx(row, col);
            self.open = self.open - 1;
            self.row_sets.get_unchecked_mut(row).remove(value);
            self.column_sets.get_unchecked_mut(col).remove(value);
            self.square_sets.get_unchecked_mut(s_idx).remove(value);
            *self.cells.get_unchecked_mut(State::cell_idx(self.board_size, row, col)) = value as i32;
        }
    }

    fn unset_cell(&mut self, row: usize, col: usize, value: u32) {
        unsafe {
            let s_idx = self.sq_idx(row, col);
            self.open = self.open + 1;
            self.row_sets.get_unchecked_mut(row).insert(value);
            self.column_sets.get_unchecked_mut(col).insert(value);
            self.square_sets.get_unchecked_mut(s_idx).insert(value);
            *self.cells.get_unchecked_mut(State::cell_idx(self.board_size, row, col)) = -1;
        }
    }

    fn full_choice_set(size: u32) -> Vec<BitSet32> {
        let all_choices = (0..size).fold(BitSet32::empty(), |mut acc, i| acc.insert(i));
        (0..size).map(|_| all_choices).collect()
    }

    fn square_index(kernel_size: usize, row: usize, col: usize) -> usize {
        (row/kernel_size)*kernel_size + (col/kernel_size)
    }

    fn sq_idx(&self, row: usize, col: usize) -> usize {
        State::square_index(self.kernel_size, row, col)
    }

    fn cell_idx(board_size: usize, row: usize, col: usize) -> usize {
        (row*board_size) + col
    }

    pub fn cell_index(&self, row: usize, col: usize) -> usize {
        State::cell_idx(self.board_size, row, col)
    }

    pub fn is_solution(&self) -> bool {
        self.open == 0
    }

    pub fn solve(&mut self) -> bool {
        self.solve_internal(1)
    }

    pub fn solve_internal(&mut self, min_constraint: u32) -> bool {
        if self.is_solution() {
            true
        } else {
            let (row, col, choices) =
                self.most_constrained(min_constraint);
            for choice in choices.iter() {
                self.set_cell(row, col, choice);
                if self.solve_internal(choices.card()-1) {
                    return true;
                } else {
                    self.unset_cell(row, col, choice);
                }
            }
            false
        }
    }

    fn most_constrained(&self, min_open_constraint: u32) -> (usize, usize, BitSet32) {
        let mut min_open: u32 = std::u32::MAX;
        let mut best_row: usize = 0;
        let mut best_col: usize = 0;
        let mut best_choice_set: BitSet32 = BitSet32::empty();

        unsafe {
            'outer: for row in 0..self.board_size {
                let row_set: BitSet32 = *self.row_sets.get_unchecked(row);
                for col in 0..self.board_size {
                    if *self.cells.get_unchecked(self.cell_index(row, col)) < 0 {
                        let open = row_set
                            .intersection(&self.column_sets.get_unchecked(col))
                            .intersection(&self.square_sets.get_unchecked(self.sq_idx(row, col)));
                        let open_count = open.card_below(min_open);
                        if open_count < min_open {
                            min_open = open_count;
                            best_row = row;
                            best_col = col;
                            best_choice_set = open;
                            if min_open <= min_open_constraint {
                                break 'outer;
                            }
                            else if min_open <= min_open_constraint+2 {
                                break;
                            }
                        }
                    }
                }
            }
        }

        (best_row, best_col, best_choice_set)
    }
}