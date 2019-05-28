use std::collections::HashMap;

pub struct Board {
    pub kernel_size: usize,
    pub cells: Vec<Vec<Option<u32>>>,
    pub char_encoding: Vec<char>
}

impl Board {
    fn new(kernel_size: usize, char_encoding: Vec<char>) -> Board {
        let board_size = kernel_size*kernel_size;
        Board {
            kernel_size,
            cells: vec![vec![None; board_size]; board_size],
            char_encoding
        }
    }

    fn board_size(&self) -> usize {
        self.kernel_size*self.kernel_size
    }

    fn render_cell(&self, row: usize, col: usize) -> char {
        self.cells[row][col].map_or('.', |i| self.char_encoding[i as usize])
    }

    pub fn render(&self) -> String {
        let mut result = String::new();
        for row in 0..self.board_size() {
            let mut line = String::new();
            for col in 0..self.board_size() {
                if col > 0 {
                    line.push(' ');
                }
                line.push(self.render_cell(row, col));
            }
            if !result.is_empty() {
                result.push('\n');
            }
            result.push_str(&line);
        }
        result
    }

    pub fn from_string(s: &String) -> Result<Board, String> {
        let mut result: Option<Board> = None;

        let mut alphabet: HashMap<char, u32> = HashMap::new();

        fn mapped_char(c: &char) -> bool {
            match c {
                ' ' | '.' | '\n' => false,
                _ => true
            }
        }

        for c in s.chars().filter(|c| mapped_char(c)) {
            if !alphabet.contains_key(&c) {
                alphabet.insert(c, alphabet.len() as u32);
            }
        }

        for (row, line) in s.lines().enumerate() {
            if result.is_none() {
                let dim: usize = line.chars().filter(|c| *c != ' ').count();

                if alphabet.len() != dim {
                    if alphabet.len() < dim {
                        let candidates = "123456789abcdefghijklmnopqrstuvwxyz".as_bytes();
                        let mut idx: usize = 0;

                        while alphabet.len() < dim {
                            let candidate = candidates[idx] as char;
                            idx += 1;
                            if !alphabet.contains_key(&candidate) {
                                alphabet.insert(candidate, alphabet.len() as u32);
                            }
                        }
                    } else {
                        println!("{:?}", alphabet);
                        return Result::Err(format!("Unique character count {} differs from dimension {}", alphabet.len(), dim));
                    }
                }

                let mut char_encoding: Vec<char> = vec!['.'; dim];
                for (key, val) in alphabet.iter() {
                    char_encoding[*val as usize] = *key;
                }

                if dim == 4 {
                    result = Some(Board::new(2, char_encoding));
                }
                else if dim == 9 {
                    result = Some(Board::new(3, char_encoding));
                }
                else if dim == 16 {
                    result = Some(Board::new(4, char_encoding));
                }
                else if dim == 25 {
                    result = Some(Board::new(5, char_encoding));
                }
                else {
                    return Result::Err(format!("Dimension {} unsupported", dim));
                }
            }
            for (col, c) in line.chars().filter(|c| *c != ' ').enumerate() {
                let encoding: Option<u32> = match c {
                    '.' => None,
                    ch  => alphabet.get(&ch).map(|i| *i)
                };
                if let Some(ref mut b) = result {
                    b.cells[row][col] = encoding;
                }
            }
        }

        Result::Ok(result.unwrap())
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_board_size() {
        let board = Board::from_string(&"acb.\ndb..\n....\n....".to_string());
        match board {
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
            Ok(b) => {
                assert_eq!(b.kernel_size, 2);
                assert_eq!(b.board_size(), 4);
            }
        }
    }

    #[test]
    fn test_board_cell_init() {
        let board = Board::from_string(&"acb.\ndb..\n....\n....".to_string());
        match board {
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
            Ok(b) => {
                fn cell_char(b: &Board, row: usize, col: usize) -> Option<char> {
                    b.cells[row][col].map(|i| b.char_encoding[i as usize])
                }
                assert_eq!(cell_char(&b, 0, 0), Some('a'));
                assert_eq!(cell_char(&b, 0, 1), Some('c'));
                assert_eq!(cell_char(&b, 0, 2), Some('b'));
                assert_eq!(cell_char(&b, 1, 1), Some('b'));
                assert_eq!(cell_char(&b, 3, 3), None);
            }
        }
    }
}