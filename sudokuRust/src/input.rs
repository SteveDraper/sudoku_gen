use super::board;

use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::io::Error;

use board::Board;

fn read_internal(mut f: &File) -> Result<String, Error> {
    let mut result = String::new();
    f.read_to_string(&mut result).map(|_| result)
}

pub fn read(filename: &str) -> Result<Board, String> {
    let path = Path::new(filename);
    let file = File::open(&path).map_err(|e|e.to_string())?;
    let content = read_internal(&file).map_err(|e|e.to_string())?;
    Board::from_string(&content)
}

mod test {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_read() {
        let cargo = read("../test_data/test4_solvable.txt");

        match cargo {
            Ok(s) => println!("{}", s.render()),
            Err(e) => {
                println!("{}", e);
                assert!(false);
            }
        }
    }
}