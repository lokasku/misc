pub mod sudoku;
pub mod utils;

use crate::utils::read_file;
use crate::sudoku::Grid;

fn main() {
    let sdk = read_file("model.txt");
    let mut sdk = Grid { content: sdk };
    
    if sdk.solve() {
        let content = sdk.get_as("line");
        for line in content {
            println!("{:?}", line);
        }
    } else {
        panic!("Cannot solve Sudoku.");
    }
}