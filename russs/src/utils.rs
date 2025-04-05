use std::fs;
use crate::sudoku::Cell;


pub fn  read_file(file: &str) -> Vec<Vec<Option<u32>>> {
    let sdk = fs::read_to_string(file).expect("Cannot read file.");
    let sdk = sdk.split("\r\n");
    let sdk = sdk.collect::<Vec<&str>>();

    let mut sdk_by_lines: Vec<Vec<Option<u32>>> = vec![];
    
    for line in sdk {
        let mut line_content: Vec<Option<u32>> = vec![];
        for char in line.chars() {
            line_content.push(char.to_digit(10u32));}
        sdk_by_lines.push(line_content);
    }

    match sdk_by_lines.len() {
        9 => for line in &sdk_by_lines {
                match line.len() {
                    9 => continue,
                    _ => panic!("One of the lines of the sudoku does not contain 9 characters.")
                }
            },
        _ => panic!("Your sudoku doesnt contains 9 lines.")
    }
    sdk_by_lines
}

pub fn get_index_as(a: u32, b: u32, kind: &str) -> Cell {
    match kind {
        "line" => Cell {value: None, a: a, b: b},
        "column" => Cell {value: None, a: b, b: a},
        "square" => Cell {
            value: None,
            a: b / 3 + (a / 3) * 3,
            b: b % 3 + (a % 3) * 3
        },
        &_ => panic!("The kind either 'line', 'column' or 'square'.")
    }
}