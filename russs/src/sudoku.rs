use crate::utils::get_index_as;

#[derive(Debug, Copy, Clone)]
pub struct Cell {
    pub value: Option<u32>,
    pub a: u32,
    pub b: u32
}

impl Cell {
    pub fn get_value(&self) -> Option<u32> { self.value }
    pub fn get_a(&self) -> u32 { self.a }
    pub fn get_b(&self) -> u32 { self.b }
}

#[derive(Debug)]
pub struct Grid {
    pub content: Vec<Vec<Option<u32>>>
}

impl Grid {
    pub fn solve(&mut self) -> bool {
        if self.is_full() {
            return self.is_valid()
        }

        let fec = self.first_empty_cell().unwrap();
        let lpd = self.get_possible_digits(fec);

        if lpd.len() == 0 {
            return false
        }

        for d in lpd {
            self.set(fec, d.unwrap());
            if self.solve() {
                return true
            }
        }
        self.unset(fec);
        return false
    }

    pub fn set(&mut self, cell: Cell, v: u32) {
        if v > 9 {
            panic!("{:?} must be between 0 and 9.", v);}
        self.content[cell.get_a() as usize][cell.get_b() as usize] = Some(v)
    }

    pub fn unset(&mut self, cell: Cell) {
        self.content[cell.get_a() as usize][cell.get_b() as usize] = None
    }

    pub fn is_valid(&self) -> bool {
        for f in ["line", "column", "square"] { // f -> format
            for g in self.get_as(f) { // g -> group
                for d in &g { // d -> digital
                    if !(g.iter().filter(|&n| *n == *d).count() > 1) || *d == None {
                        continue
                    }
                    println!("Review your sudoku.");
                    return false
                }
            }
        }
        true
    }

    pub fn is_full(&self) -> bool {
        for i_line in self.get_as("line") {
            for i_num in i_line {
                if i_num == None {
                    return false;
                } 
            }
        }
        true
    }

    pub fn first_empty_cell(&self) -> Result<Cell, Option<u32>> {
        for i_line in 0..9 {
            for i_num in 0..9 {
                match self.content[i_line][i_num] {
                    None => return Ok(Cell {
                        value: self.content[i_line][i_num],
                        a: i_line as u32,
                        b: i_num as u32
                    }),
                    _ => continue
                };
            }
        }
        Err(None)
    }

    pub fn get_as(&self, kind: &str) -> Vec<Vec<Option<u32>>> {
        let mut new_content: Vec<Vec<Option<u32>>> = vec![];
        for _a in 0..9 {
            let mut sub_vec: Vec<Option<u32>> = vec![];
            for _b in 0..9 {
                sub_vec.push(None);
            }
            new_content.push(sub_vec);
        }
        for i_line in 0..9 {
            for i_num in 0..9 {
                let i_converted = get_index_as(i_line, i_num, kind);
                new_content[i_converted.get_a() as usize][i_converted.get_b() as usize] = self.content[i_line as usize][i_num as usize];
            }
        }
        return new_content;
    }

    pub fn get_possible_digits(&self, cell: Cell) -> Vec<Option<u32>> {
        if cell.get_value() != None {
            return vec![cell.get_value()]
        }

        let mut list_digits: Vec<Option<u32>> = vec![];
        for i in 1..10 {list_digits.push(Some(i))}
        
        for kind in ["line", "column", "square"] {
            for d in self.get_as(kind)[get_index_as(cell.get_a(), cell.get_b(), kind).get_a() as usize].to_owned() {
                if d != None {
                    if list_digits.contains(&d) {
                        let index = list_digits.iter().position(|&i_d| i_d == d);
                        list_digits.remove(index.unwrap() as usize);
                    }
                }
            }
        }
        list_digits
    }
}