use std::cmp::max;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("8.txt").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let mut registers = HashMap::<&str, i32>::new();
    let mut max_ever = 0;
    for line in contents.lines() {
        let words: Vec<&str> = line.split_whitespace().collect();
        let to = words[0];
        let incdec = match words[1] {
            "inc" =>  1,
            "dec" => -1,
            _     => panic!("Couldn't parse incdec")
        };
        let to_amt = words[2].parse::<i32>().expect("Couldn't parse to_amt");
        let from = words[4];
        let op = |x, y| match words[5] {
            ">"  => x  > y,
            "<"  => x  < y,
            ">=" => x >= y,
            "<=" => x <= y,
            "!=" => x != y,
            "==" => x == y,
            _    => panic!("Couldn't parse operator")
        };
        let from_amt = words[6].parse().expect("Couldn't parse from_amt");
        let from_val = *registers.entry(from).or_insert(0);
        if op(from_val, from_amt) {
            let to_val = *registers.entry(to).or_insert(0);
            let new = val + incdec * to_amt;
            max_ever = max(max_ever, new);
            registers.insert(to, new);
        }
    }
    println!("Part 1: {}", registers.iter().map(|x| x.1).max().unwrap());
    println!("Part 2: {}", max_ever);
}
