use std::fs::File;
use std::io::prelude::*;

fn main() {
    // A lot of unwrapping follows because I really just want an exception if 
    // parsing fails
    let mut file = File::open("5.txt").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let lines: Vec<i32> = contents
        .lines()
        .map(|line| line.parse().unwrap())
        .collect();

    // part 1
    let mut lines1 = lines.clone();
    let mut ix: i32 = 0;
    let mut timer = 0;
    while ix >= 0 && ix < lines1.len() as i32 {
        let oldix = ix as usize;
        ix += lines1[oldix];
        lines1[oldix] += 1;
        timer += 1;
    }
    println!("Part 1: {}", timer);

    // part 2
    let mut lines2 = lines.clone();
    let mut ix: i32 = 0;
    let mut timer = 0;
    while ix >= 0 && ix < lines2.len() as i32 {
        let oldix = ix as usize;
        ix += lines2[oldix];
        if lines2[oldix] >= 3 {
            lines2[oldix] -= 1;
        } else {
            lines2[oldix] += 1;
        }
        timer += 1;
    }
    println!("Part 2: {}", timer);
}
