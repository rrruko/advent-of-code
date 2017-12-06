use std::cmp::Ordering;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("input").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let mut ns: Vec<i32> = contents
        .split_whitespace()
        .map(|s| s.parse().ok())
        .filter_map(|x| x)
        .collect();
    println!("Part 1: {}", redistribute(&mut ns));
    println!("Part 2: {}", redistribute(&mut ns));
 }

fn redistribute(ns: &mut Vec<i32>) -> u32 {
    let mut history: Vec<Vec<i32>> = Vec::new();
    let mut timer = 0;
    while let None = history.iter().cloned().find(|x| x == ns) {
        history.push(ns.clone());

        let (maximum, max_index) = ns.iter().cloned()
            .zip(0..)
            .max_by(|x, y| cmp_first(&x.0, &y.0))
            .unwrap();

        let mut ix = max_index;
        ns[ix] = 0;
        for _ in 0..maximum {
            ix += 1;
            if ix >= ns.len() {
                ix = 0;
            }
            ns[ix] += 1;
        }
        timer += 1;
    }
    timer
}

fn cmp_first(a: &i32, b: &i32) -> Ordering {
    if a.cmp(b) == Ordering::Equal {
        Ordering::Greater
    } else {
        a.cmp(b)
    }
}
