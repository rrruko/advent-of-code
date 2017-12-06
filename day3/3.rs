use std::collections::HashMap;

fn main() {
    part1();
    part2();
}

fn part1() {
    let mut hashmap = HashMap::<(i32, i32), u32>::new();
    let mut loc = (0, 0);
    let mut dir = (1, 0);
    hashmap.insert((0, 0), 1);

    let mut val = 1;
    while val < 312051 {
        dir = new_dir(loc, dir);
        loc = (loc.0 + dir.0, loc.1 + dir.1);
        val += 1;
        hashmap.insert(loc, val);
    }

    println!("Part 1: {}", loc.0.abs() + loc.1.abs());
}

fn part2() {
    let mut hashmap = HashMap::<(i32, i32), u32>::new();
    let mut loc = (0, 0);
    let mut dir = (1, 0);
    hashmap.insert((0, 0), 1);

    let mut val = 0;
    while val < 312051 {
        dir = new_dir(loc, dir);
        loc = (loc.0 + dir.0, loc.1 + dir.1);
        val = sum_neighbors(&hashmap, loc);
        hashmap.insert(loc, val);
    }
    println!("Part 2: {}", val);
}

fn new_dir(loc: (i32, i32), dir: (i32, i32)) -> (i32, i32) {
    let (x, y) = loc;
    if x > 0 && y > 0 && x == y {
        (-1, 0)
    } else if x < 0 && y > 0 && x == -y {
        (0, -1)
    } else if x < 0 && y < 0 && x == y {
        (1,  0)
    } else if x > 0 && y <= 0 && x == -y + 1 {
        (0,  1)
    } else {
        dir
    }
}

fn sum_neighbors(hashmap: &HashMap<(i32, i32), u32>, loc: (i32, i32)) -> u32 {
    let (x, y) = loc;
    let neighbors = vec![
        (x-1,y-1), (x-1,y),   (x-1,y+1),
        (x,  y-1),            (x,  y+1),
        (x+1,y-1), (x+1,y),   (x+1,y+1)];
    
    neighbors.iter()
        .map(|neighbor| hashmap.get(neighbor).unwrap_or(&0))
        .sum()
}
