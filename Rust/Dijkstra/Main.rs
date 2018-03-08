use std::collections::HashSet;
use std::collections::LinkedList;

fn read_graph() -> (i32, i32, i32, Vec<Vec<i32>>) {
    let mut line0 = String::new();
    let _ = std::io::stdin().read_line(&mut line0);
    let n_vertex: i32 = line0.trim().parse().unwrap();
    let mut line1 = String::new();
    let _ = std::io::stdin().read_line(&mut line1);
    let split: Vec<&str> = line1.trim().split(' ').collect();
    let (start, goal): (i32, i32) = (split[0].parse().unwrap(), split[1].parse().unwrap());

    let mut edge: Vec<Vec<i32>> = (0..n_vertex).map(|_| (0..n_vertex).map(|_| -1).collect()).collect();

    loop {
        let mut line = String::new();
        match std::io::stdin().read_line(&mut line) {
            Ok(n) if n > 0 => {
                let split: Vec<&str> = line.trim().split(' ').collect();
                let v: Vec<i32> = split.iter().map(|&x| x.parse().unwrap()).collect();
                edge[v[0] as usize][v[1] as usize] = v[2];
                edge[v[1] as usize][v[0] as usize] = v[2];
            },
            Ok(_) => break,
            Err(_) => println!("Error: input")
        }
    }
    (n_vertex, start, goal, edge)
}

fn search(n_vertex: i32, start: i32, goal: i32, edge: Vec<Vec<i32>>) -> LinkedList<i32> {
    let infty = 1000000;
    let mut unreached: HashSet<i32> = (0..n_vertex).collect();
    let mut distance: Vec<i32> = (0..n_vertex).map(|_| infty).collect();
    let mut previous: Vec<Option<i32>> = (0..n_vertex).map(|_| None).collect();

    distance[start as usize] = 0;

    while ! &unreached.is_empty() {
        let mut neighbor_index = -1;
        let mut neighbor_distance = infty;

        for &v in &unreached {
            if distance[v as usize] < neighbor_distance {
                neighbor_index = v;
                neighbor_distance = distance[v as usize];
            }
        }

        unreached.remove(&neighbor_index);

        for i in 0..n_vertex {
            if i == neighbor_index || edge[neighbor_index as usize][i as usize] == -1 {
                continue;
            }
            if distance[i as usize] > distance[neighbor_index as usize] + edge[neighbor_index as usize][i as usize] {
                distance[i as usize] = distance[neighbor_index as usize] + edge[neighbor_index as usize][i as usize];
                previous[i as usize] = Some(neighbor_index);
            }
        }
    }
    follow(goal, previous)
}

fn follow(goal: i32, previous: Vec<Option<i32>>) -> LinkedList<i32> {
    let mut current = goal;
    let mut route = LinkedList::new();
    loop {
        route.push_front(current);
        if let Some(pred) = previous[current as usize] {
            current = pred;
        } else {
            break;
        }
    }
    route
}

fn main() {
    let (n_vertex, start, goal, edge) = read_graph();
    let route = search(n_vertex, start, goal, edge);
    let mut iter = route.iter();
    print!("{}", start);
    let _ = iter.next();
    loop {
        if let Some(node) = iter.next() {
            print!(" -> {}", node);
        } else {
            break;
        }
    }
    print!("\n");
}
