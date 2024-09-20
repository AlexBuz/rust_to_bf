fn main() {
    let mut point = Point { x: 3, y: 4 };
    let mut origin = Point { x: 0, y: 0 };
    let mut distance = manhattan_distance((point, origin));
    println!("Manhattan distance: %d", distance);
    origin.x = 1;
    distance = manhattan_distance((point, origin));
    println!("Manhattan distance: %d", distance);
    origin.y = 2;
    distance = manhattan_distance((point, origin));
    println!("Manhattan distance: %d", distance);
}

struct Point {
    x: usize,
    y: usize,
}

fn abs_diff(a: usize, b: usize) -> usize {
    if a > b {
        return a - b;
    } else {
        return b - a;
    }
}

fn manhattan_distance(pair_of_points: (Point, Point)) -> usize {
    let x_diff = abs_diff(pair_of_points.0.x, pair_of_points.1.x);
    let y_diff = abs_diff(pair_of_points.0.y, pair_of_points.1.y);
    return x_diff + y_diff;
}
