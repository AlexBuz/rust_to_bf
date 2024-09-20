fn main() {
    print!("Enter a number to calculate the corresponding triangular number: ");
    let n = read_int();
    let result = triangle(n);
    println!(
        "The sum of the first %d positive integers is %d.",
        n, result
    );
}

fn triangle(mut n: usize) -> usize {
    match n {
        0 => return 0,
        _ => return triangle(n - 1) + n,
    }
}
