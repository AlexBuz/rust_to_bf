fn main() {
    print!("Enter a number to calculate the corresponding Fibonacci number: ");
    println!("Result: %d", fib(read_int()));
}

fn fib(mut n: usize) -> usize {
    match n {
        0 => return 0,
        1 => return 1,
        _ => return fib(n - 1) + fib(n - 2),
    }
}
