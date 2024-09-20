fn main() {
    print!("Enter a number to check if it's prime: ");
    if is_prime(read_int()) {
        println!("Prime.");
    } else {
        println!("Composite.");
    }
}

fn is_prime(num: usize) -> bool {
    let mut divisor = 2;
    while num != divisor {
        if num % divisor == 0 {
            return false;
        }
        divisor += 1;
    }
    return true;
}
