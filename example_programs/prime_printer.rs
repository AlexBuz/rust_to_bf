fn main() {
    print!("Enter the number of primes that you would like: ");
    let mut count = read_int();
    let mut candidate = 2;
    while count != 0 {
        if is_prime(candidate) {
            print!("%d, ", candidate);
            count -= 1;
        }
        candidate += 1;
    }
    println!("...");
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
