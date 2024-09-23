fn main() {
    print!("Enter a positive integer to print its Collatz sequence, or press enter to print the sequence of integers with Collatz sequences of maximum length up to that point: ");
    let num = read_int();
    match num {
        0 => print_max_sequence(),
        _ => print_sequence(num),
    }
}

fn print_sequence(mut num: usize) {
    while num != 1 {
        print!("{num}, ");
        match num % 2 {
            0 => num /= 2,
            _ => {
                num *= 3;
                num += 1;
            }
        }
    }
    println!("{num}");
}

fn print_max_sequence() {
    let mut num = 1;
    let mut max_length = 0;
    loop {
        if sequence_length(num) > max_length {
            max_length = sequence_length(num);
            println!("{num}: length {max_length}");
        }
        num += 1;
    }
    println!("{num}");
}

fn sequence_length(mut num: usize) -> usize {
    let mut length = 1;
    while num != 1 {
        match num % 2 {
            0 => num /= 2,
            _ => {
                num *= 3;
                num += 1;
            }
        }
        length += 1;
    }
    return length;
}
