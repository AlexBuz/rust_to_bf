fn main() {
    print!("Enter a string to reverse: ");
    reverse_line();
    println!();
}

fn reverse_line() {
    let c = read_char!();
    match c {
        '\n' => return,
        _ => {
            reverse_line();
            print_char!(c);
        }
    }
}
