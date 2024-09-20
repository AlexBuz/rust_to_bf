fn main() {
    print!("Enter 4 characters to echo: ");
    let x = read_char!();
    let y = read_char!();
    let z = read_char!();
    let w = read_char!();
    let mut t = print_char!(x);
    t = print_char!(y);
    t = print_char!(z);
    t = print_char!(w);
}
