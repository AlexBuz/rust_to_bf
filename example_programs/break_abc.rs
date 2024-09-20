fn main() {
    loop {
        if not_true() {
            print_char!('F');
        } else {
            print_char!('B');
            break;
        }
    }
    print_char!('C', '\n');
}

fn not_true() -> bool {
    print_char!('A');
    return false;
}
