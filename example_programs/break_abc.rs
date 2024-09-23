fn main() {
    loop {
        if not_true() {
            print!("F");
        } else {
            print!("B");
            break;
        }
    }
    println!("C");
}

fn not_true() -> bool {
    print!("A");
    return false;
}
