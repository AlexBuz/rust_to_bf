fn main() {
    let x = 12;
    let mut x = 41;
    {
        x += 2;
        let mut x = x - 1;
        match x {
            42 => println!("1. All good. Shadowing works."),
            _ => panic!("Shadowing is broken."),
        }
    }
    match x {
        43 => println!("2. All good. Shadowing works."),
        _ => panic!("Shadowing is broken."),
    }
}
