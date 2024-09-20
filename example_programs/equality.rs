fn main() {
    println!("Testing == operator...");
    if () == () {
        println!("() == ()");
    } else {
        println!("() != ()");
    }
    test_tup_eq((0, 0), (0, 0));
    test_tup_eq((5, 6), (5, 6));
    test_tup_eq((5, 6), (5, 7));
    test_tup_eq((5, 6), (4, 6));
    test_tup_eq((5, 6), (7, 4));
    test_tup_eq((5, 6), (0, 0));

    println!("\nTesting != operator...");
    if () != () {
        println!("() != ()");
    } else {
        println!("() == ()");
    }
    test_tup_ne((0, 0), (0, 0));
    test_tup_ne((5, 6), (5, 6));
    test_tup_ne((5, 6), (5, 7));
    test_tup_ne((5, 6), (4, 6));
    test_tup_ne((5, 6), (7, 4));
    test_tup_ne((5, 6), (0, 0));
}

fn test_tup_eq(a: (usize, usize), b: (usize, usize)) {
    if a == b {
        println!("(%d, %d) == (%d, %d)", a.0, a.1, b.0, b.1);
    } else {
        println!("(%d, %d) != (%d, %d)", a.0, a.1, b.0, b.1);
    }
}

fn test_tup_ne(a: (usize, usize), b: (usize, usize)) {
    if a != b {
        println!("(%d, %d) != (%d, %d)", a.0, a.1, b.0, b.1);
    } else {
        println!("(%d, %d) == (%d, %d)", a.0, a.1, b.0, b.1);
    }
}
