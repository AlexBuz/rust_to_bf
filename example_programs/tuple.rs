fn main() {
    let why_was_6_afraid_of_7 = (7, 8, 9);
    let mut backwards = why_was_6_afraid_of_7;
    backwards.0 = why_was_6_afraid_of_7.2;
    backwards.2 = why_was_6_afraid_of_7.0;
    print_triple(why_was_6_afraid_of_7);
    print_triple(backwards);
    let eight_nine_ten = (backwards.1, backwards.0, 1 + why_was_6_afraid_of_7.2);
    print_triple(eight_nine_ten);
    let triple_triple = (why_was_6_afraid_of_7, backwards, eight_nine_ten);
    println!();
    print_triple_triple(triple_triple);
}

fn print_triple_triple(
    triple_triple: (
        (usize, usize, usize),
        (usize, usize, usize),
        (usize, usize, usize),
    ),
) {
    print_triple(triple_triple.0);
    print_triple(triple_triple.1);
    print_triple(triple_triple.2);
}

fn print_triple(triple: (usize, usize, usize)) {
    println!(
        "First: %d, Second: %d, Third: %d",
        triple.0, triple.1, triple.2
    );
}
