fn main() {
    let mut i = 34;
    let x = [
        inc_and_ret(&mut i),
        inc_and_ret(&mut i),
        inc_and_ret(&mut i),
        inc_and_ret(&mut i),
        inc_and_ret(&mut i),
        inc_and_ret(&mut i),
        inc_and_ret(&mut i),
        inc_and_ret(&mut i),
    ];
    println!("34 + 8 = %d", i);
    let y = [inc_and_ret(&mut i); 8];
    println!("42 + 1 = %d", i);
}

fn inc_and_ret(x: &mut usize) -> usize {
    *x += 1;
    return *x;
}
