fn main() {
    let arr = [37, 38, 39, 40, 41, 42, 43, 44, 45, 46];

    let foo = (3, (4, 5), 6);
    let foo = &foo;

    println!("life = %d", arr[foo.1 .1]);
    return;
}
