fn main() {
    let x = 42;
    println!("size of usize: %d", size_of_val!(x));

    let x = ();
    println!("size of (): %d", size_of_val!(x));

    let x = true;
    println!("size of bool: %d", size_of_val!(x));

    let x = 'a';
    println!("size of char: %d", size_of_val!(x));

    let x = (42, true, 'a');
    println!("size of (usize, bool, char): %d", size_of_val!(x));

    let x = [7; 10];
    println!("size of [usize; 10]: %d", size_of_val!(x));

    let x = [(); 10];
    println!("size of [(); 10]: %d", size_of_val!(x));

    let x = [(true, 'a'); 6];
    println!("size of [(bool, char); 6]: %d", size_of_val!(x));

    println!("struct Bar {");
    println!("    a: usize,");
    println!("    b: [usize; 3],");
    println!("}");
    println!("struct Foo {");
    println!("    a: [Bar; 2],");
    println!("    b: bool,");
    println!("    c: [(); 42],");
    println!("}");

    let x = Bar {
        a: 42,
        b: [7, 8, 9],
    };
    let x = Foo {
        a: [x; 2],
        b: true,
        c: [(); 42],
    };
    println!("size of Foo: %d", size_of_val!(x));
    println!("size of &Foo: %d", size_of_val!(&x));
}

struct Bar {
    a: usize,
    b: [usize; 3],
}

struct Foo {
    a: [Bar; 2],
    b: bool,
    c: [(); 42],
}
