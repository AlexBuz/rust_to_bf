fn main() {
    let len = 10;
    let struct_size = 2;
    let malloc_size = len * struct_size;
    let evens: &mut [NumAndSquare] = malloc!(malloc_size) as &mut [NumAndSquare];
    let odds: &mut [NumAndSquare] = malloc!(malloc_size) as &mut [NumAndSquare];
    let mut i = 0;
    while i < len {
        evens[i] = num_and_square_for(i * 2);
        odds[i] = num_and_square_for(i * 2 + 1);
        i += 1;
    }
    i = 0;
    while i < len {
        println!("%d^2 = %d", evens[i].num, evens[i].square);
        println!("%d^2 = %d", odds[i].num, odds[i].square);
        i += 1;
    }
}

fn num_and_square_for(num: usize) -> NumAndSquare {
    return NumAndSquare {
        num,
        square: num * num,
    };
}

struct NumAndSquare {
    num: usize,
    square: usize,
}
