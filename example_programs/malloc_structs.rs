fn main() {
    let len = 10;
    let struct_size = size_of_val!(NumAndSquare { num: 0, square: 0 });
    let evens = malloc!(len * struct_size) as &mut [NumAndSquare];
    let odds = malloc!(len * struct_size) as &mut [NumAndSquare];
    let mut i = 0;
    while i < len {
        evens[i] = num_and_square_for(i * 2);
        odds[i] = num_and_square_for(i * 2 + 1);
        i += 1;
    }
    i = 0;
    while i < len {
        println!("{}^2 = {}", evens[i].num, evens[i].square);
        println!("{}^2 = {}", odds[i].num, odds[i].square);
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
