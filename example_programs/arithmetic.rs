fn main() {
    let max = 12;

    let mut a: usize;
    let mut b: usize;

    println!("Testing addition...");
    a = 0;
    while a <= max {
        b = 1;
        while b <= max {
            println!("{a} + {b} = {}", a + b);
            b += 1;
        }
        a += 1;
    }

    println!("\nTesting subtraction...");
    a = 0;
    while a <= max {
        b = 0;
        while b <= a {
            println!("{a} - {b} = {}", a - b);
            b += 1;
        }
        a += 1;
    }

    println!("\nTesting multiplication...");
    a = 0;
    while a <= max {
        b = 1;
        while b <= max {
            println!("{a} * {b} = {}", a * b);
            b += 1;
        }
        a += 1;
    }

    println!("\nTesting division...");
    a = 0;
    while a <= max {
        b = 1;
        while b <= max {
            println!("{a} / {b} = {}", a / b);
            b += 1;
        }
        a += 1;
    }

    println!("\nTesting modulus...");
    a = 0;
    while a <= max {
        b = 1;
        while b <= max {
            println!("{a} % {b} = {}", a % b);
            b += 1;
        }
        a += 1;
    }

    println!("\nTesting division by zero...");
    b = 0;
    // should panic
    println!("{a} / {b} = {}", a / b);

    // unreachable
    println!("\nTesting modulus by zero...");
    println!("{a} % {b} = {}", a % b);
}
