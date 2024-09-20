fn main() {
    loop {
        let a = 'F';
        if a != 'F' {
            print_char!(a);
        } else {
            break;
        }
        print_char!(a);
    }
    loop {
        let b = 'F';
        if b != '\0' {
            break;
        } else {
            print_char!(b);
        }
        print_char!(b);
    }
    loop {
        let a = 0;
        let b = 0;
        let c = 'F';
        let d = 0;
        match d {
            0 => {
                let x = a;
                return;
            }
            1 => {
                let x = b;
                break;
            }
            2 => {
                let x = c;
            }
            _ => {
                print_char!(c);
            }
        }
        break;
    }
}
