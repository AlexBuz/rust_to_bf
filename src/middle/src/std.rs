fn add(mut a: usize, b: usize) -> usize {
    a += b;
    return a;
}

fn sub(mut a: usize, b: usize) -> usize {
    a -= b;
    return a;
}

fn mul(mut a: usize, b: usize) -> usize {
    let mut result = 0;
    loop {
        match a {
            0 => return result,
            _ => {
                a -= 1;
                result += b;
            }
        }
    }
}

fn div(mut dividend: usize, divisor: usize) -> usize {
    match divisor {
        0 => panic!("attempt to divide by zero"),
        _ => {
            let mut remainder = divisor;
            let mut quotient = 0;
            loop {
                match dividend {
                    0 => {
                        match remainder {
                            0 => quotient += 1,
                            _ => {}
                        }
                        return quotient;
                    }
                    _ => {
                        dividend -= 1;
                        match remainder {
                            0 => {
                                remainder = divisor;
                                quotient += 1;
                            }
                            _ => {}
                        }
                        remainder -= 1;
                    }
                }
            }
        }
    }
}

fn rem(mut dividend: usize, divisor: usize) -> usize {
    match divisor {
        0 => panic!("attempt to calculate the remainder with a divisor of zero"),
        _ => {
            let mut remainder = divisor;
            loop {
                match dividend {
                    0 => {
                        match remainder {
                            0 => {}
                            _ => {
                                let temp = remainder;
                                remainder = divisor;
                                remainder -= temp;
                            }
                        }
                        return remainder;
                    }
                    _ => {
                        dividend -= 1;
                        match remainder {
                            0 => remainder = divisor,
                            _ => {}
                        }
                        remainder -= 1;
                    }
                }
            }
        }
    }
}

fn lt(mut a: usize, mut b: usize) -> bool {
    loop {
        match b {
            0 => return false,
            _ => {
                b -= 1;
                match a {
                    0 => return true,
                    _ => a -= 1,
                }
            }
        }
    }
}

fn le(mut a: usize, mut b: usize) -> bool {
    loop {
        match a {
            0 => return true,
            _ => {
                a -= 1;
                match b {
                    0 => return false,
                    _ => b -= 1,
                }
            }
        }
    }
}

fn gt(mut a: usize, mut b: usize) -> bool {
    loop {
        match a {
            0 => return false,
            _ => {
                a -= 1;
                match b {
                    0 => return true,
                    _ => b -= 1,
                }
            }
        }
    }
}

fn ge(mut a: usize, mut b: usize) -> bool {
    loop {
        match b {
            0 => return true,
            _ => {
                b -= 1;
                match a {
                    0 => return false,
                    _ => a -= 1,
                }
            }
        }
    }
}

fn not(a: bool) -> bool {
    if a {
        return false;
    } else {
        return true;
    }
}

fn print_int(mut num: usize) {
    let divisor = 10;
    let mut remainder = divisor;
    let mut quotient = 0;
    loop {
        match num {
            0 => {
                match remainder {
                    0 => quotient += 1,
                    _ => {
                        let temp = remainder;
                        remainder = divisor;
                        remainder -= temp;
                    }
                }
                match quotient {
                    0 => {}
                    _ => print_int(quotient),
                }
                remainder += '0' as usize;
                print_char!(remainder as char);
                return;
            }
            _ => {
                num -= 1;
                match remainder {
                    0 => {
                        remainder = divisor;
                        quotient += 1;
                    }
                    _ => {}
                }
                remainder -= 1;
            }
        }
    }
}

fn read_int() -> usize {
    let mut num = 0;
    loop {
        let mut c = read_char!() as usize;
        if c < '0' as usize {
            return num;
        }
        c -= '0' as usize;
        if c > 9 {
            return num;
        }
        num *= 10;
        num += c;
    }
}
