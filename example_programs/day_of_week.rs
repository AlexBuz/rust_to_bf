fn main() {
    let date = read_date();
    println!(
        "%s %d, %d is a %s.",
        get_month_str(date.month),
        date.day,
        100 * date.century + date.year,
        get_day_of_week(date)
    );
}

struct Date {
    century: usize,
    year: usize,
    month: usize,
    day: usize,
}

fn read_date() -> Date {
    print!("Enter a date in YYYY-MM-DD format: ");

    let mut century = read_digit();
    century *= 10;
    century += read_digit();

    let mut year = read_digit();
    year *= 10;
    year += read_digit();

    expect_hyphen();

    let mut month = read_digit();
    month *= 10;
    month += read_digit();

    expect_hyphen();

    let mut day = read_digit();
    day *= 10;
    day += read_digit();

    if day < 1 || day > 31 {
        println!("Invalid day: %d", day);
        exit!();
    }

    return Date {
        century,
        year,
        month,
        day,
    };
}

fn get_month_str(month: usize) -> &str {
    match month {
        1 => return "January",
        2 => return "February",
        3 => return "March",
        4 => return "April",
        5 => return "May",
        6 => return "June",
        7 => return "July",
        8 => return "August",
        9 => return "September",
        10 => return "October",
        11 => return "November",
        12 => return "December",
        _ => {
            println!("Invalid month: %d", month);
            exit!();
        }
    }
}

fn get_century_offset(century: usize) -> usize {
    match century % 4 {
        0 => return 0,
        1 => return 5,
        2 => return 3,
        3 => return 1,
    }
}

fn get_year_offset(mut year: usize) -> usize {
    year += year / 4;
    return year;
}

fn is_leap_year(century: usize, year: usize) -> bool {
    return year % 4 == 0 && (year != 0 || century % 4 == 0);
}

fn get_month_offset(century: usize, year: usize, month: usize) -> usize {
    match month {
        1 => return 6 - is_leap_year(century, year) as usize,
        2 => return 2 - is_leap_year(century, year) as usize,
        3 => return 2,
        4 => return 5,
        5 => return 0,
        6 => return 3,
        7 => return 5,
        8 => return 1,
        9 => return 4,
        10 => return 6,
        11 => return 2,
        12 => return 4,
        _ => {
            println!("Invalid month: %d", month);
            exit!();
        }
    }
}

fn get_day_of_week(date: Date) -> &str {
    let mut total = get_century_offset(date.century);
    total += get_year_offset(date.year);
    total += get_month_offset(date.century, date.year, date.month);
    total += date.day;
    total %= 7;
    match total {
        0 => return "Sunday",
        1 => return "Monday",
        2 => return "Tuesday",
        3 => return "Wednesday",
        4 => return "Thursday",
        5 => return "Friday",
        6 => return "Saturday",
    }
}

fn read_digit() -> usize {
    let c = read_char!() as usize;
    if c < 48 || c > 57 {
        println!("Invalid input: expected a digit, but got '%c'", c as char);
        exit!();
    }
    return c - 48;
}

fn expect_hyphen() {
    let c = read_char!();
    if c != '-' {
        println!("Invalid input: expected a hyphen, but got '%c'", c);
        exit!();
    }
}
