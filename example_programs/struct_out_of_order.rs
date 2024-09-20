fn main() {
    let person = Person {
        address: inspect_int(123),
        name: Name {
            last: inspect_string("Smith"),
            first: inspect_string("John"),
        },
        age: inspect_int(40),
    };
    println!(
        "%s %s is %d years old and lives at %d.",
        person.name.first, person.name.last, person.age, person.address
    );
}

fn inspect_string(s: &str) -> &str {
    println!("String: %s", s);
    return s;
}

fn inspect_int(i: usize) -> usize {
    println!("Int: %d", i);
    return i;
}

struct Person {
    name: Name,
    age: usize,
    address: usize,
}

struct Name {
    first: &str,
    last: &str,
}
