fn main() {
    let old = Person {
        name: Name {
            first: "Bob",
            last: "D",
        },
        age: 30,
        address: 123,
    };
    let mut new = old;
    new.name = Name {
        first: "John",
        last: "Smith",
    };
    new.name.last = "S";
    new.address = 456;
    new.age = 40;
    print_person(old);
    print_person(new);
    let person = random_person("Nancy");
    print_person(person);
}

fn print_person(person: Person) {
    println!(
        "{} {} is {} years old and lives at {}.",
        person.name.first, person.name.last, person.age, person.address
    );
}

struct Name {
    first: &str,
    last: &str,
}

struct Person {
    name: Name,
    age: usize,
    address: usize,
}

fn random_name() -> Name {
    return Name {
        first: "Alice",
        last: "C",
    };
}

fn random_person(replace_first: &str) -> Person {
    let mut name = random_name();
    name.first = replace_first;
    return Person {
        name,
        age: 50,
        address: 789,
    };
}
