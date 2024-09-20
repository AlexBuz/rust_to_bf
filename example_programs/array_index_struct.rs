fn main() {
    let people = [
        Person {
            name: "Alice",
            age: 25,
        },
        Person {
            name: "Bob",
            age: 30,
        },
        Person {
            name: "Charlie",
            age: 35,
        },
        Person {
            name: "David",
            age: 40,
        },
        Person {
            name: "Eve",
            age: 45,
        },
        Person {
            name: "Frank",
            age: 50,
        },
        Person {
            name: "Grace",
            age: 55,
        },
        Person {
            name: "Heidi",
            age: 60,
        },
        Person {
            name: "Ivan",
            age: 65,
        },
        Person {
            name: "Judy",
            age: 70,
        },
        Person {
            name: "Kevin",
            age: 75,
        },
    ];
    let mut sum = 0;
    let mut len = 11;
    let mut i = 0;
    while i < len {
        println!(
            "%d. %s is %d years old.",
            i + 1,
            people[i].name,
            people[i].age
        );
        sum += people[i].age;
        i += 1;
    }
    let avg = sum / len;
    println!("The average age is %d.", avg);
}

struct Person {
    name: &str,
    age: usize,
}
