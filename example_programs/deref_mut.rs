fn main() {
    let mut p = Person {
        name: Name {
            first: "John",
            last: "Smith",
        },
        age: 30,
    };

    let name_mut = &mut p.name;
    name_mut.first = "Jane";

    let first_mut = &mut name_mut.first;
    *first_mut = "Jill";

    let name_mut = NameMut { name: &mut p.name };
    name_mut.name.first = "Jeff";

    // should not compile (cannot assign value of type `&mut Name` to immutable place):
    // name_mut.name = &mut p.name;

    let name_mut_ref = &name_mut;

    // should not compile (cannot take mutable reference to immutable place of type `NameMut`):
    // let name_mut_ref = &mut name_mut;
    // name_mut_ref.name = &mut p.name;

    println!(
        "%s %s is %d years old.",
        name_mut_ref.name.first, p.name.last, p.age
    );
}

struct NameMut {
    name: &mut Name,
}

struct Name {
    first: &str,
    last: &str,
}

struct Person {
    name: Name,
    age: usize,
}
