fn main() {
    let greeting = "Hello, world!";
    println!("{greeting} 👋");
    let age = 21;
    english(age);
    greek(age);
    chinese(age);
}

fn english(age: usize) {
    let i = 'I';
    println!(
        "{i} am {age} years old, and next year {i} will be {}.",
        age + 1
    );
}

fn greek(age: usize) {
    println!("Είμαι {age} χρονών, και του χρόνου θα γίνω {}.", age + 1);
}

fn chinese(age: usize) {
    println!("我今年{age}岁，明年就{}岁了。", age + 1);
}
