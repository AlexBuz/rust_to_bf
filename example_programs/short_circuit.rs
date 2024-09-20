fn main() {
    let a = false;
    let b = true;
    a || should_run(1);
    b || should_not_run();
    a && should_not_run();
    b && should_run(2);
    let mut c = false;
    c = (c && should_not_run() || should_run(3)) && should_run(4);
}

fn should_run(num: usize) -> bool {
    println!("%d. This should run. All good.", num);
    return true;
}

fn should_not_run() -> bool {
    panic!("This should NOT run.");
}
