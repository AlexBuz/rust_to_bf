fn main() {
    let size = 5;
    let arr1: &mut [usize] = malloc!(size);
    let arr2: &mut [usize] = malloc!(size);
    let mut i = 0;
    while i < size {
        arr1[i] = i * 2;
        arr2[i] = i * 2 + 1;
        i += 1;
    }
    i = 0;
    while i < size {
        print!("%d %d ", arr1[i], arr2[i]);
        i += 1;
    }
    println!();
}
