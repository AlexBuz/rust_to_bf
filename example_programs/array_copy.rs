fn main() {
    let arr = [37, 38, 39, 40, 41, 42, 43, 44, 45, 46];
    let arr = *&arr;
    println!("life = %d", arr[5]);
    let arr = Array {
        arr: &arr as &[usize],
    };
    let arr = &arr;
    println!("life = %d", arr.arr[5]);
}

struct Array {
    arr: &[usize],
}
