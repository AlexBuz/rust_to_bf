fn main() {
    let mut arr: [usize; 12] = [42, 43, 44, 200, 46, 47, 48, 49, 50, 51, 52, 53];
    {
        // this is a copy so the original array is not modified
        let mut arr = *&arr;

        let third = &mut arr[2];
        *third = 100;
    }
    {
        let fourth = &mut arr[3];
        *fourth = 45;
    }
    let mut i = 0;
    while i < 12 {
        println!("x[%d] = %d", i, arr[ret_and_inc(&mut i)]);
    }
}

fn ret_and_inc(x: &mut usize) -> usize {
    let ret = *x;
    *x += 1;
    return ret;
}
