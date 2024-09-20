fn main() {
    let mut nums0 = ((4, 9), 2);
    let mut nums1 = ((7, 5), 8);

    swap_tuples(&mut nums0, &mut nums1);
    let a = &mut nums0.0 .0;
    let b = &mut nums0.1;
    swap(&mut *a, &mut *b);

    swap_tuples(&mut nums0, &mut nums1);
    swap_tuple(&mut nums0.0);

    let mut p = &mut nums0.0 .0;
    let mut p = &mut nums0.0 .0;
    let mut p = &mut nums0.0 .0;
    let mut p = &mut nums0.0 .0;
    let mut p = &mut nums0.0 .0;
    *p = 3;
    let mut pp = &mut p;
    let mut pp = &mut p;
    let mut pp = &mut p;
    let mut pp = &mut p;
    let mut pp = &mut p;
    **pp = 2;
    let mut ppp = &mut pp;
    let mut ppp = &mut pp;
    let mut ppp = &mut pp;
    let mut ppp = &mut pp;
    let mut ppp = &mut pp;
    ***id(id(*&mut ppp)) = 1;

    println!(
        "1/7 ≈ 0.%d%d%d%d%d%d",
        nums0.0 .0, nums0.0 .1, nums0.1, nums1.0 .0, nums1.0 .1, nums1.1
    );
}

fn id(a: &mut (&mut &mut usize)) -> &mut &mut &mut usize {
    return a;
}

fn swap(a: &mut usize, b: &mut usize) {
    let temp = *a;
    *a = *b;
    *b = temp;
}

fn swap_tuples(nums: &mut ((usize, usize), usize), nums1: &mut ((usize, usize), usize)) {
    let temp = *nums;
    *nums = *nums1;
    *nums1 = temp;
}

fn swap_tuple(mut nums: &mut (usize, usize)) {
    // Method 1:
    swap(&mut nums.0, &mut nums.1);
    // ... desugars to ...
    swap(&mut (*nums).0, &mut (*nums).1);

    // Method 2:
    let temp0 = nums.0;
    nums.0 = nums.1;
    nums.1 = temp0;
    // ... desugars to ...
    let temp = (*nums).0;
    (*nums).0 = (*nums).1;
    (*nums).1 = temp;

    // Method 3:
    let mut temp = *nums;
    swap(&mut temp.0, &mut temp.1);
    *nums = temp;
}
