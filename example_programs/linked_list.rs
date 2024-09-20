fn main() {
    let mut list = new_list();
    let list = &mut list;

    push(list, 4);
    push(list, 1);
    push(list, 8);
    push(list, 7);
    pop(list);
    let one = get(list, 1);
    push(list, one);
    push(list, 5);
    push(list, 6);
    pop(list);
    push(list, 9);
    set(list, 2, 3);
    swap(get_mut(list, 0), get_mut(list, 2));

    print!("List: ");
    print_list(list);

    let mut sum = 0;
    let mut iter = iter_mut(list);
    let mut item: &mut usize;
    while next_mut(&mut iter, &mut item) {
        sum += *item;
        *item = sum;
    }

    print!("Cumulative sum: ");
    print_list(list);
}

fn swap(a: &mut usize, b: &mut usize) {
    let temp = *a;
    *a = *b;
    *b = temp;
}

struct Node {
    item: usize,
    next: &mut Node,
}

struct LinkedList {
    head: &mut Node,
}

fn new_list() -> LinkedList {
    return LinkedList {
        head: 0 as &mut Node,
    };
}

fn push(list: &mut LinkedList, item: usize) {
    let mut next = &mut list.head;
    while *next as usize != 0 {
        next = &mut (*next).next;
    }
    *next = boxed!(Node {
        item,
        next: 0 as &mut Node
    });
}

fn pop(list: &mut LinkedList) -> usize {
    let mut next = &mut list.head;
    if *next as usize == 0 {
        panic!("cannot pop from empty list");
    }
    while (*next).next as usize != 0 {
        next = &mut (*next).next;
    }
    let item = (*next).item;
    *next = 0 as &mut Node;
    return item;
}

fn len(list: &LinkedList) -> usize {
    let mut count = 0;
    let mut next = &list.head;
    while *next != 0 as &mut Node {
        count += 1;
        next = &(*next).next;
    }
    return count;
}

fn get_mut(list: &mut LinkedList, index: usize) -> &mut usize {
    let mut next = &mut list.head;
    let mut i = 0;
    loop {
        if *next == 0 as &mut Node {
            panic!("index out of bounds");
        }
        if i == index {
            break;
        }
        next = &mut (*next).next;
        i += 1;
    }
    return &mut (*next).item;
}

fn get(list: &LinkedList, index: usize) -> usize {
    let mut next = &list.head;
    let mut i = 0;
    loop {
        if *next == 0 as &mut Node {
            panic!("index out of bounds");
        }
        if i == index {
            break;
        }
        next = &(*next).next;
        i += 1;
    }
    return (*next).item;
}

fn set(list: &mut LinkedList, index: usize, item: usize) {
    *get_mut(list, index) = item;
}

struct ListIter {
    node: &Node,
}

fn iter(list: &LinkedList) -> ListIter {
    return ListIter { node: list.head };
}

fn next(list_iter: &mut ListIter, item: &mut usize) -> bool {
    if list_iter.node as usize == 0 {
        return false;
    }
    *item = list_iter.node.item;
    list_iter.node = list_iter.node.next;
    return true;
}

fn print_list(list: &LinkedList) {
    let mut iter = iter(list);
    let mut item: usize;
    print!("[");
    if next(&mut iter, &mut item) {
        print!("%d", item);
    }
    while next(&mut iter, &mut item) {
        print!(", %d", item);
    }
    println!("]");
}

struct ListIterMut {
    node: &mut Node,
}

fn iter_mut(list: &mut LinkedList) -> ListIterMut {
    return ListIterMut { node: list.head };
}

fn next_mut(list_iter: &mut ListIterMut, item: &mut &mut usize) -> bool {
    if list_iter.node as usize == 0 {
        return false;
    }
    *item = &mut list_iter.node.item;
    list_iter.node = list_iter.node.next;
    return true;
}
