fn main() {
    let code = malloc!(0);
    let mut code_len = 0;
    loop {
        let mut c = 0;
        match read_char!() {
            '\0' => break,
            '!' => break,
            '>' => c = 1,
            '<' => c = 2,
            '+' => c = 3,
            '-' => c = 4,
            '[' => c = 5,
            ']' => c = 6,
            '.' => c = 7,
            ',' => c = 8,
            _ => continue,
        }
        code[code_len] = c;
        code_len += 1;
    }
    extend_last_malloc(code_len);
    let mut input = new_list();
    loop {
        let c = read_char!() as usize;
        push(&mut input, c);
        if c == 0 {
            break;
        }
    }
    let mut tape = malloc!(0);
    let mut state = ExecutionState {
        code,
        code_ptr: 0,
        tape,
        tape_ptr: 0,
        input: iter(&input),
    };
    execute_bf(&mut state);
}

fn extend_last_malloc(amount: usize) {
    // the address of the next free heap cell is stored in the first heap cell
    let next_free_address = 0 as &mut usize;
    // addresses alternate between heap and stack so we need to multiply by 2
    *next_free_address += 2 * amount;
}

struct ExecutionState {
    code: &[usize],
    code_ptr: usize,
    tape: &mut [usize],
    tape_ptr: usize,
    input: ListIter,
}

fn execute_bf(state: &mut ExecutionState) {
    loop {
        match state.code[state.code_ptr] {
            0 => break,
            1 => state.tape_ptr += 1,
            2 => state.tape_ptr -= 1,
            3 => state.tape[state.tape_ptr] += 1,
            4 => state.tape[state.tape_ptr] -= 1,
            5 => {
                if state.tape[state.tape_ptr] == 0 {
                    let mut depth = 1;
                    while depth != 0 {
                        state.code_ptr += 1;
                        match state.code[state.code_ptr] {
                            5 => depth += 1,
                            6 => depth -= 1,
                            _ => {}
                        }
                    }
                }
            }
            6 => {
                if state.tape[state.tape_ptr] != 0 {
                    let mut depth = 1;
                    while depth != 0 {
                        state.code_ptr -= 1;
                        match state.code[state.code_ptr] {
                            5 => depth -= 1,
                            6 => depth += 1,
                            _ => {}
                        }
                    }
                }
            }
            7 => print!("{}", state.tape[state.tape_ptr] as char),
            8 => {
                let mut item = 0;
                if next(&mut state.input, &mut item) {
                    state.tape[state.tape_ptr] = item;
                }
            }
            _ => {}
        }
        state.code_ptr += 1;
    }
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
