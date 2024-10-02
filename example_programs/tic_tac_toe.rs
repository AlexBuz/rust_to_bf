struct Board {
    cells: [char; 9],
    turn: char,
    occupied_count: usize,
}

fn print_board(board: &Board) {
    let cells = &board.cells;
    println!();
    println!(" {} | {} | {} ", cells[0], cells[1], cells[2]);
    println!("---+---+---");
    println!(" {} | {} | {} ", cells[3], cells[4], cells[5]);
    println!("---+---+---");
    println!(" {} | {} | {} ", cells[6], cells[7], cells[8]);
    println!();
}

fn is_3_in_a_row(board: &Board, loc0: usize, loc1: usize, loc2: usize, turn: char) -> bool {
    return board.cells[loc0] == turn && board.cells[loc1] == turn && board.cells[loc2] == turn;
}

fn is_win(board: &Board) -> bool {
    return is_3_in_a_row(board, 0, 1, 2, board.turn)
        || is_3_in_a_row(board, 3, 4, 5, board.turn)
        || is_3_in_a_row(board, 6, 7, 8, board.turn)
        || is_3_in_a_row(board, 0, 3, 6, board.turn)
        || is_3_in_a_row(board, 1, 4, 7, board.turn)
        || is_3_in_a_row(board, 2, 5, 8, board.turn)
        || is_3_in_a_row(board, 0, 4, 8, board.turn)
        || is_3_in_a_row(board, 2, 4, 6, board.turn);
}

fn cycle_turn(turn: &mut char) {
    match *turn {
        'X' => *turn = 'O',
        'O' => *turn = 'X',
    }
}

fn neg(num: usize) -> usize {
    return 22 - num;
}

fn negamax(board: &mut Board, depth: usize, mut alpha: usize, mut beta: usize) -> usize {
    if is_win(board) {
        return 1 + depth;
    }
    if board.occupied_count == 9 {
        return 11;
    }
    cycle_turn(&mut board.turn);

    let mut best_score = 0;
    let mut i = 0;
    while i < 9 {
        if board.cells[i] == ' ' {
            board.cells[i] = board.turn;

            board.occupied_count += 1;

            let score = neg(negamax(board, depth + 1, neg(beta), neg(alpha)));

            board.cells[i] = ' ';
            board.occupied_count -= 1;

            if score > best_score {
                best_score = score;
            }
            if score > alpha {
                alpha = score;
            }
            if alpha >= beta {
                break;
            }
        }
        i += 1;
    }
    cycle_turn(&mut board.turn);

    return best_score;
}

fn get_best_move(board: &mut Board) -> usize {
    let mut best_move = 0;
    let mut best_score = 0;
    let mut i = 0;
    while i < 9 {
        if board.cells[i] == ' ' {
            board.cells[i] = board.turn;

            board.occupied_count += 1;

            let score = neg(negamax(board, 0, 0, 22));

            board.cells[i] = ' ';
            board.occupied_count -= 1;

            if score > best_score {
                best_score = score;
                best_move = i;
            }
        }
        i += 1;
    }

    return best_move;
}

fn tic_tac_toe() {
    let mut board = Board {
        cells: [' '; 9],
        turn: 'X',
        occupied_count: 0,
    };

    println!("Welcome to Tic Tac Toe!");
    println!("Player X will go first.");

    let mut computer_player = '\0';
    loop {
        print!("Enter the letter of the computer player (X or O, or press enter if there are two human players): ");
        let input = read_char!();
        match input {
            '\n' => break,
            'X' => computer_player = 'X',
            'x' => computer_player = 'X',
            'O' => computer_player = 'O',
            'o' => computer_player = 'O',
            _ => {
                println!("Invalid player.");
                continue;
            }
        }
        while read_char!() != '\n' {}
        break;
    }

    print_board(&board);

    let mut move_count = 0;
    while move_count < 9 {
        loop {
            if board.turn == computer_player {
                print!("Computer's turn. Thinking...");
                let best_move = get_best_move(&mut board);
                println!(" Picked cell {}.", best_move + 1);
                board.cells[best_move] = board.turn;
                break;
            } else {
                print!(
                    "Enter the cell where player {} would like to make their move (1-9): ",
                    board.turn
                );

                let move_loc = read_int();
                if move_loc < 1 || move_loc > 9 {
                    println!("Invalid cell.");
                    continue;
                }

                let cell = &mut board.cells[(move_loc - 1)];
                match *cell {
                    ' ' => {
                        *cell = board.turn;
                        break;
                    }
                    _ => println!("Cell already occupied_count."),
                }
            }
        }
        board.occupied_count += 1;

        print_board(&board);

        if is_win(&board) {
            println!("Player {} wins!", board.turn);
            return;
        }

        cycle_turn(&mut board.turn);

        move_count += 1;
    }

    println!("It's a draw!");
}

fn main() {
    tic_tac_toe();
}
