# `rust_to_bf`

`rust_to_bf` is a compiler from a subset of [Rust](https://www.rust-lang.org/) to [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) (BF), with support for primitive types (`usize`, `char`, `bool`, `&str`), integer arithmetic (`+`, `-`, `*`, `/`, `%`), boolean logic (`&&`, `||`, `!`), compound types (arrays, tuples, structs), references (`&`, `&mut`), control flow statements (`if`, `match`, `loop`, `while`, `break`, `continue`), functions (including recursion), dynamic memory allocation, input/output, and [more](#supported-rust-features-non-exhaustive).

## Background

The target language of this compiler, BF, is an esoteric programming language known for its extreme minimalism and difficulty to write serious programs in. A program in BF consists of a sequence of single-character instructions that control the actions and movements of a data pointer along a one-dimensional array of zero-initialized integer cells:

| Instruction | Action                                                                         |
| ----------- | ------------------------------------------------------------------------------ |
| `>`         | Move the data pointer right by 1 cell.                                         |
| `<`         | Move the data pointer left by 1 cell.                                          |
| `+`         | Increment the value of the pointed-to cell by 1.                               |
| `-`         | Decrement the value of the pointed-to cell by 1.                               |
| `.`         | Output the value of the pointed-to cell.                                       |
| `,`         | Replace the value of the pointed-to cell with the next byte of input.          |
| `[`         | If the value of the pointed-to cell is zero, jump forward to the matching `]`. |
| `]`         | If the value of the pointed-to cell is nonzero, jump back to the matching `[`. |

Despite their simplicity, these eight instructions are sufficient to render the BF language [Turing-complete](https://en.wikipedia.org/wiki/Turing_completeness), meaning that it is capable of computing any computable function and thus is theoretically just as capable as any other programming language.

The goal of this project is to turn theory into practice by actually compiling a high-level language—with full support for features like indirect addressing, dynamic memory allocation, and recursion—into BF.

Originally, I planned to develop my own high-level language for this purpose. However, as I worked on the parser, it started to resemble a simplified version of Rust, so I decided to embrace this and pursue writing a Rust-to-BF compiler instead. As an exercise, though, I still finished writing the parser using the [`chumsky`](https://crates.io/crates/chumsky) parser combinator library rather than using a dedicated Rust parsing library like [`syn`](https://crates.io/crates/syn).

## Project Structure

The compiler itself is split into three library subcrates, corresponding to the three major stages of compilation:

1. In the `frontend` crate, the input Rust code is tokenized and parsed into an abstract syntax tree (AST).
2. In the `middle` crate, the AST is transformed into a custom intermediate representation (IR) consisting of load/store, control flow, input/output, and stack frame management statements. This crate also includes an interpreter for running the IR.
3. In the `backend` crate, the IR is translated into BF code. This crate also includes a BF interpreter for running the generated code.

The `rust_to_bf` binary crate wraps these three components together and provides a command-line interface (CLI) for compiling Rust programs to BF as well as for running them using either the IR or BF interpreter.

## Usage

To compile a Rust program to BF, use the `compile` subcommand, specifying the input Rust file and the output BF file (or omit the output file to print the BF code to stdout):

```sh
cargo run --release -- compile <input.rs> [-o <output.bf>]
```

To run a Rust program without outputting BF code, use the `run` subcommand and optionally specify which interpreter to use (either `ir` or `bf`, defaulting to `ir` for greater speed):

```sh
cargo run --release -- run <input.rs> [--interpreter=ir|bf]
```

## Example Programs

The `example_programs` directory contains several example programs that can be compiled and run by `rust_to_bf`. These include:

- `recursive_fibonacci.rs`:
  - Computes Fibonacci numbers recursively.
- `day_of_week.rs`:
  - Determines the day of the week for any given date.
- `prime_printer.rs`:
  - Prints as many prime numbers as the user desires.
- `collatz.rs`:
  - Prints the [Collatz sequence](https://en.wikipedia.org/wiki/Collatz_conjecture) starting from a user-specified number.
- `linked_list.rs`:
  - Demonstrates how a linked list can be implemented using the `boxed!` macro for heap allocation (see the [Standard Library](#standard-library) section for more information).
- `bf_interpreter.rs`:
  - An interpreter for arbitrary BF programs. Takes a BF program as input, optionally followed by an exclamation mark (`!`) and the input to the program, and outputs the result of running the program with the given input.
- `tic_tac_toe.rs`:
  - A text-based tic-tac-toe game with an unbeatable computer opponent as well as a 2-player mode.

## Caveats

Although `rust_to_bf` supports many Rust features, it lacks support for many others, including enums, pattern matching, methods, generics, modulues, traits, and lifetimes (see the [Supported Rust Features](#supported-rust-features-non-exhaustive) section for a more detailed list). It also lacks support for most of the Rust standard library. However, it does come with a small standard library of its own, which includes macros and functions for performing I/O, allocating heap memory, and terminating the program prematurely. See the [Standard Library](#standard-library) section for more information.

Additionally, the generated BF code is not optimized for efficiency, nor does the provided BF interpreter perform optimizations. This means that complex Rust programs may compile to BF code that is slow to run. Note, though, that the primary goal of this project is to demonstrate that it is _possible_ to compile Rust to BF, not necessarily do generate efficient BF code in the process. With that being said, optimizations are certainly an avenue for future work, and given the systematic nature of the IR-to-BF translation, it is conceivable that the BF interpreter could be made to reverse the translation process and run the corresponding IR code instead.

## Standard Library

### Input

- `read_char!() -> char`:
  - Reads a character of input from the user.
- `read_int() -> usize`:
  - Reads an integer from the user.
  - Note that this is a function, not a macro, so it must be called without the `!`.

### Output

- `print!(...)`:
  - Takes a format string literal and a variable number of arguments and prints the string with the arguments interpolated in place of the `{}` placeholders.
  - Arguments must be of primitive type (`usize`, `char`, `bool`, `&str`).
- `println!(...)`:
  - Same as `print!(...)`, but appends a newline to the output.
  - If no arguments are provided, it simply prints a newline.

### Memory Management

- `boxed!(value: T) -> &mut T`:
  - Stores a value of type `T` on the heap and returns a mutable reference to it.
- `malloc!(n: usize) -> &mut [usize]`:
  - Allocates a contiguous block of `n` cells on the heap and returns a mutable reference to it.
  - The cells are guaranteed to be zero-initialized.
- `size_of_val!(value: T) -> usize`:
  - Returns the size in cells of any value.

### Early Termination

- `exit!()`:
  - Terminates the program.
- `panic!(...)`:
  - Prints an error message and terminates the program.
  - Equivalent to `println!(...)` followed by `exit!()`.

## Implementation Details

### Frontend

This component is responsible for tokenizing and parsing the Rust source code and producing an AST. This is accomplished by composing a series of parser combinators provided by the `chumsky` library. Specifically, an alpha version of `chumsky` 1.0.0 is used which supports zero-copy parsing. This avoids the need to clone strings when constructing the AST.

### Middle

This component is responsible typechecking the AST and transforming it into IR code. The IR statements operate on a abstract machine with a stack that is addressable either absolutely or relative to a movable frame base. There are statements for raising and lowering the frame base, adding/subtracting values between stack cells and a temporary register, input/output, repeatedly executing a block of statements while a given stack cell is nonzero, and conditionally executing one of multiple blocks depending on the value of a given stack cell. To facilitate dynamic memory allocation, the IR reserves every other stack cell for heap allocations, with the first such cell storing the address of the next available heap cell.

To convert Rust control flow constructs into IR statements, a vector of basic blocks (straight-line sequences of statements with a single entry and exit point) is built as the AST is traversed. Then, at the top level of the IR, an all-encompassing `Switch` statement is used to dispatch control to the appropriate basic block based on the value at the base of the current stack frame, which is initially set to the entry block ID of the `main` function.

To call a function, the frame base is raised to preserve the local variables of the current function, make space for the return value, and store the return block ID. Then, the value at the bottom of the now-raised frame is set to target function's entry block ID. Once the target function begins executing, it lowers the frame base to give itself access the return value slot. To return, it sets the return value and raises the frame base just enough to preserve that value, leaving the return block ID at the bottom of the now-raised frame. The return block in the calling function then lowers the frame base to access the return value as well as the rest of its local variables, and execution continues from there.

A similar process is used to jump to blocks within the same function, allowing for loops and conditional statements to be translated to IR code. In these cases, less movement of the frame base is required. In particular, the entry block ID of the current function is simply overwritten with the target block ID and the frame base is raised just enough to preserve the return value slot and return block ID. The target block then lowers the frame base to access the return value slot and continues execution from there.

Here is a diagram illustrating the movements of the frame base as described above:

```txt
stack layout:  [..., return value, return id, call/jump id, args, locals]
1. caller:      ^^^
2. to call:                                   ^^^^^^^^^^^^^^^^^^
3. callee:           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
callee control flow:
  1. to jump:                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^
  2. post-jump:      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
1. to return:                      ^^^^^^^^^
2. post-return: ^^^^^^^^^^^^^^^^^
```

The `^^^` characters on each line indicate the portion of the stack (starting at the frame base) that remains accessible at that stage. The `return value` slot is where the callee's return value is placed, the `return id` slot is used to store the block ID to return to, the `call/jump id` slot is used to store the block ID to call or jump to, `args` represents the arguments passed to the callee, and `locals` represents the local variables of the callee.

### Backend

This component is responsible for translating the IR into BF code.

To accomplish this, the BF tape is laid out as follows:

```txt
reg1, reg0,
mem_base (0), stack_base (0),
temp0, mem0, mem0_marker, unused (0),
temp1, mem1, mem1_marker, stack0_marker,
temp2, mem2, mem2_marker, unused (0),
temp3, mem3, mem3_marker, stack1_marker,
temp4, mem4, mem4_marker, unused (0),
temp5, mem5, mem5_marker, stack2_marker,
...
```

The `reg0` cell serves as the destination for `Load` statements and the source for `Store` statements.

The `reg1` cell serves as a temporary register for `Store` statements that write to an indirect memory location, since the `reg0` cell is overwritten with the address of the target memory location during indirect addressing.

The `mem_base` cell is always set to zero and marks the beginning of the heap.

The `stack_base` cell is always set to zero and marks the beginning of the stack. This cell also serves as the home position for the BF data pointer, which is moved back to this cell after each statement is executed.

After the `reg1`, `reg0`, `mem_base`, and `stack_base` cells, the tape is divided into blocks of four cells each, with each block representing a memory location:

1. The first cell of each block is a temporary cell that is used by `Load` statements to preserve the value of the source memory location while it is being copied to the `reg0` cell, since copying is implemented in BF as a loop that repeatedly subtracts the value of the source cell and increments the value of the destination cell until the source cell is zero. By first moving the source cell to this temporary cell (i.e., by using a BF loop like `[-<+>]`), the source cell can then be restored while the `reg0` cell is updated.
2. The second cell of each block stores the actual value of the memory location.
3. The third cell of each block is a marker that is used to implement indirect addressing. When an indirect memory location is being accessed, a trail of 1s is written to the markers leading up to it. This trail can then be traversed by repeatedly moving left or right by four cells until a zero is encountered (i.e., `[<<<<]` or `[>>>>]` in BF). This allows for moving between the indirect memory location and `reg0` without needing to know the distance between them in advance.
4. The fourth cell of each block alternates between being unused (for even-numbered blocks, which are reserved for heap memory) and marking stack cells that are below the current frame base (for odd-numbered blocks, which are used for stack memory). To raise the frame base by `n` cells, `n` additional stack markers are set to 1, and to lower the frame base by `n` cells, the last `n` stack markers are set to 0. Thus, the first cell of the current frame is marked by the first stack marker with a value of 0. This facilitates accessing local variables, since the stack markers can be traversed to move between the stack base and the current frame base without indirect addressing.

## Supported Rust Features (non-exhaustive)

- [x] variables:
  - [x] immutable: `let three = 3;`
  - [x] mutable: `let mut a = 'A';`
  - [x] with explicit type: `let b: bool = false;`
- [x] types:
  - [x] primitives: `usize`, `char`, `bool`, `&str`
  - [x] tuples: `(T,)`, `(T, U)`, `(T, U, V)`, ...
  - [x] arrays: `[T; len]`
  - [x] references: `&T`, `&mut T`
    - [x] automatic coercion from `&mut T` to `&T` when appropriate
  - [x] structs: `struct Foo { a: usize, b: char }`
  - [x] explicit casting: `41 as char`, `true as usize`, `&mut arr as &mut [usize]`
- [x] expressions
  - [x] literals: `3`, `'A'`, `false`, `"Hello 👋 world!"`
  - [x] tuples: `(3,)`, `('A', false)`, `(30, true, 'B')`
  - [x] arrays:
    - [x] `[3, 1, 4, 1, 5, 9]`
    - [x] `[false; 4]` for `[false, false, false, false]`
  - [x] structs: `Foo { a: 12, b: 'W' } `
  - [x] operators (with standard precedence):
    - [x] arithmetic: `+`, `-`, `*`, `/`, `%`
    - [x] relational: `==`, `!=`, `>`, `>=`, `<`, `<=`
    - [x] logical: `&&`, `||`, `!`
  - [x] parentheses: `(`, `)`
- [x] assignment:
  - [x] operators: `=`, `+=`, `-=`, `*=`, `/=`, `%=`
  - [x] assignable places:
    - [x] variables: `x = 5;`
    - [x] tuple elements: `t.0 = 3;`
    - [x] struct fields: `f.a += 1`
    - [x] array elements: `arr[2] = 'H';`
    - [x] dereferenced references: `*p = 3;`
- [x] auto-deref:
  - [x] `p.0` is desugared to `(*p).0` if `p` is a reference to a tuple
  - [x] `p.a` is desugared to `(*p).a` if `p` is a reference to a struct
  - [x] `p[3]` is desugared to `(*p)[3]` if `p` is a reference to an array
- [x] if statements: `if condition { ... } else if other_condition { ... } else { ... }`
- [x] loops:
  - [x] infinite loop: `loop { ... }`
  - [x] while loop: `while condition { ... }`
  - [x] `break` and `continue` statements
- [x] functions:
  - [x] declaration: `fn foo(a: usize, b: char) -> bool { ... }`
  - [x] call: `foo(3, 'A')`
  - [x] recursion
  - [x] mutual recursion
- [x] comments:
  - [x] line comments: `// this is a line comment`
  - [x] block comments: `/* this is a /* nested */ block comment */`
- [x] macros: `print!`, `println!`, `panic!`
- [ ] enums
- [ ] pattern matching
- [ ] generics
- [ ] impl blocks
  - [ ] methods
  - [ ] associated functions
  - [ ] blanket impls
- [ ] trait-related features
  - [ ] traits
  - [ ] constrained generics
  - [ ] iterators
  - [ ] for loops
  - [ ] closures
- [ ] standard library types and traits
- [ ] expanded format specifier support
  - [ ] `{:?}` for Debug
- [ ] &[T] and &str with length information (i.e., fat pointers)
- [ ] non-ASCII char values
- [ ] expression-oriented features
  - [ ] control flow statements as expressions
  - [ ] last expression in block as return value
- [ ] range types/patterns
- [ ] expanded numeric types
  - [ ] specific-width integers
  - [ ] signed integers
  - [ ] floating-point numbers
- [ ] bitwise operators
- [ ] modules
- [ ] dynamic dispatch
- [ ] lifetimes and borrow-checking
