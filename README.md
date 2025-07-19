# Xylem Programming Language

Xylem is a statically-typed, block-structured programming language with support for functions, variables, control flow, and more. This project includes an interpreter and a suite of language tests.

## Features
- Variable declarations and assignments
- Functions with typed parameters and return types
- If/elif/else statements
- While and for loops
- Break and return
- Type checking and semantic analysis
- Error reporting
- Test suite for language features

## Prerequisites
- [Rust](https://www.rust-lang.org/tools/install) (latest stable recommended)
- PowerShell (for running the test script on Windows)

## Building the Interpreter

Clone the repository and build the project:

```sh
# Clone the repo
git clone https://github.com/KronosWasTaken/Xylem.git
cd Xylem

# Build in release mode
cargo build --release
```

This will produce the executable at `target/release/xylem` (or `xylem.exe` on Windows).

## Running .xl Programs

1. Add `xylem.exe` to your PATH
2. Open a terminal in the folder containing your `.xl` file.
3. Run:
   ```sh
   xylem your_file.xl
   ```

Example:
```sh
xylem example.xl
```

## Running the Test Suite

All language tests are in the `tests/` folder as `.xl` files.

### On Windows (PowerShell)
Use the provided script:
```sh
./run_all_tests.ps1
```
This will run all `.xl` files in the `tests/` folder and report any failures.

## Language Reference
See [`src/grammar.ebnf`](src/grammar.ebnf) for the full language grammar.

## Example
See [`example.xl`](example.xl) for a showcase of all language features.