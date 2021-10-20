# BASM

This is an assembler for the Butka Instruction Set, a theoretical ISA for Dr. Butka's CEC 330
class at Embry-Riddle.

## Usage

To see program usage details, use the `--help` argument.

```plaintext
$ basm --help
basm 0.1.0
Nicolas 'Montessquio' Suarez <nsuarez@kernelstack.net>
An assembler for the Butka Instruction Set (basm).

USAGE:
    basm [FLAGS] [OPTIONS] <INPUT>

FLAGS:
    -h, --help       Prints help information
    -e               preprocess only
    -d               prints the debug information alongside the assembly
    -V, --version    Prints version information
    -v               Sets the level of verbosity

OPTIONS:
    -o <output>        write output to an outfile

ARGS:
    <INPUT>    Sets the input file to use
```

By default, the program writes the assembled binary file to the current working directory.

## Building

This project relies on rustc 1.55.0 (c8dfcfe04 2021-09-06).

Clone the repository and use `cargo build`.

## Testing

Running tests is as simple as cloning the repository and running `cargo test`.

## License

This software is licensed under the MIT License.

See `LICENSE`.