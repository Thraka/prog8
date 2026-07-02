# ProgB Examples

This directory contains example programs written in **ProgB**, the BASIC-style syntax frontend for Prog8.

## What is ProgB?

ProgB is an alternative syntax for Prog8 that provides a QuickBASIC-style syntax while compiling to the same AST and backend. This allows developers who prefer BASIC syntax to write programs for 6502-based systems while leveraging Prog8's powerful compiler infrastructure.

## File Extension

- `.pb` - ProgB source files (BASIC syntax)
- `.p8` - Prog8 source files (original syntax)

Both file types can coexist in the same project and import each other!

## Examples

| File | Description |
|------|-------------|
| [hello.pb](hello.pb) | Hello World with various ProgB features demonstration |
| [numbergame.pb](numbergame.pb) | Classic number guessing game |
| [primes.pb](primes.pb) | Prime number calculation using Sieve of Eratosthenes |
| [fibonacci.pb](fibonacci.pb) | Fibonacci sequence calculation |
| [bordercolor.pb](bordercolor.pb) | Border color animation with inline assembly |

## Compiling

Compile ProgB files the same way as Prog8 files:

```bash
prog8c -target c64 hello.pb
```

## Key Syntax Differences from Prog8

| Prog8 | ProgB |
|-------|-------|
| `blockname { }` | `MODULE blockname ... END MODULE` |
| `sub name() { }` | `SUB name() ... END SUB` |
| `sub name() -> ubyte { }` | `FUNCTION name() AS UBYTE ... END FUNCTION` |
| `ubyte x = 5` | `DIM x AS UBYTE = 5` |
| `if condition { }` | `IF condition THEN ... END IF` |
| `for i in 1 to 10 { }` | `FOR i = 1 TO 10 ... NEXT` |
| `while condition { }` | `WHILE condition ... WEND` |
| `; comment` | `' comment` |
| `%import textio` | `IMPORT textio` |

## Learn More

See the full ProgB language specification in the documentation.
