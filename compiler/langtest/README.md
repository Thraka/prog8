# ProgB / Prog8 Language Equivalence Tests

This directory contains tests that validate ProgB and Prog8 produce identical assembly output.

## Directory Structure

```
langtest/
├── progb/          # ProgB source files (.pb)
├── prog8/          # Prog8 source files (.p8)
├── output/         # Generated during test run (temporary)
├── run-tests.ps1   # Test runner script
└── README.md       # This file
```

## Running Tests

```powershell
# Run all tests for C64 target (default)
.\run-tests.ps1

# Run a specific test only
.\run-tests.ps1 -TestName hello
.\run-tests.ps1 -TestName controlflow

# Run for a specific target
.\run-tests.ps1 -Target cx16
.\run-tests.ps1 -Target virtual

# Keep output files for inspection
.\run-tests.ps1 -KeepOutput

# Verbose output (shows compile commands)
.\run-tests.ps1 -Verbose

# Combine options
.\run-tests.ps1 -TestName hello -Target cx16 -KeepOutput -Verbose

# Specify custom compiler path
.\run-tests.ps1 -CompilerPath "C:\path\to\prog8c.bat"
```

## Adding New Tests

1. Create a `.pb` file in the `progb/` directory
2. Create a matching `.p8` file with the same base name in the `prog8/` directory
3. Run `.\run-tests.ps1` to validate they produce identical assembly

### Example

```
progb/mytest.pb   <-- ProgB version
prog8/mytest.p8   <-- Prog8 version (must have same base name)
```

## Test Files

| Test | Description |
|------|-------------|
| hello | Basic hello world |
| variables | Variable declarations, types, constants |
| controlflow | For, while, do-until, repeat, if-else |
| operators | Arithmetic, bitwise, comparison, logical operators |
| functions | Subroutines, functions, inline functions |
| whencase | When/Select Case statements |

## How It Works

1. The script finds matching `.pb` and `.p8` files by base name
2. Each file is compiled to assembly using the Prog8 compiler
3. Comments are stripped from both assembly outputs
4. The stripped assembly is compared line-by-line
5. Test passes if assembly is identical, fails if different

## Prerequisites

Build the compiler first:

```cmd
.\gradlew.bat installdist
```

The script will look for the compiler at:
- `..\..\compiler\build\install\prog8c\bin\prog8c.bat`
- `..\..\build\install\prog8c\bin\prog8c.bat`
