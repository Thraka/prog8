---
name: progb-langtest
description: Verify that a ProgB (.pb) source file compiles to the exact same assembly as its equivalent Prog8 (.p8) file, using the run-tests.ps1 harness. Use when adding or validating a ProgB language feature and you want to confirm output parity with Prog8 for a matched code pair.
license: MIT
compatibility: opencode
---

# ProgB Langtest Skill

Use this skill to prove that a **ProgB** (`.pb`) program produces **byte-identical assembly** to the equivalent **Prog8** (`.p8`) program. This is the primary manual acceptance test when implementing or changing a ProgB language feature (e.g. `PRIVATE`, `ENUM`, `SWAP`, 2D arrays).

The harness lives at `compiler/langtest/run-tests.ps1`. It compiles a matched `.p8`/`.pb` pair with the same base name, strips comments, and diffs the generated `.asm`.

## Prerequisites

- **Build the compiler first.** The harness runs the installed `prog8c`, not your latest source, so rebuild after any compiler/grammar change:
  ```powershell
  .\gradlew.bat installdist
  ```
  After editing the `.g4` grammar, regenerate first: `.\gradlew.bat :parser:generateGrammarSource` then `.\gradlew.bat installdist`.
- **Drive/root gotcha:** if compiling crashes with `IllegalArgumentException: 'other' has different root` in `SourceCode.relative()`, the workspace was opened via a mapped/duplicate drive path. Open and run from the repo's real drive location instead.

## Test pair layout

Put both files in the target folder under `compiler/langtest/`, with the **same base name**:

```
compiler/langtest/<target>/<name>.p8    # Prog8 version
compiler/langtest/<target>/<name>.pb    # ProgB version
```

- `<target>` is `c64` or `cx16` (folder name = compile target).
- Both files must be **semantically equivalent** — same blocks, subs, and logic — just expressed in each syntax. The goal is identical generated code.
- The harness auto-copies any sibling files whose names are referenced in the source (for includes/binaries).

## Writing a good pair

- Keep the two files as close as possible so any asm difference points at a real frontend/AST discrepancy.
- Exercise the specific feature under test in a way that survives optimization (unused private vars and aliases get removed during full compilation — that's fine here because we compare *asm output*, but make sure the feature actually affects emitted code, e.g. reference the symbols).

## Running

```powershell
# From the repo root:
.\compiler\langtest\run-tests.ps1 -Target cx16 -TestName <name>

# Or from inside compiler\langtest:
.\run-tests.ps1 -Target c64 -TestName <name>
```

### Parameters
- `-Target <c64|cx16>` — folder of test code and the compile target. Default `c64`.
- `-TestName <name>` — base name of the pair (no extension). Omit to run **all** pairs in the target folder.
- `-SwapOrder` — compile Prog8 first (helps isolate order-dependent issues).
- `-Verbose` — print the exact `prog8c` command lines.
- `-CompilerPath <path>` — override the auto-detected `prog8c.bat`.

Debugging aids (leave off for normal runs; only add when investigating a mismatch):
- `-KeepOutput` — keep `compiler/langtest/output/` (with `progb/` and `prog8/` subdirs) so you can open the full `.asm` / `.stripped.asm` files. Not needed for a normal pass/fail run — the script already prints the diff.
- `-KeepComments` — compare assembly *including* comments (stricter). The default strips comments; keep it default normally, but enabling it helps trace mismatched assembly lines back to their source when debugging.

## Interpreting results

- **PASS** — stripped assembly matched; ProgB and Prog8 emit identical code for that pair.
- **FAIL** — the harness prints up to 10 line-level diffs labeled `ProgB:` vs `Prog8:`. That's usually enough to spot the problem; a diff typically means the ProgB grammar/visitor mapped the construct to a different AST than Prog8. To dig deeper, re-run with `-KeepOutput -KeepComments` and open `output/progb/<name>.asm` vs `output/prog8/<name>.asm` — the comments help map each asm line back to its source.
- **Errors: 1** with a `COMPILE ERROR` block — one side failed to compile. Read the printed stdout/stderr. Common causes: missing `rts` in `asmsub`, a genuine ProgB grammar/visitor bug, or the drive-root crash above.

## Typical workflow for a feature

1. Implement/adjust the ProgB grammar (`Prog8QB.g4`) and visitor (`Antlr2KotlinVisitorQB.kt`).
2. `.\gradlew.bat :parser:generateGrammarSource` then `.\gradlew.bat installdist`.
3. Create `compiler/langtest/<target>/<name>.p8` and `<name>.pb` exercising the feature.
4. `.\compiler\langtest\run-tests.ps1 -Target <target> -TestName <name>`.
5. If FAIL, read the printed diff; for deeper investigation re-run with `-KeepOutput -KeepComments`, then fix the visitor/grammar, rebuild, and re-run.
6. For AST-flag/shape parity that optimization would erase, also add a parse-level kotest under `compiler/test/progb/` using `Prog8Parser.parseModule` / `ProgBParser.parseModule` on `SourceCode.Text(...)`.
