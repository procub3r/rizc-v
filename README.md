# rizc-v

A RISC-V emulator written in Zig with the goal of running Linux.

## This repo has been ARCHIVED on 27 Feb 2025.
## This has been a great run but this project has been restarted at [procub3r/riscv-emu](https://github.com/procub3r/riscv-emu).

## Dependencies

- Grab `riscv64-elf-*.tar.gz` from [riscv-gnu-toolchain](https://github.com/riscv-collab/riscv-gnu-toolchain) releases and add the `bin/` folder to PATH to build tests.
- Install `libmpc` if you don't already have it (non devel package is fine). Required by riscv-gnu-toolchain to build the tests.

## Build, Run and Test

Clone with the `--recurse-submodules` flag to pull [riscv-tests](https://github.com/riscv-software-src/riscv-tests) as a submodule for testing.

```
zig build run  # To run the emulator
zig build test # To run tests
```

## TODO

- [x] Unprivileged RV32I core
- [ ] Put tests in place
    - [ ] Try to compile tests with `build.zig` instead of `riscv-gnu-toolchain`
- [ ] MAFD extensions
- [ ] Privileged RV32I core
- [ ] Port OpenSBI
    - Linux needs this
- [ ] Port Linux
    - ez `¯\_(ツ)_/¯`
