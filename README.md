# rizc-v

A RISC-V emulator written in Zig with the goal of running Linux.

## Dependencies

Grab `riscv64-elf-*.tar.gz` from [riscv-gnu-toolchain](https://github.com/riscv-collab/riscv-gnu-toolchain) releases and add it to PATH to build tests.

## Build, Run and Test

Clone with the `--recurse-submodules` flag to pull [riscv-tests](https://github.com/riscv-software-src/riscv-tests) as a submodule for testing.

```
zig build run  # To run the emulator
zig build test # To run tests
```

## TODO

- [x] Unprivileged RV32I core
- [ ] Put tests in place
    - Try to compile tests with `build.zig` instead of `riscv-gnu-toolchain`
- [ ] MAFD extensions
- [ ] Privileged RV32I core
- [ ] Port OpenSBI
    - Linux needs this
- [ ] Port Linux
    - ez `¯\_(ツ)_/¯`
