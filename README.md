# rizc-v

A RISC-V emulator written in Zig with the goal of running Linux.

## Dependencies

None (yet)

## Build

`$ zig build run`

## TODO

- [x] Unprivileged RV32I core
- [ ] Put tests in place
    - Integrate with [riscv-tests](https://github.com/riscv-software-src/riscv-tests)
- [ ] MAFD extensions
- [ ] Privileged RV32I core
- [ ] Port OpenSBI
    - Linux needs this
- [ ] Port Linux
    - ez `¯\_(ツ)_/¯`
