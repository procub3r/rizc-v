const std = @import("std");

/// Opcode types
const Opcode = enum(u7) {
    lui = 0b0110111,
    auipc = 0b0010111,
    jal = 0b1101111,
    jalr = 0b1100111,
    branch = 0b1100011,
    load = 0b0000011,
    store = 0b0100011,
    imm = 0b0010011,
    reg = 0b0110011,
    fence = 0b0001111,
    system = 0b1110011,
};

// Instruction formats
const InstrR = packed struct { op: u7, rd: u5, funct3: u3, rs1: u5, rs2: u5, funct7: u7 };
const InstrI = packed struct { op: u7, rd: u5, funct3: u3, rs1: u5, i0_11: u12 };
const InstrS = packed struct { op: u7, i0_4: u5, funct3: u3, rs1: u5, rs2: u5, i5_11: u7 };
const InstrB = packed struct { op: u7, i11: u1, i1_4: u4, funct3: u3, rs1: u5, rs2: u5, i5_10: u6, i12: u1 };
const InstrU = packed struct { op: u7, rd: u5, i12_31: u20 };
const InstrJ = packed struct { op: u7, rd: u5, i12_19: u8, i11: u1, i1_10: u10, i20: u1 };

/// Extract the immediate value from an instruction
fn immediate(instr: anytype) i32 {
    // Instruction immediates
    const ImmI = packed struct { i0_11: u12 };
    const ImmS = packed struct { i0_4: u5, i5_11: u7 };
    const ImmB = packed struct { zero: u1, i1_4: u4, i5_10: u6, i11: u1, i12: u1 };
    const ImmU = packed struct { zero: u12, i12_31: u20 };
    const ImmJ = packed struct { zero: u1, i1_10: u10, i11: u1, i12_19: u8, i20: u1 };

    const imm = switch (@TypeOf(instr)) {
        InstrI => ImmI{ .i0_11 = instr.i0_11 },
        InstrS => ImmS{ .i0_4 = instr.i0_4, .i5_11 = instr.i5_11 },
        InstrB => ImmB{ .zero = 0, .i1_4 = instr.i1_4, .i5_10 = instr.i5_10, .i11 = instr.i11, .i12 = instr.i12 },
        InstrU => ImmU{ .zero = 0, .i12_31 = instr.i12_31 },
        InstrJ => ImmJ{ .zero = 0, .i1_10 = instr.i1_10, .i11 = instr.i11, .i12_19 = instr.i12_19, .i20 = instr.i20 },
        else => @compileError("Invalid type"),
    };
    return @as(std.meta.Int(.signed, @bitSizeOf(@TypeOf(imm))), @bitCast(imm));
}

/// Unprivileged single-hart RV32I core
/// with XLEN = 32, ILEN = 32
pub const Core = struct {
    x: [32]i32, // registers
    pc: i32, // program counter
    instr_raw: u32 = 0, // current instruction
    csr: [4096]i32, // control and status registers
    memory: []u8,

    const Self = @This();

    /// Create a core
    pub fn init(memory: []u8) Self {
        return Self{ .x = .{0} ** 32, .csr = .{0} ** 4096, .pc = 0, .memory = memory };
    }

    /// Dump the architectural state of the core
    pub fn dump(self: *Self) void {
        std.debug.print("---\npc = 0x{x:0>8}\n", .{self.pcUnsigned()});
        var i: u5 = 0;
        while (i < 16) : (i += 1) {
            std.debug.print(
                "x{d:<2}= 0x{x:0>8}  " ** 2 ++ "\n",
                .{ i, self.regUnsigned(i), 16 + i, self.regUnsigned(16 + i) },
            );
        }
    }

    /// Load a value of type T from memory[addr]
    fn load(self: *Self, comptime T: type, addr: u32) T {
        return std.mem.bytesAsValue(T, self.memory[addr .. addr + @sizeOf(T)]).*;
    }

    /// Store a value of type T to memory[addr]
    fn store(self: *Self, addr: u32, value: anytype) void {
        const T = @TypeOf(value);
        std.mem.bytesAsValue(T, self.memory[addr .. addr + @sizeOf(T)]).* = value;
    }

    /// Read register value. Reads to x0 return 0
    fn reg(self: *Self, i: u5) i32 {
        if (i == 0) return 0;
        return self.x[i];
    }

    /// Read register unsigned. Reads to x0 return 0
    fn regUnsigned(self: *Self, i: u5) u32 {
        return @bitCast(self.reg(i));
    }

    /// Bit cast pc into an unsigned int
    inline fn pcUnsigned(self: *Self) u32 {
        return @bitCast(self.pc);
    }

    /// Handle illegal-instruction exception.
    /// This is a temporary function! TODO: Implement exception / interrupt trapping.
    fn illegalInstr(self: *Self) noreturn {
        std.debug.panic(
            "illegal-instruction exception. found 0x{x:0>8} at addr 0x{x:0>8}",
            .{ self.instr_raw, self.pcUnsigned() },
        );
    }

    /// Jump to addr
    fn jump(self: *Self, addr: i32) void {
        if (self.pcUnsigned() % 4 != 0)
            std.debug.panic("instruction-address-misaligned exception. target addr 0x{x:0>8} is not 0 (mod 4)", .{addr});
        self.pc = addr;
    }

    /// Step through the next instruction
    pub fn step(self: *Self) void {
        // Fetch raw 32 bit instruction from memory
        self.instr_raw = self.load(u32, @bitCast(self.pc));

        // Encodings with bits [15:0] all zeros are defined as illegal instructions.
        // The encoding with bits [ILEN-1:0] all ones is also illegal.
        if (self.instr_raw & 0xffff == 0 or self.instr_raw == 0xffffffff) self.illegalInstr();

        // Extract opcode (bits [6:0]) from the instruction
        const opcode_raw: u7 = @truncate(self.instr_raw);
        const opcode: Opcode = @enumFromInt(opcode_raw);

        // Decode and Execute instruction
        switch (opcode) {
            .lui => {
                // load upper immediate
                const instr: InstrU = @bitCast(self.instr_raw);
                self.x[instr.rd] = immediate(instr);
            },
            .auipc => {
                // add upper immediate to pc
                const instr: InstrU = @bitCast(self.instr_raw);
                self.x[instr.rd] = self.pc +% immediate(instr);
            },
            .jal => {
                // jump and link
                const instr: InstrJ = @bitCast(self.instr_raw);
                self.x[instr.rd] = self.pc +% 4; // link
                const target = self.pc +% immediate(instr);
                self.jump(target); // jump
                return; // return here to avoid incrementing pc at the end
            },
            .jalr => {
                // jump and link register
                const instr: InstrI = @bitCast(self.instr_raw);
                if (instr.funct3 != 0b000) self.illegalInstr();
                self.x[instr.rd] = self.pc +% 4; // link

                // target is obtained by adding imm to rs1 and clearing the LSB
                const target = (self.reg(instr.rs1) +% immediate(instr)) >> 1 << 1;
                self.jump(target); // jump
                return; // return here to avoid incrementing pc at the end
            },
            .branch => branch: {
                const instr: InstrB = @bitCast(self.instr_raw);
                const condition = switch (instr.funct3) {
                    0b000 => self.reg(instr.rs1) == self.reg(instr.rs2), // beq
                    0b001 => self.reg(instr.rs1) != self.reg(instr.rs2), // bne
                    0b100 => self.reg(instr.rs1) < self.reg(instr.rs2), // blt
                    0b101 => self.reg(instr.rs1) >= self.reg(instr.rs2), // bge
                    0b110 => self.regUnsigned(instr.rs1) < self.regUnsigned(instr.rs2), // bltu
                    0b111 => self.regUnsigned(instr.rs1) >= self.regUnsigned(instr.rs2), // bgeu
                    else => self.illegalInstr(),
                };
                if (!condition) break :branch; // don't branch if the condition fails
                self.jump(immediate(instr));
                return; // return here to avoid incrementing pc at the end
            },
            .load => {
                const instr: InstrI = @bitCast(self.instr_raw);
                const addr: u32 = @bitCast(immediate(instr) +% self.reg(instr.rs1));
                self.x[instr.rd] = switch (instr.funct3) {
                    0b000 => self.load(i8, addr), // lb
                    0b001 => self.load(i16, addr), // lh
                    0b010 => self.load(i32, addr), // lw
                    0b100 => self.load(u8, addr), // lbu
                    0b101 => self.load(u16, addr), // lhu
                    else => self.illegalInstr(),
                };
            },
            .store => {
                const instr: InstrS = @bitCast(self.instr_raw);
                const addr: u32 = @bitCast(immediate(instr) +% self.reg(instr.rs1));
                switch (instr.funct3) {
                    0b000 => self.store(addr, @as(u8, @truncate(self.regUnsigned(instr.rs2)))), // sb
                    0b001 => self.store(addr, @as(u16, @truncate(self.regUnsigned(instr.rs2)))), // sh
                    0b010 => self.store(addr, self.reg(instr.rs2)), // sw
                    else => self.illegalInstr(),
                }
            },
            .imm => {
                const instr: InstrI = @bitCast(self.instr_raw);
                const imm = immediate(instr);
                const imm_unsigned: u32 = @bitCast(imm);
                const imm_upper: u7 = @truncate(imm_unsigned >> 5);
                const shamt: u5 = @truncate(imm_unsigned); // lower 5 bits of imm
                const result: i32 = switch (instr.funct3) {
                    0b000 => self.reg(instr.rs1) +% imm, // addi
                    0b010 => @intFromBool(self.reg(instr.rs1) < imm), // slti
                    0b011 => @intFromBool(self.regUnsigned(instr.rs1) < imm_unsigned), // sltiu
                    0b001 => switch (imm_upper) {
                        0b0000000 => self.reg(instr.rs1) << shamt, // slli
                        else => self.illegalInstr(),
                    },
                    0b101 => switch (imm_upper) {
                        0b0000000 => @bitCast(@as(u32, @bitCast(self.reg(instr.rs1))) >> shamt), // srli
                        0b0100000 => self.reg(instr.rs1) >> shamt, // srai
                        else => self.illegalInstr(),
                    },
                    0b100 => self.reg(instr.rs1) ^ imm, // xori
                    0b110 => self.reg(instr.rs1) | imm, // ori
                    0b111 => self.reg(instr.rs1) & imm, // andi
                };
                self.x[instr.rd] = result;
            },
            .reg => {
                const instr: InstrR = @bitCast(self.instr_raw);
                const x = self.reg(instr.rs1);
                const y = self.reg(instr.rs2);
                const shamt: u5 = @truncate(self.regUnsigned(instr.rs2));
                const result: i32 = switch (instr.funct7) {
                    0b0000000 => switch (instr.funct3) {
                        0b000 => x +% y, // add
                        0b001 => x << shamt, // sll
                        0b010 => @intFromBool(x < y), // slt
                        0b011 => @intFromBool(self.regUnsigned(instr.rs1) < self.regUnsigned(instr.rs2)), // sltu
                        0b100 => x ^ y, // xor
                        0b101 => @bitCast(@as(u32, @bitCast(x)) >> shamt), // srl
                        0b110 => x | y, // or
                        0b111 => x & y, // and
                    },
                    0b0100000 => switch (instr.funct3) {
                        0b000 => x -% y, // sub
                        0b101 => x >> @truncate(@as(u32, @bitCast(y))), // sra
                        else => self.illegalInstr(),
                    },
                    else => self.illegalInstr(),
                };
                self.x[instr.rd] = result;
            },
            // fence is a nop cause memory reads and writes are emulated in order.
            // pause (under the fence opcode) is also a nop
            .fence => {},
            .system => {
                const instr: InstrI = @bitCast(self.instr_raw);
                switch (instr.funct3) {
                    0b000 => {
                        switch (instr.i0_11) {
                            0b000000000000 => {}, // ecall
                            0b000000000001 => {}, // ebreak
                            else => self.illegalInstr(),
                        }
                    },
                    // Zicsr extension
                    // TODO: Implement csrRead() and csrWrite() instead of directly reading and writing csrs
                    0b001 => { // csrrw
                        const initial_rs1 = self.reg(instr.rs1);
                        // If rd=x0, then the instruction shall not read the CSR and shall
                        // not cause any of the side effects that might occur on a CSR read.
                        if (instr.rd != 0) self.x[instr.rd] = self.csr[instr.i0_11];
                        self.csr[instr.i0_11] = initial_rs1;
                    },
                    // For both CSRRS and CSRRC, if rs1=x0, then the instruction will not write to the
                    // CSR at all, and so shall not cause any of the side effects that might otherwise occur
                    // on a CSR write, nor raise illegal-instruction exceptions on accesses to read-only CSRs
                    0b010 => { // csrrs
                        const initial_rs1 = self.reg(instr.rs1);
                        self.x[instr.rd] = self.csr[instr.i0_11];
                        // Any bit that is high in rs1 will cause the corresponding
                        // bit to be set in the CSR, (TODO:) if that CSR bit is writable.
                        if (instr.rs1 != 0) self.csr[instr.i0_11] |= initial_rs1;
                    },
                    0b011 => { // csrrc
                        const initial_rs1 = self.reg(instr.rs1);
                        self.x[instr.rd] = self.csr[instr.i0_11];
                        // Any bit that is high in rs1 will cause the corresponding
                        // bit to be cleared in the CSR, (TODO:) if that CSR bit is writable.
                        if (instr.rs1 != 0) self.csr[instr.i0_11] &= ~initial_rs1;
                    },
                    0b101 => { // csrrwi
                        // If rd=x0, then the instruction shall not read the CSR and shall
                        // not cause any of the side effects that might occur on a CSR read.
                        if (instr.rd != 0) self.x[instr.rd] = self.csr[instr.i0_11];
                        self.csr[instr.i0_11] = instr.rs1;
                    },
                    0b110 => { // csrrsi
                        self.x[instr.rd] = self.csr[instr.i0_11];
                        // Any bit that is high in rs1 will cause the corresponding
                        // bit to be set in the CSR, (TODO:) if that CSR bit is writable.
                        if (instr.rs1 != 0) self.csr[instr.i0_11] |= instr.rs1;
                    },
                    0b111 => { // csrrci
                        self.x[instr.rd] = self.csr[instr.i0_11];
                        // Any bit that is high in rs1 will cause the corresponding
                        // bit to be cleared in the CSR, (TODO:) if that CSR bit is writable.
                        if (instr.rs1 != 0) self.csr[instr.i0_11] &= ~instr.rs1;
                    },
                    else => self.illegalInstr(),
                }
            },
        }
        self.pc +%= 4; // increment pc
    }
};
