const std = @import("std");
const EEI = @import("eei.zig").EEI;
const Core = @import("core.zig").Core;

// Test a single test binary
test "rv32ui-p-addi" {
    // Open the ELF file and parse the ELF header
    const elf_file = try std.fs.cwd().openFile("riscv-tests/isa/rv32ui-p-addi", .{});
    const elf_hdr = try std.elf.Header.read(elf_file);

    // Create the test execution environment.
    // All test binaries use memory starting from their entrypoints.
    var test_env = TestEnvironment.init(elf_hdr.entry);

    // Iterate through all the program headers
    var program_headers = elf_hdr.program_header_iterator(elf_file);
    while (try program_headers.next()) |phdr| {
        // Ignore non-loadable segments
        if (phdr.p_type != std.elf.PT_LOAD) continue;

        // Load segment to test memory
        try test_env.loadSegment(phdr, elf_file);
    }

    // Create the RISC-V core
    var core = Core.init(&test_env.eei);
    // Set PC to the entry point of the test binary
    core.pc = @bitCast(@as(u32, @intCast(elf_hdr.entry)));

    // Run an instruction!
    try core.step();
    core.dump();
}

/// Test Execution Environment
const TestEnvironment = struct {
    memory: [0x10000]u8, // Memory region used by test processes
    // TODO: Come up with a better name and explanation for the offset field
    offset: usize, // Memory region start address.
    eei: EEI,

    const Self = @This();

    pub fn init(offset: usize) Self {
        return Self{
            .memory = undefined,
            .offset = offset,
            .eei = .{
                .read_byte_ptr = readByte,
                .write_byte_ptr = writeByte,
            },
        };
    }

    fn readByte(bus_ptr: *EEI, addr_: u32) error{InaccessibleAddress}!u8 {
        const self: *const Self = @fieldParentPtr("eei", bus_ptr);
        const addr = addr_ - self.offset;
        if (addr >= self.memory.len) return error.InaccessibleAddress;
        return self.memory[addr];
    }

    fn writeByte(bus_ptr: *EEI, addr_: u32, byte: u8) error{InaccessibleAddress}!void {
        const self: *Self = @fieldParentPtr("eei", bus_ptr);
        const addr = addr_ - self.offset;
        if (addr >= self.memory.len) return error.InaccessibleAddress;
        self.memory[addr] = byte;
    }

    /// Load segment from an ELF file to raw memory
    pub fn loadSegment(self: *Self, phdr: std.elf.Elf64_Phdr, elf_file: std.fs.File) !void {
        const start = phdr.p_vaddr - self.offset;
        const end = start + phdr.p_filesz;
        try elf_file.seekableStream().seekTo(phdr.p_offset);
        try elf_file.reader().readNoEof(self.memory[start..end]);
        // It is not necessary to zero extend loaded segments because all
        // segments in the test binaries have p_memsz equal to p_filesz.
    }
};

pub fn main() !void {
    std.debug.print("Run `zig build test`\n", .{});
}
