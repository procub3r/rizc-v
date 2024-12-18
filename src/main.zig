const std = @import("std");
const Bus = @import("core.zig").Bus;
const Core = @import("core.zig").Core;

pub fn main() !void {
    std.debug.print("Run `zig build test`\n", .{});
}

test "rv32ui" {
    // Memory to run tests
    const TestMemory = struct {
        const Self = @This();
        raw: [0x10000]u8,
        offset: usize, // Address of first byte in raw
        bus: Bus,

        pub fn init(offset: usize) Self {
            return Self{
                .raw = .{0} ** 0x10000,
                .offset = offset,
                .bus = .{
                    .readByte = readByte,
                    .writeByte = writeByte,
                },
            };
        }

        fn readByte(bus_ptr: *Bus, addr_: u32) error{InaccessibleAddress}!u8 {
            const self: *const Self = @fieldParentPtr("bus", bus_ptr);
            const addr = addr_ - self.offset;
            if (addr >= self.raw.len) return error.InaccessibleAddress;
            return self.raw[addr];
        }

        fn writeByte(bus_ptr: *Bus, addr_: u32, byte: u8) error{InaccessibleAddress}!void {
            const self: *Self = @fieldParentPtr("bus", bus_ptr);
            const addr = addr_ - self.offset;
            if (addr >= self.raw.len) return error.InaccessibleAddress;
            self.raw[addr] = byte;
        }
    };

    // Create the memory and the core
    var memory = TestMemory.init(0);
    var core = Core.init(&memory.bus);

    // Run an instruction!
    // This must raise an illegal-instruction exception because the memory is un-initialized
    try core.step();
}
