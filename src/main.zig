const std = @import("std");
const Core = @import("core.zig").Core;

pub fn main() !void {
    // Hardcoded instructions
    var memory = [_]u8{
        0x67, 0x0c, 0x84, 0x00, // jalr x24, 8(x8) <-,
        0x00, 0x00, 0x00, 0x00, // padding           |
        0x67, 0x0d, 0x00, 0x00, // jalr x0, 0(x0)  --'
    };

    // Initialize core
    var core = Core.init(&memory);

    // Step 4'evah!
    while (true) {
        core.step();
        core.dump();
    }
}
