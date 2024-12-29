const std = @import("std");

/// Execution Environment Interface
pub const EEI = struct {
    read_byte_ptr: *const fn (self: *Self, addr: u32) error{InaccessibleAddress}!u8,
    write_byte_ptr: *const fn (self: *Self, addr: u32, byte: u8) error{InaccessibleAddress}!void,

    const Self = @This();

    pub fn readByte(self: *Self, comptime T: type, addr: u32) error{InaccessibleAddress}!T {
        var buf: [@sizeOf(T)]u8 = undefined;
        for (0..@sizeOf(T)) |i| {
            buf[i] = try self.read_byte_ptr(self, addr + @as(u32, @intCast(i)));
        }
        return std.mem.bytesToValue(T, &buf);
    }

    pub fn writeByte(self: *Self, addr: u32, value: anytype) error{InaccessibleAddress}!void {
        for (std.mem.toBytes(value), 0..) |byte, i| {
            try self.write_byte_ptr(self, addr + @as(u32, @intCast(i)), byte);
        }
    }
};
