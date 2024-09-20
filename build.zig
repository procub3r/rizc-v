const std = @import("std");

pub fn build(b: *std.Build) void {
    // standard target and optimziation options
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // main executable
    const exe = b.addExecutable(.{
        .name = "rizc-v",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    // run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    // cmdline args
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // run executable
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // tests
    const build_riscv_tests = b.addSystemCommand(&.{ "make", "-C", "riscv-tests/isa" });
    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    run_exe_unit_tests.step.dependOn(&build_riscv_tests.step);

    // run tests
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
