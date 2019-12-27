load("@io_bazel_rules_scala//scala_proto:scala_proto_toolchain.bzl",
     "scala_proto_toolchain")

scala_proto_toolchain(
    name = "default_scala_proto_toolchain_impl",
    with_flat_package = True,
    with_grpc = False,
    with_single_line_to_string = False,
)

toolchain(
    name = "default_scala_proto_toolchain",
    toolchain = ":default_scala_proto_toolchain_impl",
    toolchain_type = "@io_bazel_rules_scala//scala_proto:toolchain_type",
)
