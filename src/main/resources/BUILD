load("@jflex_rules//cup:cup.bzl", "cup")
load("@io_bazel_rules_proto//proto:defs.bzl", "proto_library")
load("@io_bazel_rules_scala//scala_proto:scala_proto.bzl",
     "scala_proto_library")

cup(
    name = "gen_parser",
    src = "parser.cup",
    parser = "C4Parser",
    symbols = "C4Symbols",
    visibility = ["//visibility:public"],
)

proto_library(
    name = "ast_proto",
    srcs = ["ast.proto"],
)

scala_proto_library(
    name = "ast_scala_proto",
    deps = [":ast_proto"],
    visibility = ["//visibility:public"],
)
