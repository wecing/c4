load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_test")

scala_library(
    name = "util",
    srcs = ["TestUtil.scala"],
    visibility = [
        "//src/test/scala:__subpackages__",
    ],
)

scala_test(
    name = "tests",
    srcs = glob(["*Test.scala"]),
    deps = ["//src/main/scala/c4:lib"],
)
