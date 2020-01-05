load("@io_bazel_rules_scala//scala:scala.bzl", "scala_test")

scala_test(
    name = "tests",
    srcs = glob(["*.scala"]),
    deps = [
        "//src/main/scala/c4:lib",
        "//src/test/scala/c4/util",
        "//third_party/jvm/org/typelevel:cats_core",
    ],
    resources = [
        "//src/test/scala/c4/io/pp_test_data",
    ],
)
