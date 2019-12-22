load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary")

scala_library(
    name = "lib",
    srcs = glob(["**/*.scala"]) + ["//src/main/resources:gen_parser"],
    deps = [
        "//third_party/com/github/vbmacher:java_cup_runtime",
        "//third_party/org/typelevel:cats_core",
    ],
    visibility = [
        "//src/test/scala:__subpackages__",
    ],
)

scala_binary(
    name = "main",
    srcs = ["Main.scala"],
    deps = [
        ":lib",
        "//third_party/com/github/vbmacher:java_cup_runtime",
    ],
    main_class = "c4.Main",
)
