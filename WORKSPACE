workspace(name = "c4")

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "jflex_rules",
    remote = "https://github.com/jflex-de/bazel_rules.git",
    commit = "9d4239f31104e1311258268ecc5e3cb41f048f6f", # 12/29 2019
    shallow_since = "1577655108 +0100",
)

git_repository(
    name = "io_bazel_rules_scala",
    remote = "https://github.com/bazelbuild/rules_scala.git",
    commit = "bd0c388125e12f4f173648fc4474f73160a5c628", # 12/28 2019
    shallow_since = "1577561321 +0200",
)

git_repository(
    name = "io_bazel_rules_proto",
    remote = "https://github.com/bazelbuild/rules_proto.git",
    commit = "2c0468366367d7ed97a1f702f9cd7155ab3f73c5", # 12/4 2019
    shallow_since = "1575470667 +0100",
)

git_repository(
    name = "bazel_skylib",
    remote = "https://github.com/bazelbuild/bazel-skylib.git",
    commit = "327d61b5eaa15c11a868a1f7f3f97cdf07d31c58", # 10/30 2019
    shallow_since = "1572441481 +0100",
)

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
scala_register_toolchains()

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

load("@io_bazel_rules_scala//scala_proto:scala_proto.bzl",
     "scala_proto_repositories")
scala_proto_repositories()

register_toolchains("//:default_scala_proto_toolchain")

load("@io_bazel_rules_proto//proto:repositories.bzl",
     "rules_proto_dependencies", "rules_proto_toolchains")
rules_proto_dependencies()
rules_proto_toolchains()

load("//third_party/jvm:workspace.bzl", "maven_dependencies")
maven_dependencies()
