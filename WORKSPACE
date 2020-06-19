workspace(name = "c4")

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "jflex_rules",
    remote = "https://github.com/jflex-de/bazel_rules.git",
    commit = "b9a053fa6e461bdd131f0918966afba4f94993c7", # 03/28 2020
)

git_repository(
    name = "io_bazel_rules_scala",
    remote = "https://github.com/bazelbuild/rules_scala.git",
    commit = "056d5921d2c595e7ce2d54a627e8bc68ece7e28d", # 06/16 2020
)

git_repository(
    name = "io_bazel_rules_proto",
    remote = "https://github.com/bazelbuild/rules_proto.git",
    commit = "486aaf1808a15b87f1b6778be6d30a17a87e491a", # 06/03 2020
)

git_repository(
    name = "bazel_skylib",
    remote = "https://github.com/bazelbuild/bazel-skylib.git",
    commit = "3b666f525dfc645eea2bc4832796f8d6a07a997a", # 06/19 2020
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
