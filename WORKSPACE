workspace(name = "c4")

load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "jflex_rules",
    remote = "https://github.com/jflex-de/bazel_rules.git",
    branch = "stable",
)

git_repository(
    name = "io_bazel_rules_scala",
    remote= "https://github.com/bazelbuild/rules_scala.git",
    branch = "master",
)

git_repository(
    name = "bazel_skylib",
    remote= "https://github.com/bazelbuild/bazel-skylib.git",
    branch = "master",
)

git_repository(
    name = "io_bazel_rules_proto",
    remote= "https://github.com/bazelbuild/rules_proto.git",
    branch = "master",
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

load("//third_party:workspace.bzl", "maven_dependencies")
maven_dependencies()
