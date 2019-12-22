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

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")
scala_register_toolchains()

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()

load("//third_party:workspace.bzl", "maven_dependencies")
maven_dependencies()
