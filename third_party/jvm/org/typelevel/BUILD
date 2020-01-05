load("@io_bazel_rules_scala//scala:scala_import.bzl", "scala_import")
load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library")
scala_import(
    name = "cats_core",
    exports = [
        "//third_party/jvm/org/scala_lang:scala_library",
        ":cats_kernel",
        ":cats_macros",
        ":machinist"
    ],
    jars = [
        "//external:jar/org/typelevel/cats_core_2_11"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_free",
    exports = [
        "//third_party/jvm/org/scala_lang:scala_library",
        ":cats_core",
        ":cats_macros",
        ":machinist"
    ],
    jars = [
        "//external:jar/org/typelevel/cats_free_2_11"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_kernel",
    exports = [
        "//third_party/jvm/org/scala_lang:scala_library"
    ],
    jars = [
        "//external:jar/org/typelevel/cats_kernel_2_11"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "cats_macros",
    exports = [
        "//third_party/jvm/org/scala_lang:scala_library",
        ":machinist"
    ],
    jars = [
        "//external:jar/org/typelevel/cats_macros_2_11"
    ],
    visibility = [
        "//visibility:public"
    ]
)



scala_import(
    name = "machinist",
    exports = [
        "//third_party/jvm/org/scala_lang:scala_library",
        "//third_party/jvm/org/scala_lang:scala_reflect"
    ],
    jars = [
        "//external:jar/org/typelevel/machinist_2_11"
    ],
    visibility = [
        "//visibility:public"
    ]
)


