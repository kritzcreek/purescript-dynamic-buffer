{ name = "dynamic-buffers"
, dependencies =
  [ "arraybuffer-types", "effect", "refs" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
