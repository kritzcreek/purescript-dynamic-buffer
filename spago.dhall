{ name = "dynamic-buffers"
, license = "MPL-2.0"
, repository = "https://github.com/kritzcreek/purescript-dynamic-buffer.git"
, dependencies = [ "arraybuffer-types", "effect", "prelude", "refs" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
