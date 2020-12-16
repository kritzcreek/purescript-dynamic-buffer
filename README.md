# purescript-dynamic-buffer

Growable buffers

Can be used to build up binary data. Inspired by OCaml's buffer type.

This is a small, minimal library, with very few dependencies, which
should be reasonably fast.

If you're looking for a general purpose library and don't mind a few
extra dependencies you should first take a look at
[`arraybuffer-builder`](https://github.com/jamesdbrock/purescript-arraybuffer-builder).

## Examples

```purescript
module Main where

import Prelude

import Data.Array as Array
import Data.Traversable (for_)
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Console as Console

main :: Effect Unit
main = do
  buf <- DBuffer.create 8
  for_ (Array.range 1 10) do
    DBuffer.addByte buf
  Console.log (DBuffer.debugToString buf)
  -- [0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA]
```

For a more comprehensive usage check out my [encoder for the WebAssembly binary format](https://github.com/kritzcreek/purescript-wasm/blob/main/src/Wasm/Encode.purs).

More examples can also be found in the [testsuite](./test/Main.purs)


## License
Copyright 2020 Christoph Hegemann

This software is subject to the terms of the Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed with this file, You can obtain one at http://mozilla.org/MPL/2.0/.
