module DynamicBuffer
  ( DBuffer
  , Offset

  , create
  , addByte
  , addBuffer
  , setByte

  , fromUtf8

  , contents
  , unsafeContents
  , size

  , debugToString
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect, whileE)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)


-- | A byte-offset into a Buffer. Just an alias for documentation
-- | purposes.
type Offset = Int

-- | A growable Buffer type for binary data
newtype DBuffer = DBuffer
  { -- The underlying storage
    bytes :: Ref Uint8Array
    -- From the outside this is the observable `size` of this buffer
  , position :: Ref Int
    -- The capacity of the current storage.
  , capacity :: Ref Int
  }

-- | Creates a new `DBuffer` with the given initial capacity.
-- |
-- | Picking a capacity in the order of magnitude you expect to write
-- | will reduce the overall amount of allocation.
create :: Int -> Effect DBuffer
create n = do
  bytes' <- runEffectFn1 allocate n
  bytes <- Ref.new bytes'
  position <- Ref.new 0
  capacity <- Ref.new n
  pure (DBuffer { bytes, position, capacity })

-- | Writes a single byte into the DBuffer
-- |
-- | Throws an exception if you pass a value that's not within
-- | (0..255)
addByte :: DBuffer -> Int -> Effect Unit
addByte b@(DBuffer buf) x = do
  runEffectFn2 whenE (x < 0 || x >= 256)
    (throwImpl ("addByte: Out of range " <> show x))

  capacity <- Ref.read buf.capacity
  position <- Ref.read buf.position
  runEffectFn2 whenE (position >= capacity) (resize b 1)

  bytes <- Ref.read buf.bytes
  runEffectFn3 setImpl bytes position x
  Ref.write (position + 1) buf.position

-- | Adds the contents of the second buffer to the first. The contents
-- | are copied, so subsequent modifications to the second buffer
-- | don't affect the first.
-- |
-- | ```purescript
-- | import Prelude
-- |
-- | import DynamicBuffer as DBuffer
-- | import Debug.Trace as Debug
-- |
-- | main = do
-- |   b1 <- DBuffer.create 8
-- |   b2 <- DBuffer.create 8
-- |   DBuffer.addByte b1 10
-- |   DBuffer.addByte b2 20
-- |   DBuffer.addByte b2 30
-- |   DBuffer.addBuffer b1 b2
-- |
-- |   Debug.traceM =<< DBuffer.size b1 -- 3
-- |   Debug.traceM =<< DBuffer.contents b1 -- Uint8Array(3) [ 10, 20, 30 ]
-- | ```
addBuffer :: DBuffer -> DBuffer -> Effect Unit
addBuffer b@(DBuffer buf) x@(DBuffer xb) = do
  xbPosition <- Ref.read xb.position
  bufPosition <- Ref.read buf.position
  bufLen <- Ref.read buf.capacity
  let newPosition = bufPosition + xbPosition
  runEffectFn2 whenE (newPosition > bufLen) (resize b xbPosition)

  xbBytes <- Ref.read xb.bytes
  xbSub <- runEffectFn3 subarray xbBytes 0 xbPosition
  bytes <- Ref.read buf.bytes
  runEffectFn3 setAllImpl bytes bufPosition xbSub

  Ref.write newPosition buf.position

-- | Creates a DBuffer that contains the Utf8 encoding of the given
-- | String.
fromUtf8 :: String -> Effect DBuffer
fromUtf8 s = do
  bytes' <- runEffectFn1 encodeUtf8 s
  let capacity' = lengthImpl bytes'
  bytes <- Ref.new bytes'
  capacity <- Ref.new capacity'
  position <- Ref.new capacity'
  pure (DBuffer { bytes, capacity, position })

-- | Extracts the contents of this DBuffer
contents :: DBuffer -> Effect Uint8Array
contents b@(DBuffer buf) = do
  position <- Ref.read buf.position
  result <- runEffectFn1 allocate position
  bytes <- unsafeContents b
  runEffectFn3 setAllImpl result 0 bytes
  pure result

-- | Extracts the contents of this DBuffer without copying.
-- |
-- | Careful! Modifying the contents of the DBuffer afterwards will
-- | modify the returned Uint8Array in place
unsafeContents :: DBuffer -> Effect Uint8Array
unsafeContents (DBuffer buf) = do
  bytes <- Ref.read buf.bytes
  position <- Ref.read buf.position
  runEffectFn3 subarray bytes 0 position

-- | Returns the current size of the DBuffer. _Not_ its capacity.
size :: DBuffer -> Effect Int
size (DBuffer { position }) = Ref.read position

-- | Mutates the buffer at the given offset.
-- |
-- | Throws an exception if you pass an offset that's not within the
-- | current Buffer's size. _Not capacity_.
-- |
-- | Throws an exception if you pass a value that's not within
-- | (0..255)
setByte :: DBuffer -> Offset -> Int -> Effect Unit
setByte b@(DBuffer buf) offset x = do
  runEffectFn2 whenE (x < 0 || x >= 256)
    (throwImpl ("setByte: Out of range " <> show x))
  size' <- size b
  runEffectFn2 whenE (offset >= size')
    (throwImpl ("setByte: Offset out of bounds " <> show offset <> ", size is " <> show size'))

  bytes <- Ref.read buf.bytes
  runEffectFn3 setImpl bytes offset x

-- | Displays the contents of the DBuffer for debugging purposes.
-- | Don't use this in "production", it's not referentially
-- | transparent.
-- |
-- | ```purescript
-- | import Prelude
-- |
-- | import DynamicBuffer as DBuffer
-- | import Effect.Console as Console
-- |
-- | main = do
-- |   b <- DBuffer.create 8
-- |   DBuffer.addByte b 10
-- |   DBuffer.addByte b 20
-- |   DBuffer.addByte b 30
-- |
-- |   Console.log (DBuffer.debugToString b) -- [0xA, 0x14, 0x1E]
-- | ```
debugToString :: DBuffer -> String
debugToString (DBuffer buf) = unsafePerformEffect do
  bytes <- Ref.read buf.bytes
  position <- Ref.read buf.position
  map toStringImpl (runEffectFn3 subarray bytes 0 position)

foreign import allocate :: EffectFn1 Int Uint8Array
foreign import encodeUtf8 :: EffectFn1 String Uint8Array
foreign import lengthImpl :: Uint8Array -> Int
foreign import setAllImpl :: EffectFn3 Uint8Array Offset Uint8Array Unit
foreign import setImpl :: EffectFn3 Uint8Array Offset Int Unit
foreign import subarray :: EffectFn3 Uint8Array Offset Int Uint8Array
foreign import throwImpl :: String -> Effect Unit
foreign import toStringImpl :: Uint8Array -> String
foreign import whenE :: EffectFn2 Boolean (Effect Unit) Unit

resize :: DBuffer -> Int -> Effect Unit
resize (DBuffer buf) more = do
  oldLen <- Ref.read buf.capacity
  newLen <- Ref.new oldLen
  -- TODO: Check we're not overshooting 32bit
  whileE (map (_ < oldLen + more) (Ref.read newLen))
    (Ref.modify (_ * 2) newLen)
  newLen' <- Ref.read newLen
  newBytes <- runEffectFn1 allocate newLen'
  oldBytes <- Ref.read buf.bytes
  runEffectFn3 setAllImpl newBytes 0 oldBytes
  Ref.write newBytes buf.bytes
  Ref.write newLen' buf.capacity
