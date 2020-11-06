module DynamicBuffer
  ( DBuffer
  , Offset

  , create
  , addInt8
  , addBuffer
  , setByte

  , fromUtf8

  , getBytes
  , getPosition

  , toString
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect, whileE)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

type Offset = Int

foreign import lengthImpl :: Uint8Array -> Int
foreign import setImpl :: EffectFn3 Uint8Array Offset Int Unit
foreign import setAllImpl :: EffectFn3 Uint8Array Offset Uint8Array Unit
foreign import allocate :: EffectFn1 Int Uint8Array
foreign import subarray :: EffectFn3 Uint8Array Offset Int Uint8Array
foreign import whenE :: EffectFn2 Boolean (Effect Unit) Unit
foreign import throwImpl :: String -> Effect Unit

newtype DBuffer = DBuffer
  { bytes :: Ref Uint8Array
  , position :: Ref Int
  , length :: Ref Int
  }

create :: Int -> Effect DBuffer
create n = do
  bytes' <- runEffectFn1 allocate n
  bytes <- Ref.new bytes'
  position <- Ref.new 0
  length <- Ref.new n
  pure (DBuffer { bytes, position, length })

resize :: DBuffer -> Int -> Effect Unit
resize (DBuffer buf) more = do
  oldLen <- Ref.read buf.length
  newLen <- Ref.new oldLen
  -- TODO: Check we're not overshooting 32bit
  whileE (map (_ < oldLen + more) (Ref.read newLen))
    (Ref.modify (_ * 2) newLen)
  newLen' <- Ref.read newLen
  newBytes <- runEffectFn1 allocate newLen'
  oldBytes <- Ref.read buf.bytes
  runEffectFn3 setAllImpl newBytes 0 oldBytes
  Ref.write newBytes buf.bytes
  Ref.write newLen' buf.length

addInt8 :: DBuffer -> Int -> Effect Unit
addInt8 b@(DBuffer buf) x = do
  runEffectFn2 whenE (x < 0 || x >= 256)
    (throwImpl ("addInt8: Out of range" <> show x))

  length <- Ref.read buf.length
  position <- Ref.read buf.position
  runEffectFn2 whenE (position >= length) (resize b 1)

  bytes <- Ref.read buf.bytes
  runEffectFn3 setImpl bytes position x
  Ref.write (position + 1) buf.position

addBuffer :: DBuffer -> DBuffer -> Effect Unit
addBuffer b@(DBuffer buf) x@(DBuffer xb) = do
  xbPosition <- Ref.read xb.position
  bufPosition <- Ref.read buf.position
  bufLen <- Ref.read buf.length
  let newPosition = bufPosition + xbPosition
  runEffectFn2 whenE (newPosition > bufLen) (resize b xbPosition)

  xbBytes <- Ref.read xb.bytes
  xbSub <- runEffectFn3 subarray xbBytes 0 xbPosition
  bytes <- Ref.read buf.bytes
  runEffectFn3 setAllImpl bytes bufPosition xbSub

  Ref.write newPosition buf.position

foreign import encode_utf8 :: EffectFn1 String Uint8Array

fromUtf8 :: String -> Effect DBuffer
fromUtf8 s = do
  bytes' <- runEffectFn1 encode_utf8 s
  let length' = lengthImpl bytes'
  bytes <- Ref.new bytes'
  length <- Ref.new length'
  position <- Ref.new length'
  pure (DBuffer { bytes, length, position })

getBytes :: DBuffer -> Effect Uint8Array
getBytes (DBuffer buf) = do
  bytes <- Ref.read buf.bytes
  position <- Ref.read buf.position
  runEffectFn3 subarray bytes 0 position

getPosition :: DBuffer -> Effect Int
getPosition (DBuffer { position }) = Ref.read position

setByte :: DBuffer -> Offset -> Int -> Effect Unit
setByte (DBuffer buf) offset x = do
  bytes <- Ref.read buf.bytes
  runEffectFn3 setImpl bytes offset x

foreign import toStringImpl :: Uint8Array -> String

toString :: DBuffer -> String
toString (DBuffer buf) = unsafePerformEffect do
  bytes <- Ref.read buf.bytes
  position <- Ref.read buf.position
  map toStringImpl (runEffectFn3 subarray bytes 0 position)
