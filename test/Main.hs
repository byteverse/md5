{-# LANGUAGE BangPatterns #-}

import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Numeric (showHex)

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder.Bounded as BB
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Hash.Md5 as Md5

main :: IO ()
main = do
  putStrLn "Hashing: theoceanscovertheearth"
  putStr "Expected: "
  printHash expected
  putStr "Got:      "
  printHash actual
  if actual == expected
    then putStrLn "Success"
    else fail "Did not match"

printHash :: ByteArray -> IO ()
printHash !b = putStr (go 0)
 where
  go !ix =
    if ix < 16
      then
        let val = PM.indexByteArray b ix :: Word8
         in if val < 16
              then '0' : showHex val (go (ix + 1))
              else showHex val (go (ix + 1))
      else "\n"

actual :: ByteArray
actual =
  BB.run
    Nat.constant
    ( Md5.boundedBuilder
        ( Bytes.unsafeDrop
            5
            (Ascii.fromString "12345theoceanscovertheearth")
        )
    )

expected :: ByteArray
expected =
  Exts.fromList
    [ 0xf6
    , 0xc6
    , 0x90
    , 0x8c
    , 0x1d
    , 0xb5
    , 0x81
    , 0x4c
    , 0xa6
    , 0xfc
    , 0x9e
    , 0x59
    , 0x65
    , 0x8a
    , 0xf9
    , 0xd4
    ]
