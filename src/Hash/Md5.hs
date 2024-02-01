{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Hash.Md5
  ( Context (..)

    -- * Context Reuse
  , context
  , reinitialize
  , update
  , finalize

    -- * One Shot
  , boundedBuilder
  ) where

import Control.Monad.ST (ST)
import Data.Bytes.Builder.Bounded as BB
import Data.Bytes.Builder.Bounded.Unsafe as BBU
import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (..), MutableByteArray (..), newByteArray)
import GHC.Exts (unsafeCoerce#)
import GHC.IO (unsafeIOToST)

import Hash.Md5.Internal

newtype Context s = Context (MutableByteArray s)

-- | Create a new context. The context is initialized.
context :: ST s (Context s)
context = do
  b <- newByteArray 88
  reinitialize (Context b)
  pure (Context b)

-- | Reset the context so that it may be used to hash another byte sequence.
reinitialize :: Context s -> ST s ()
reinitialize (Context (MutableByteArray ctx)) =
  unsafeIOToST (c_md5_init (unsafeCoerce# ctx))

finalize ::
  Context s ->
  -- | Destination, implied length is 16
  MutableByteArray s ->
  -- | Destination offset
  Int ->
  ST s ()
finalize (Context (MutableByteArray ctx)) (MutableByteArray x) !a =
  unsafeIOToST (c_md5_finalize (unsafeCoerce# ctx) (unsafeCoerce# x) a)

update ::
  Context s ->
  Bytes ->
  ST s ()
update (Context (MutableByteArray ctx)) (Bytes (ByteArray arr) off len) =
  unsafeIOToST (c_md5_update_unsafe (unsafeCoerce# ctx) arr off len)

performHash :: MutableByteArray s -> Int -> ByteArray -> Int -> Int -> ST s ()
performHash (MutableByteArray x) !a (ByteArray y) !b !c =
  unsafeIOToST $ do
    MutableByteArray ctx <- newByteArray 88
    c_md5_init ctx
    c_md5_update_unsafe ctx y b c
    c_md5_finalize ctx (unsafeCoerce# x) a

boundedBuilder :: Bytes -> BB.Builder 16
boundedBuilder (Bytes arr off len) =
  BBU.construct
    ( \buf ix -> do
        performHash buf ix arr off len
        pure (ix + 16)
    )
