{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

{- |
Module      : Hash.MD5.Internal
License     : BSD-3
-}
module Hash.Md5.Internal
  ( c_md5_init
  , c_md5_update_unsafe
  , c_md5_finalize
  ) where

import GHC.Exts (ByteArray#, MutableByteArray#, RealWorld)

-- MD5 Context
--
-- The context data is exactly 88 bytes long, however
-- the data in the context is stored depends on host-endianness.
--
-- The context data is made up of
--
--  * a 'Word64' representing the number of bytes already feed to hash algorithm so far,
--  * a 64-element 'Word8' buffer holding partial input-chunks, and finally
--  * a 4-element 'Word32' array holding the current work-in-progress digest-value.
--
-- Consequently, a MD5 digest as produced by 'finalize' is 16 bytes long.
foreign import capi unsafe "md5.h hs_byteverse_md5_init"
  c_md5_init ::
    MutableByteArray# RealWorld -> -- ctx, updated
    IO ()

foreign import capi unsafe "md5.h hs_byteverse_md5_update"
  c_md5_update_unsafe ::
    MutableByteArray# RealWorld -> -- ctx, updated
    ByteArray# -> -- bytes that are hashed
    Int -> -- offset into bytes, often zero
    Int -> -- length of bytes
    IO ()

foreign import capi unsafe "md5.h hs_byteverse_md5_finalize"
  c_md5_finalize ::
    MutableByteArray# RealWorld -> -- ctx
    MutableByteArray# RealWorld -> -- output buffer, must be 16 bytes
    Int -> -- output buffer offset
    IO ()
