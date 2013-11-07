{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Crypto.DH.Curve25519
-- Copyright   : (c) Austin Seipp 2013
-- License     : MIT
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides bindings to the curve25519 diffie-hellman
-- function. The underlying implementation uses the @ref@
-- implementation of curve25519 from SUPERCOP, and should be
-- relatively fast.
--
-- For more information visit <http://cr.yp.to/ecdh.html>
--
module Crypto.DH.Curve25519
       ( PublicKey(..)
       , SecretKey(..)
       , createKeypair -- :: IO (PublicKey, SecretKey)
       , curve25519    -- :: SecretKey -> PublicKey -> ByteString
       ) where
import           Foreign.C.Types
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr

import           System.IO.Unsafe         (unsafePerformIO)

import           Data.ByteString          as S
import           Data.ByteString.Internal as SI
import           Data.ByteString.Unsafe   as SU
import           Data.Word

--------------------------------------------------------------------------------

-- | A 'SecretKey' created by 'createKeypair'. Be sure to keep this
-- safe!
newtype SecretKey = SecretKey { unSecretKey :: ByteString }
        deriving (Eq, Show, Ord)

-- | A 'PublicKey' created by 'createKeypair'.
newtype PublicKey = PublicKey { unPublicKey :: ByteString }
        deriving (Eq, Show, Ord)

-- | Randomly generate a public and private key for doing
-- authenticated signing and verification.
createKeypair :: IO (PublicKey, SecretKey)
createKeypair = do
  pk <- SI.mallocByteString cryptoDhPUBLICKEYBYTES
  sk <- SI.mallocByteString cryptoDhSECRETKEYBYTES

  _ <- withForeignPtr pk $ \ppk -> do
    _ <- withForeignPtr sk $ \psk ->
      c_crypto_dh_keypair ppk psk >> return ()
    return ()

  return (PublicKey $ SI.fromForeignPtr pk 0 cryptoDhPUBLICKEYBYTES,
          SecretKey $ SI.fromForeignPtr sk 0 cryptoDhSECRETKEYBYTES)

curve25519 :: SecretKey -> PublicKey -> ByteString
curve25519 (SecretKey sk) (PublicKey pk) =
  unsafePerformIO . SU.unsafeUseAsCString sk $ \psk ->
    SU.unsafeUseAsCString pk $ \ppk ->
      SI.create cryptoDhBYTES $ \out ->
        c_crypto_dh out ppk psk >> return ()
{-# INLINE curve25519 #-}

--
-- FFI DH binding
--

cryptoDhPUBLICKEYBYTES :: Int
cryptoDhPUBLICKEYBYTES = 32

cryptoDhSECRETKEYBYTES :: Int
cryptoDhSECRETKEYBYTES = 32

cryptoDhBYTES :: Int
cryptoDhBYTES = 32

foreign import ccall unsafe "curve25519_dh_keypair"
  c_crypto_dh_keypair :: Ptr Word8 -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "curve25519_dh"
  c_crypto_dh :: Ptr Word8 -> Ptr CChar -> Ptr CChar -> IO CInt
