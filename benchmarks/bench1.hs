{-# LANGUAGE CPP #-}
module Main
       ( main -- :: IO ()
       ) where

import Criterion.Main
import Crypto.DH.Curve25519

import Control.DeepSeq
import qualified Data.ByteString as B

--------------------------------------------------------------------------------

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData B.ByteString
#endif

instance NFData SecretKey
instance NFData PublicKey

--------------------------------------------------------------------------------

main :: IO ()
main = do
  keys1@(p1,s2) <- createKeypair
  keys2@(p2,s1) <- createKeypair
  defaultMain
    [ bench "createKeypair" $ nfIO createKeypair
    , bench "curve25519"    $ nf (curve25519 s1) p1
    , bench "roundtrip"     $ nf (roundtrip keys1) keys2
    ]

roundtrip :: (PublicKey, SecretKey) -> (PublicKey, SecretKey) -> B.ByteString -> Bool
roundtrip (p1,s2) (p2,s1) xs = curve25519 s1 p1 == curve25519 s2 p2
