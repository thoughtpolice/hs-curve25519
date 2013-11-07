{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Main
       ( main  -- :: IO ()
       ) where
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as S

import           Crypto.DH.Curve25519

import           System.Environment       (getArgs)
import           Test.QuickCheck
import           Test.QuickCheck.Property (morallyDubiousIOProperty)
import           Text.Printf

--------------------------------------------------------------------------------
-- Orphans

instance Arbitrary ByteString where
  arbitrary = S.pack `liftM` arbitrary

instance Arbitrary SecretKey where
  arbitrary = SecretKey `liftM` arbitrary

instance Arbitrary PublicKey where
  arbitrary = PublicKey `liftM` arbitrary

--------------------------------------------------------------------------------
-- Tests

type KP = (PublicKey, SecretKey)

keypairProp :: (KP -> KP -> Bool) -> Property
keypairProp k = morallyDubiousIOProperty $
                k `liftM` createKeypair `ap` createKeypair

roundtrip :: ByteString -> Property
roundtrip xs
  = keypairProp $ \(p1,s2) (p2,s1) ->
      curve25519 s1 p1 == curve25519 s2 p2

--------------------------------------------------------------------------------
-- Driver

main :: IO ()
main = do
  args <- fmap (drop 1) getArgs
  let n = if null args then 100 else read (head args) :: Int
  (results, passed) <- runTests n
  printf "Passed %d tests!\n" (sum passed)
  unless (and results) (fail "Not all tests passed!")

runTests :: Int -> IO ([Bool], [Int])
runTests ntests = fmap unzip . forM (tests ntests) $ \(s, a) ->
  printf "%-40s: " s >> a

tests :: Int -> [(String, IO (Bool,Int))]
tests ntests =
  [ ("curve25519 roundtrip",            wrap roundtrip)
  ]
  where
    wrap :: Testable prop => prop -> IO (Bool, Int)
    wrap prop = do
      r <- quickCheckWithResult stdArgs{maxSize=ntests} prop
      case r of
        Success n _ _           -> return (True, n)
        GaveUp  n _ _           -> return (True, n)
#if MIN_VERSION_QuickCheck(2,6,0)
        Failure n _ _ _ _ _ _ _ -> return (False, n)
#else
        Failure n _ _ _ _ _ _   -> return (False, n)
#endif
        _                       -> return (False, 0)
