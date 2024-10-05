{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Base32768 (encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.IO qualified as T
import System.Environment (getArgs)
import System.Random
import Text.Read (readMaybe)

main :: IO ()
main = do
    n <- fmap (map readMaybe) getArgs >>= \case
      [Just n] | 0 <= n -> pure (n :: Int)
      _                 ->
          fail "expecting exactly one argument, a non-negative interger"

    putStr "const testCases = ["

    _ <- (\f -> foldl f newStdGen [1 .. n]) $ \mg _i -> mg >>= \g0 -> do
        let (len  , g1) = genWord8 g0
            (bytes, g2) = genByteString (fromIntegral len) g1
        putStr "["
        putStr $ show $ B.unpack bytes
        putStr ",\""
        T.putStr $ toLazyText $ encode $ BL.fromStrict bytes
        putStr "\"],"
        pure g2
      
    putStrLn "]"
