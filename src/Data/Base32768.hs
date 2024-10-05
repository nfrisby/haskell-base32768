{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

{- |
 
  Base32768 is a binary-to-text encoding optimised for UTF-16-encoded text.
  (e.g. Windows, Java, JavaScript)

-}

module Data.Base32768 (encode) where

import Data.Bits (testBit, unsafeShiftL)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as A
import Data.Text.Lazy.Builder qualified as T
import Data.Word (Word8)

-- TODO 'Text' is UTF-8, not UTF-16

encode :: ByteString -> T.Builder
encode bytes =
    B.foldr
        (flip $ foldBits $ \bit k acc -> popOrId (push bit acc) k)
        eof
        bytes
        (State 0 0)

-- | MSB first
foldBits :: (Bool -> a -> a) -> a -> Word8 -> a
foldBits cons nil byte =
    -- TODO some basic arithmetic would let us schedule the pop (dispatching to
    -- one of 15 branches) instead of dynamically checking after every bit
    f 7 $ f 6 $ f 5 $ f 4 $ f 3 $ f 2 $ f 1 $ f 0 $ nil
  where
    f i = cons (testBit byte i)

-----

-- @z@ and @numZBits@
--
-- The exact types don't matter very much here, but it's safe for the two to be
-- different types and it's convenient for @z :: Int@ to match the 'PrimArray'
-- interface.
--
-- @0 <= numZBits 15@, @0 <= z < 2^numZBits@
data State = State !Int !Word

popOrId :: State -> (State -> T.Builder) -> T.Builder
popOrId acc@(State z numZBits) k =
    if numZBits /= bITS_PER_CHAR then k acc else
    T.singleton (A.indexPrimArray fifteenE z) <> k (State 0 0)

push :: Bool -> State -> State
push b (State z numZBits) =
    State (unsafeShiftL z 1 + b') (numZBits + 1)
  where
    b' = if b then 1 else 0

-----

eof :: State -> T.Builder
eof (State z numZBits)
  | 0 == numZBits = mempty
  | numZBits <= 7 = pad sevenE    7
  | otherwise     = pad fifteenE 15
  where
    -- append 1s
    pad arr x =
        let n = fromIntegral $ x - numZBits
        in
            T.singleton
          $ A.indexPrimArray arr
          $ unsafeShiftL z n + 2^n - 1

-----

bITS_PER_CHAR :: Word
bITS_PER_CHAR = 15   -- Base32768 is a 15-bit encoding

repertoireE :: String -> PrimArray Char
repertoireE =
    A.primArrayFromList
  . concat
  . go
  where
    go = \case
        []      -> []
        [_]     -> error "impossible!"   -- even length by construction
        c1:c2:s ->
            -- 'Char' is very much the code point itself, so we don't need the
            -- explicit conversions that qntm needed within JavaScript.
            [c1 .. c2] : go s

fifteenE :: PrimArray Char
fifteenE = repertoireE "ҠҿԀԟڀڿݠޟ߀ߟကဟႠႿᄀᅟᆀᆟᇠሿበቿዠዿጠጿᎠᏟᐠᙟᚠᛟកសᠠᡟᣀᣟᦀᦟ᧠᧿ᨠᨿᯀᯟᰀᰟᴀᴟ⇠⇿⋀⋟⍀⏟␀␟─❟➀➿⠀⥿⦠⦿⨠⩟⪀⪿⫠⭟ⰀⰟⲀⳟⴀⴟⵀⵟ⺠⻟㇀㇟㐀䶟䷀龿ꀀꑿ꒠꒿ꔀꗿꙀꙟꚠꛟ꜀ꝟꞀꞟꡀꡟ"

sevenE :: PrimArray Char
sevenE = repertoireE "ƀƟɀʟ"
