{-# LANGUAGE DeriveDataTypeable #-}

module Burrito.Internal.Type.Digit where

import qualified Burrito.Internal.Type.Case as Case
import qualified Data.Bits as Bits
import qualified Data.Word as Word
import qualified Data.Data as Data

data Digit
  = Ox0
  | Ox1
  | Ox2
  | Ox3
  | Ox4
  | Ox5
  | Ox6
  | Ox7
  | Ox8
  | Ox9
  | OxA Case.Case
  | OxB Case.Case
  | OxC Case.Case
  | OxD Case.Case
  | OxE Case.Case
  | OxF Case.Case
  deriving (Data.Data, Eq, Show)

fromChar :: Char -> Maybe Digit
fromChar x = case x of
  '0' -> Just Ox0
  '1' -> Just Ox1
  '2' -> Just Ox2
  '3' -> Just Ox3
  '4' -> Just Ox4
  '5' -> Just Ox5
  '6' -> Just Ox6
  '7' -> Just Ox7
  '8' -> Just Ox8
  '9' -> Just Ox9
  'A' -> Just $ OxA Case.Upper
  'B' -> Just $ OxB Case.Upper
  'C' -> Just $ OxC Case.Upper
  'D' -> Just $ OxD Case.Upper
  'E' -> Just $ OxE Case.Upper
  'F' -> Just $ OxF Case.Upper
  'a' -> Just $ OxA Case.Lower
  'b' -> Just $ OxB Case.Lower
  'c' -> Just $ OxC Case.Lower
  'd' -> Just $ OxD Case.Lower
  'e' -> Just $ OxE Case.Lower
  'f' -> Just $ OxF Case.Lower
  _ -> Nothing

fromWord8 :: Word.Word8 -> (Digit, Digit)
fromWord8 x =
  let
    f y = case y of
      0x0 -> Ox0
      0x1 -> Ox1
      0x2 -> Ox2
      0x3 -> Ox3
      0x4 -> Ox4
      0x5 -> Ox5
      0x6 -> Ox6
      0x7 -> Ox7
      0x8 -> Ox8
      0x9 -> Ox9
      0xA -> OxA Case.Upper
      0xB -> OxB Case.Upper
      0xC -> OxC Case.Upper
      0xD -> OxD Case.Upper
      0xE -> OxE Case.Upper
      0xF -> OxF Case.Upper
      _ -> error $ "invalid nibble: " <> show y
  in (f $ Bits.shiftR x 4, f $ x Bits..&. 0x0F)

toWord8 :: Digit -> Digit -> Word.Word8
toWord8 x y =
  let
    f z = case z of
      Ox0 -> 0x0
      Ox1 -> 0x1
      Ox2 -> 0x2
      Ox3 -> 0x3
      Ox4 -> 0x4
      Ox5 -> 0x5
      Ox6 -> 0x6
      Ox7 -> 0x7
      Ox8 -> 0x8
      Ox9 -> 0x9
      OxA _ -> 0xA
      OxB _ -> 0xB
      OxC _ -> 0xC
      OxD _ -> 0xD
      OxE _ -> 0xE
      OxF _ -> 0xF
  in Bits.shiftL (f x) 4 Bits..|. f y
