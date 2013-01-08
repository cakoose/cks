module Data.Cks.BinaryWriter (
	BinaryWriter, ErrorMessage,
	writeBinary_String, writeBinary_Bool, writeBinary_Void,
	writeBinary_Int, writeBinary_Nat,
	writeBinary_Int64, writeBinary_Int32, writeBinary_Int16, writeBinary_Int8, 
	writeBinary_Nat64, writeBinary_Nat32, writeBinary_Nat16, writeBinary_Nat8, 
	writeBinary_List, writeBinary_Maybe, writeBinary_Set, writeBinary_Map,
	writeBinary_OptionIndex1, writeBinary_OptionIndex2, writeBinary_OptionIndex3, writeBinary_OptionIndex4,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Bits (Bits, shiftR, shiftL, (.|.), (.&.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.Set as Set
import Data.Word
import Data.Int
import Data.Char (ord, chr)
import Data.Cks.MaybeE

type ErrorMessage = String

type BinaryWriter a = a -> MaybeE ErrorMessage ByteString

--------------------------------------------------------

writeBinary_OptionIndex1 :: Int -> ByteString
writeBinary_OptionIndex1 n = B.singleton (getLowByte n)
writeBinary_OptionIndex2 :: Int -> ByteString
writeBinary_OptionIndex2 n = getBytes 2 n
writeBinary_OptionIndex3 :: Int -> ByteString
writeBinary_OptionIndex3 n = getBytes 3 n
writeBinary_OptionIndex4 :: Int -> ByteString
writeBinary_OptionIndex4 n = getBytes 4 n

writeBinary_String :: BinaryWriter String
writeBinary_String s = do
	encoded <- encodeString s
	Result $ B.append header encoded
	where
		header = writeBinary_FixedNatAsUnbound' (length s)

encodeString :: String -> MaybeE ErrorMessage ByteString
encodeString s = do
	bytes <- mapM encodeChar s
	Result $ B.concat bytes

encodeChar :: Char -> MaybeE ErrorMessage ByteString
encodeChar c | (c < (chr (1 `shiftL` 7))) = Result $ B.singleton (fromIntegral (ord c))
encodeChar c | (c < (chr (1 `shiftL` 14))) = Result $ B.pack [b1, b2]
	where
		i = ord c
		b1 = getByte i 8 .|. (1 `shiftL` 7)
		b2 = getByte i 0
encodeChar c | (c >= (chr 0xD800) && c <= (chr 0xDFFF)) = Error $ "invalid character: #" ++ (show $ ord c)
encodeChar c | (c < (chr 0x10FFFF)) = Result $ B.pack [b1, b2, b3]
	where
		i = ord c
		b1 = getByte i 16 .|. (3 `shiftL` 6)
		b2 = getByte i 8
		b3 = getByte i 0
encodeChar c = Error $ "invalid character: #" ++ (show $ ord c)

writeBinary_Int :: BinaryWriter Integer
writeBinary_Int n
	| n > 0  = Result $ writeBinary_IntPos n
	| n < 0  = Result $ writeBinary_IntNeg n
	| True   = Result $ B.singleton 0

writeBinary_NumberBytes bytes =
	if numBytes <= 18
		then B.cons (0xf0 .|. (getLowByte $ numBytes-4)) bytes
		else B.cons 0xff (B.append (writeBinary_FixedNatAsUnbound' (numBytes-19)) bytes)
	where
		numBytes = B.length bytes
		-- ASSERT(numBytes >= 3)

writeBinary_IntPos n =
	if numBits <= 7
		then B.singleton (B.index bytes 0)
		else if numBits <= 14
			then if numBits <= 8
				then B.pack [ (1 `shiftL` 7), (B.index bytes 0) ]
				else B.pack [ (1 `shiftL` 7) .|. (B.index bytes 0), (B.index bytes 1) ]
			else if numBits <= 21
				then if numBits <= 16
					then B.pack [ (3 `shiftL` 6), (B.index bytes 0), (B.index bytes 1) ]
					else B.pack [ (3 `shiftL` 6) .|. (B.index bytes 0), (B.index bytes 1), (B.index bytes 2) ]
				else if numBits <= 28
					then if numBits <= 24
						then B.pack [ (7 `shiftL` 5), (B.index bytes 0), (B.index bytes 1), (B.index bytes 2) ]
						else B.pack [ (7 `shiftL` 5) .|. (B.index bytes 0), (B.index bytes 1), (B.index bytes 2), (B.index bytes 3) ]
					else writeBinary_NumberBytes bytes
	where
		bytesPre = getBytesNat n
		first = B.head bytesPre  -- ASSERT(first /= 0)
		-- Find the first one.
		firstOne = mostSignificantOneBit first
		(bytes, numBits) = if firstOne == 7
			-- Need an extra leading zero.
			then (B.cons 0 bytesPre, ((B.length bytesPre) * 8) + 1)
			else (bytesPre, ((B.length bytesPre) * 8) - (6-firstOne))

writeBinary_IntNeg n =
	if numBits <= 7
		then B.singleton (0x7f .&. B.index bytes 0)
		else if numBits <= 14
			then if numBits <= 8
				then B.pack [ 0xbf, (B.index bytes 0) ]
				else B.pack [ 0xbf .&. (B.index bytes 0), (B.index bytes 1) ]
			else if numBits <= 21
				then if numBits <= 16
					then B.pack [ 0xdf, (B.index bytes 0), (B.index bytes 1) ]
					else B.pack [ 0xdf .&. (B.index bytes 0), (B.index bytes 1), (B.index bytes 2) ]
				else if numBits <= 28
					then if numBits <= 24
						then B.pack [ 0xef, (B.index bytes 0), (B.index bytes 1), (B.index bytes 2) ]
						else B.pack [ 0xef .&. (B.index bytes 0), (B.index bytes 1), (B.index bytes 2), (B.index bytes 3) ]
					else writeBinary_NumberBytes bytes
	where
		bytesPre = getBytesNeg n
		first = B.head bytesPre  -- ASSERT(first /= 0)
		-- Find the first one.
		firstZero = mostSignificantZeroBit first
		(bytes, numBits) = if firstZero == 7
			-- Need an extra leading one.
			then (B.cons 0xff bytesPre, ((B.length bytesPre) * 8) + 1)
			else (bytesPre, ((B.length bytesPre) * 8) - (6-firstZero))

mostSignificantOneBit :: Word8 -> Int
mostSignificantOneBit 0 = undefined
mostSignificantOneBit n = f n 7
	where
		f n pos = if (n .&. 0x80) /= 0 then pos else f (n `shiftL` 1) (pos-1)

mostSignificantZeroBit :: Word8 -> Int
mostSignificantZeroBit 0 = undefined
mostSignificantZeroBit n = f n 7
	where
		f n pos = if (n .&. 0x80) == 0 then pos else f (n `shiftL` 1) (pos-1)

writeBinary_Nat :: BinaryWriter Integer
writeBinary_Nat n = do
	ensureNat n
	Result $ if n == toInteger nInt
		then writeBinary_FixedNatAsUnbound' nInt 
		else writeBinary_NumberBytes (getBytesNat n)
	where
		nInt = (fromInteger n) :: Int

writeBinary_FixedNatAsUnbound' :: Int -> ByteString
writeBinary_FixedNatAsUnbound' n =
	if n < (1 `shiftL` 7)
		then B.singleton (getLowByte n)
		else if n < (1 `shiftL` 14)
			then B.pack [ (1 `shiftL` 7) .|. (getByte n 8), (getByte n 0) ]
			else if n < (1 `shiftL` 21)
				then B.pack [ (3 `shiftL` 6) .|. (getByte n 16), (getByte n 8), (getByte n 0) ]
				else if n < (1 `shiftL` 28)
					then B.pack [ (7 `shiftL` 5) .|. (getByte n 24), (getByte n 16), (getByte n 8), (getByte n 0) ]
					else
						let
							bytes = getBytesNat n
							numBytes = B.length bytes
							-- ASSERT(4 <= numBytes <= 18)
							header = 0xf0 .|. (getLowByte (numBytes-4))
						in
							B.cons header bytes

getLowByte :: (Integral a) => a -> Word8
getLowByte = fromInteger.toInteger

ensureNat :: (Integral a, Show a) => a -> MaybeE ErrorMessage ()
ensureNat n = if n >= 0 then Result () else Error $ "expecting non-negative value, found " ++ (show n)

getByte :: (Bits a, Integral a) => a -> Int -> Word8
getByte num bits = (getLowByte (num `shiftR` bits)) :: Word8

getBytesNat :: (Bits a, Integral a) => a -> ByteString
getBytesNat 0 = B.singleton 0
getBytesNat n = B.reverse $ B.unfoldr takeByte n
	where
		takeByte 0 = Nothing
		takeByte n = Just (getLowByte n, n `shiftR` 8)

getBytesNeg :: (Bits a, Integral a) => a -> ByteString
getBytesNeg (-1) = B.singleton 0xff
getBytesNeg n = B.reverse $ B.unfoldr takeByte n
	where
		takeByte (-1) = Nothing
		takeByte n = Just (getLowByte n, n `shiftR` 8)

getBytes :: (Bits a, Integral a) => Int -> a -> ByteString
getBytes numBytes n = f B.empty n numBytes
	where
		f l _ 0 = l
		f l n remaining = f (B.cons (getLowByte n) l) (n `shiftR` 8) (remaining-1)

writeBinary_Nat64 :: BinaryWriter Word64
writeBinary_Nat64 n = Result $ getBytes 8 n
writeBinary_Nat32 :: BinaryWriter Word32
writeBinary_Nat32 n = Result $ getBytes 4 n
writeBinary_Nat16 :: BinaryWriter Word16
writeBinary_Nat16 n = Result $ getBytes 2 n
writeBinary_Nat8 :: BinaryWriter Word8
writeBinary_Nat8 n = Result $ B.singleton (getLowByte n)

writeBinary_Int64 :: BinaryWriter Int64
writeBinary_Int64 n = Result $ getBytes 8 n
writeBinary_Int32 :: BinaryWriter Int32
writeBinary_Int32 n = Result $ getBytes 4 n
writeBinary_Int16 :: BinaryWriter Int16
writeBinary_Int16 n = Result $ getBytes 2 n
writeBinary_Int8 :: BinaryWriter Int8
writeBinary_Int8 n = Result $ B.singleton (getLowByte n)

writeBinary_Bool :: BinaryWriter Bool
writeBinary_Bool b = Result $ B.singleton (if b then 1 else 0)

writeBinary_Void :: BinaryWriter ()
writeBinary_Void _ = Result $ B.empty

--------------------------------------------------------

writeBinary_List :: BinaryWriter e -> BinaryWriter [e]
writeBinary_List mA l = do
	contents <- mapM mA l
	Result $ B.concat (len:contents)
	where
		len = writeBinary_FixedNatAsUnbound' (length l)

writeBinary_Set :: BinaryWriter e -> BinaryWriter (Set e)
writeBinary_Set mA set = do
	contents <- mapM mA (Set.toList set)
	Result $ B.concat (len:contents)
	where
		len = writeBinary_FixedNatAsUnbound' (Set.size set)

writeBinary_Map :: BinaryWriter k -> BinaryWriter v -> BinaryWriter (Map k v)
writeBinary_Map mK mV map = do
	contents <- mapM stringify (Map.toList map)
	Result $ B.concat (len:contents)
	where
		len = writeBinary_FixedNatAsUnbound' (Map.size map)
		stringify (k, v) = do
			k' <- mK k
			v' <- mV v
			Result $ B.append k' v'

writeBinary_Maybe :: BinaryWriter e -> BinaryWriter (Maybe e)
writeBinary_Maybe mA v = case v of
	Just e -> do
		e' <- mA e
		Result $ B.cons 1 e'
	Nothing -> Result $ B.singleton 0
