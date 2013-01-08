module Data.Cks.BinaryReader (
	BinaryReader, ErrorMessage, run, runPartial,
	readBinary_String, readBinary_Bool, readBinary_Void,
	readBinary_List, readBinary_Maybe, readBinary_Set, readBinary_Map,
	readBinary_Int, readBinary_Nat,
	readBinary_Int64, readBinary_Int32, readBinary_Int16, readBinary_Int8, 
	readBinary_Nat64, readBinary_Nat32, readBinary_Nat16, readBinary_Nat8, 
	readBinary_OptionIndex1, readBinary_OptionIndex2, readBinary_OptionIndex3, readBinary_OptionIndex4,
	readBinary_Variant,
	(<?>),
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Control.Applicative (Applicative(..))
import Control.Monad (foldM, when, liftM, ap)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.Array (Array, (!))

import Data.Word
import Data.Int
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Char (chr)
import Data.Cks.MaybeE

type ErrorMessage = String

newtype BinaryReader a = BinaryReader (ByteString -> MaybeE String (a, ByteString))

{-|
	Run the given binary reader on the given input string.
	If the input is malformed, the return value will be an error message string.
	If the input is valid, the return value will be the parsed value.

	If there are bytes left over after the value has been parsed, this is
	considered to be malformed input.  If you are OK with having bytes left
	over, use the 'runPartial' function instead.
-}
run :: BinaryReader a -> ByteString -> MaybeE String a
run (BinaryReader br) input = case br input of
	Error err -> Error err
	Result (value, rest) -> if B.null rest
		then Result value
		else Error "encountered extra data (after the value)"

{-|
	Run the given binary reader on the given input string.
	If the input is malformed, the return value will be an error message string.
	If the input is valid, the return value will be the parsed value along
	with whatever remains of the input.
-}
runPartial :: BinaryReader a -> ByteString -> MaybeE String (a, ByteString)
runPartial (BinaryReader br) = br

instance Monad BinaryReader where
	(>>=) (BinaryReader f) gb =
		BinaryReader $ \input -> case f input of
			Error err -> Error err
			Result (a, rest) ->
				let (BinaryReader g) = gb a
				in g rest
	(>>) (BinaryReader f) (BinaryReader g) =
		BinaryReader $ \input -> case f input of
			Error err -> Error err
			Result (_, rest) -> g rest
	return a = BinaryReader $ \input -> Result (a, input)
	--fail s = throw s

-- Future-proofing for the Applicative-Monad merge in GHC 7.10.
instance Functor BinaryReader where
    fmap = liftM
instance Applicative BinaryReader where
    pure = return
    (<*>) = ap

brError :: String -> BinaryReader a
brError error = BinaryReader $ \_ -> Error error

-- Read a single byte.
brRead :: BinaryReader Word8
brRead = BinaryReader $ \input ->
	if B.null input
		then Error "unexpected end of input"
		else Result (B.head input, B.tail input)

-- Read some number of bytes.
brReadN :: Int -> BinaryReader ByteString
brReadN n = BinaryReader $ \input ->
	if B.length input < n
		then Error $ "unexpected end of input (needed "++(show n)++" bytes, found "++(show $ B.length input)++" bytes)"
		else Result $ B.splitAt n input

-- Add in a textual context, such as "list item 3", for error messages.
brContext :: String -> BinaryReader a -> BinaryReader a
brContext c (BinaryReader br) = BinaryReader $ \input ->
	case br input of
		Error err -> Error $ err ++ ", at " ++ c
		Result (a, rest) -> Result (a, rest)

(<?>) :: BinaryReader a -> String -> BinaryReader a
(<?>) = flip brContext

------------------------------------------------------------
-- Nat

readBinary_Nat8 :: BinaryReader Word8
readBinary_Nat8 = brRead <?> "Nat8"

readBinary_Nat16 :: BinaryReader Word16
readBinary_Nat16 = do
	a <- brRead <?> "first byte of Nat16"
	b <- brRead <?> "second byte of Nat16"
	return $ mkNat16 a b

readBinary_Nat32 :: BinaryReader Word32
readBinary_Nat32 = do
	a <- brRead <?> "first byte of Nat32"
	b <- brRead <?> "second byte of Nat32"
	c <- brRead <?> "third byte of Nat32"
	d <- brRead <?> "fourth byte of Nat32"
	return $ mkNat32 a b c d

readBinary_Nat64 :: BinaryReader Word64
readBinary_Nat64 = do
	a <- brRead <?> "first byte of Nat64"
	b <- brRead <?> "second byte of Nat64"
	c <- brRead <?> "third byte of Nat64"
	d <- brRead <?> "fourth byte of Nat64"
	e <- brRead <?> "fifth byte of Nat64"
	f <- brRead <?> "sixth byte of Nat64"
	g <- brRead <?> "seventh byte of Nat64"
	h <- brRead <?> "eighth byte of Nat64"
	return $ mkNat64 a b c d e f g h

------------------------------------------------------------
-- Int

readBinary_Int8 :: BinaryReader Int8
readBinary_Int8 = do
	b <- brRead <?> "Int8"
	return $ mkInt8 b

readBinary_Int16 :: BinaryReader Int16
readBinary_Int16 = do
	a <- brRead <?> "first byte of Int16"
	b <- brRead <?> "second byte of Int16"
	return $ mkInt16 a b

readBinary_Int32 :: BinaryReader Int32
readBinary_Int32 = do
	a <- brRead <?> "first byte of Int32"
	b <- brRead <?> "second byte of Int32"
	c <- brRead <?> "third byte of Int32"
	d <- brRead <?> "fourth byte of Int32"
	return $ mkInt32 a b c d

readBinary_Int64 :: BinaryReader Int64
readBinary_Int64 = do
	a <- brRead <?> "first byte of Int64"
	b <- brRead <?> "second byte of Int64"
	c <- brRead <?> "third byte of Int64"
	d <- brRead <?> "fourth byte of Int64"
	e <- brRead <?> "fifth byte of Int64"
	f <- brRead <?> "sixth byte of Int64"
	g <- brRead <?> "seventh byte of Int64"
	h <- brRead <?> "eighth byte of Int64"
	return $ mkInt64 a b c d e f g h

------------------------------------------------------------

mkNat16 a b = (a' `shiftL` 8) .|. b'
	where
		a' = fromIntegral a :: Word16
		b' = fromIntegral b :: Word16

mkNat32 a b c d = (a' `shiftL` 24) .|. (b' `shiftL` 16) .|. (c' `shiftL` 8) .|. d'
	where
		a' = fromIntegral a :: Word32
		b' = fromIntegral b :: Word32
		c' = fromIntegral c :: Word32
		d' = fromIntegral d :: Word32

mkNat64 a b c d e f g h =
		(a' `shiftL` 56) .|.
		(b' `shiftL` 48) .|.
		(c' `shiftL` 40) .|.
		(d' `shiftL` 32) .|.
		(e' `shiftL` 24) .|.
		(f' `shiftL` 16) .|.
		(g' `shiftL` 8) .|.
		h'
	where
		a' = fromIntegral a :: Word64
		b' = fromIntegral b :: Word64
		c' = fromIntegral c :: Word64
		d' = fromIntegral d :: Word64
		e' = fromIntegral e :: Word64
		f' = fromIntegral f :: Word64
		g' = fromIntegral g :: Word64
		h' = fromIntegral h :: Word64

mkInt8 a = (fromIntegral a) :: Int8

mkInt16 a b = (fromIntegral $ mkNat16 a b) :: Int16

mkInt32 a b c d = (fromIntegral $ mkNat32 a b c d) :: Int32

mkInt64 a b c d e f g h = (fromIntegral $ mkNat64 a b c d e f g h) :: Int64

------------------------------------------------------------
-- Unbound

readBinary_NatLength :: BinaryReader Int
readBinary_NatLength = do
	x <- readBinary_Nat
	if x > toInteger maxInt
		then brError $ "length value too large: " ++ show x
		else return $ fromInteger x
	where
		maxInt :: Int
		maxInt = maxBound

readBinary_Nat :: BinaryReader Integer
readBinary_Nat = do
	a <- brRead <?> "first byte of Nat"
	if (a .&. 0x80) == 0
		then return $ toInteger a
		else if (a .&. 0x40) == 0
			then do
				b <- brRead <?> "second byte of 2-byte Nat"
				return $ toInteger $ mkNat16 (a .&. 0x3f) b
			else if (a .&. 0x20) == 0
				then do
					b <- brRead <?> "second byte of 3-byte Nat"
					c <- brRead <?> "third byte of 3-byte Nat"
					return $ toInteger $ mkNat32 0 (a .&. 0x1f) b c
				else if (a .&. 0x10) == 0
					then do
						b <- brRead <?> "second byte of 4-byte Nat"
						c <- brRead <?> "third byte of 4-byte Nat"
						d <- brRead <?> "fourth byte of 4-byte Nat"
						return $ toInteger $ mkNat32 (a .&. 0xf) b c d
					else do
						let header = a .&. 0x0f
						numBytes <- do
							if header == 0x0f
								then do
									l <- readBinary_NatLength
									return $ l + 19
								else return $ fromIntegral $ header + 4
						bits <- brReadN numBytes <?> (show numBytes++"-byte payload of Int")
						return $ byteStringToNat bits

readBinary_Int :: BinaryReader Integer
readBinary_Int = do
	a <- brRead <?> "first byte of Int"
	if (a .&. 0x80) == 0
		then do
			-- [0] {7 bits}
			let a' = if (a .&. 0x40) == 0 then a else a .|. (1 `shiftL` 7)  -- sign extend
			return $ toInteger $ mkInt8 a'
		else if (a .&. 0x40) == 0
			then do
				-- [10] {14 bits}
				b <- brRead <?> "second byte of 2-byte Int"
				let a' = if (a .&. 0x20) == 0 then a .&. 0x3f else a .|. (1 `shiftL` 6)  -- sign extend
				return $ toInteger $ mkInt16 a' b
			else if (a .&. 0x20) == 0
				then do
					-- [110] {21 bits}
					b <- brRead <?> "second byte of 3-byte Int"
					c <- brRead <?> "third byte of 3-byte Int"
					let (pre, a') = if (a .&. 0x10) == 0
						then (0x00, a .&. 0x1f)
						else (0xff, a .|. (1 `shiftL` 5))  -- sign extend
					return $ toInteger $ mkInt32 pre a' b c
				else if (a .&. 0x10) == 0
					then do
						-- [1110] {28 bits}
						b <- brRead <?> "second byte of 4-byte Int"
						c <- brRead <?> "third byte of 4-byte Int"
						d <- brRead <?> "fourth byte of 4-byte Int"
						let a' = if (a .&. 0x8) == 0 then a .&. 0x0f else a .|. (1 `shiftL` 4)  -- sign extend
						return $ toInteger $ mkInt32 a' b c d
					else do
						let header = a .&. 0x0f
						numBytes <- do
							if header == 0x0f
								then do
									l <- readBinary_NatLength
									return $ l + 19
								else return $ fromIntegral $ header + 4
						bits <- brReadN numBytes <?> (show numBytes++"-byte payload of Int")
						return $ byteStringToInt (B.head bits) (B.tail bits)

byteStringToNat :: ByteString -> Integer
byteStringToNat bs = B.foldl shiftAndAdd 0 bs
	where
		shiftAndAdd value digit = (value `shiftL` 8) .|. toInteger digit

byteStringToInt :: Word8 -> ByteString -> Integer
byteStringToInt head tail = B.foldl shiftAndAdd start tail
	where
		start = toInteger ((fromIntegral head) :: Int8)
		shiftAndAdd value digit = (value `shiftL` 8) .|. toInteger  digit

------------------------------------------------------------

readBinary_List :: BinaryReader a -> BinaryReader [a]
readBinary_List mA = do
	l <- readBinary_NatLength <?> "length of list"
	mapM elementReader [1..l]
	where
		elementReader i = mA <?> ("list element " ++ show i)

------------------------------------------------------------

readBinary_Set :: Ord a => BinaryReader a -> BinaryReader (Set a)
readBinary_Set mA = do
	l <- readBinary_NatLength <?> "size of set"
	foldM elementReader Set.empty [1..l]
	where
		elementReader s i = do
			e <- mA <?> ("set element " ++ show i)
			if Set.member e s
				then brError $ "duplicate set element at entry " ++ show i
				else return $ Set.insert e s

------------------------------------------------------------

readBinary_Map :: Ord k => BinaryReader k -> BinaryReader v -> BinaryReader (Map k v)
readBinary_Map mK mV = do
	l <- readBinary_NatLength <?> "size of map"
	foldM elementReader Map.empty [1..l]
	where
		elementReader m i = do
			k <- mK <?> ("map key " ++ show i)
			v <- mV <?> ("map value " ++ show i)
			if Map.member k m
				then brError $ "duplicate map key at entry " ++ show i
				else return $ Map.insert k v m

------------------------------------------------------------

readBinary_Maybe :: BinaryReader a -> BinaryReader (Maybe a)
readBinary_Maybe mA = do
	b <- brRead <?> "tag of optional value"
	case b of
		0 -> return $ Nothing
		1 -> do
			v <- mA <?> "optional value: \"Set\" option"
			return $ Just v
		_ -> brError $ "invalid tag for optional value: " ++ (show b)

------------------------------------------------------------

readBinary_String :: BinaryReader String
readBinary_String = do
	l <- readBinary_NatLength <?> "length of string"
	let readers = take l (repeat readBinaryChar)
	sequence readers

readBinaryChar :: BinaryReader Char
readBinaryChar = do
	b <- brRead
	case (b `shiftR` 6) of
		0 -> return $ chr (fromIntegral b)
		1 -> return $ chr (fromIntegral b)
		2 -> do
			let b1 = (b .&. 0x3f)
			b2 <- brRead
			return $ chr.fromIntegral $ mkInt16 b1 b2
		3 -> do
			let b1 = (b .&. 0x1f)
			b2 <- brRead
			b3 <- brRead
			let i = mkInt32 0 b1 b2 b3
			when (i > 0x10FFFF) $ brError $ "character value out of range: " ++ (show i)
			when (i >= 0xD800 && i <= 0xDFFF) $ brError $ "character value out of range: " ++ (show i) ++ " (characters in the UTF-16 surrogate range are not allowed)"
			return $ chr ((fromIntegral i) :: Int)
		x -> brError $ "internal error: impossible case: " ++ (show x)

readBinary_Void :: BinaryReader ()
readBinary_Void = return ()

readBinary_Bool :: BinaryReader Bool
readBinary_Bool = do
	b <- brRead <?> "Bool value"
	case b of
		0 -> return False
		1 -> return True
		_ -> brError $ "invalid value for Bool: " ++ (show b)

------------------------------------------------------------

readBinary_OptionIndex1 :: BinaryReader Int
readBinary_OptionIndex1 = do
	a <- brRead <?> "tag"
	return $ fromIntegral a

readBinary_OptionIndex2 :: BinaryReader Int
readBinary_OptionIndex2 = do
	a <- brRead <?> "tag byte 1 of 2"
	b <- brRead <?> "tag byte 2 of 2"
	return $ fromIntegral $ mkNat16 a b

readBinary_OptionIndex3 :: BinaryReader Int
readBinary_OptionIndex3 = do
	a <- brRead <?> "tag byte 1 of 3"
	b <- brRead <?> "tag byte 2 of 3"
	c <- brRead <?> "tag byte 3 of 3"
	return $ fromIntegral $ mkNat32 0 a b c

readBinary_OptionIndex4 :: BinaryReader Int
readBinary_OptionIndex4 = do
	a <- brRead <?> "tag byte 1 of 4"
	b <- brRead <?> "tag byte 2 of 4"
	c <- brRead <?> "tag byte 3 of 4"
	d <- brRead <?> "tag byte 4 of 4"
	let i = fromIntegral (mkNat32 a b c d) :: Int
	if i < 0
		then brError $ "tag value out of range: " ++ (show $ mkNat32 a b c d)
		else return $ i

readBinary_Variant :: Int -> Array Int (BinaryReader a) -> BinaryReader a
readBinary_Variant i array =
	if i <= max
		then array ! i
		else brError $ "option index out of range: " ++ show i ++ ", maximum allowed = " ++ show max
	where
		(_, max) = Array.bounds array
