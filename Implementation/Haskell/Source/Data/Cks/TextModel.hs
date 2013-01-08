module Data.Cks.TextModel (
	Value(..), VCollectionElement, ValueL, MLoc, Loc, ErrorMessage(..),
	showValue, showValueL,
	quoteString, commaSep,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (chr, ord)
import Data.Array.Unboxed (UArray)
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IArray
import Data.Bits (shiftR, (.&.))

type Loc = (Int, Int)
type MLoc = Maybe Loc

type ValueL = (MLoc, Value)

newtype ErrorMessage = ErrorMessage (MLoc, String)

data Value
	= VVariant String ValueL
	| VRecord (Map String (MLoc, ValueL))
	| VCollection [VCollectionElement]
	| VString String
	| VInteger Integer
	| VVoid
	deriving Eq

type VCollectionElement = (ValueL, Maybe (MLoc, ValueL))

instance Show ErrorMessage where
	show = showError

showError :: ErrorMessage -> String
showError (ErrorMessage (Nothing, msg)) = msg
showError (ErrorMessage ((Just (line, col)), msg)) = "line "++(show line)++"."++(show col)++": "++msg

-- Show without location information.

showValue :: ValueL -> String
showValue (_,v) = case v of
	VVariant n (_,VVoid) -> n
	VVariant n v -> n ++ ": " ++ showValue v
	VRecord m -> "{" ++ commaSep (map showField (Map.assocs m)) ++ "}"
	VCollection l -> "[" ++ commaSep (map showCollectionElement l) ++ "]"
	VString s -> quoteString s
	VInteger i -> show i
	VVoid -> "()"
	where
		showField (n,(_,v)) = n ++ " = " ++ showValue v

-- Show with location information.

showValueL :: ValueL -> String
showValueL (l,v) = makeLocPrefix l ++ case v of
	VVariant n (_,VVoid) -> n
	VVariant n v -> n ++ ": " ++ showValueL v
	VRecord m -> "{" ++ commaSep (map showField (Map.assocs m)) ++ "}"
	VCollection l -> "[" ++ commaSep (map showCollectionElementL l) ++ "]"
	VString s -> quoteString s
	VInteger i -> show i
	VVoid -> "()"
	where
		makeLocPrefix :: MLoc -> String
		makeLocPrefix Nothing = ""
		makeLocPrefix (Just (l,c)) = "<"++show l++","++show c++">"
		showField (n,(_,v)) = n ++ " = " ++ showValueL v

showCollectionElement :: VCollectionElement -> String
showCollectionElementL :: VCollectionElement -> String

showCollectionElement (v, Nothing) = showValue v
showCollectionElement (v, Just (_, v2)) = showValue v ++ " -> " ++ showValue v2
showCollectionElementL (v, Nothing) = showValueL v
showCollectionElementL (v, Just (_, v2)) = showValueL v ++ " -> " ++ showValueL v2

commaSep :: [String] -> String
commaSep [] = ""
commaSep (h:t) = h ++ concatMap (", "++) t

quoteString :: String -> String
quoteString s = "\"" ++ concatMap q s ++ "\""
	where
		q '"' = "\\\""
		q '\\' = "\\\\"
		q '\n' = "\\n"
		q '\r' = "\\r"
		q '\t' = "\\t"
		q '\0' = "\\0"
		q c | isIsoControl c = "\\x" ++ toHex2 c
		q c = [c]

toHex2 :: Char -> String
toHex2 c = c1:c2:[]
	where
		i = ord c
		c1 = hexDigit (i `shiftR` 4)
		c2 = hexDigit (i .&. 0xf)

hexDigits :: UArray Int Char
hexDigits = IArray.listArray (0, 15) "01234567890abcdef"
hexDigit :: Int -> Char
hexDigit i = hexDigits ! i

isIsoControl c | c < (chr 0x20) = True
isIsoControl c | c >= (chr 0x7f) && c < (chr 0xa0) = True
isIsoControl _ = False
