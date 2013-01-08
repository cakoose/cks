module Data.Cks.TextParser (
	parse
) where

import Data.Cks.TextModel (Value(..), VCollectionElement, ValueL, MLoc, ErrorMessage(..),)
import Data.Char (digitToInt, isSpace)
import Text.ParserCombinators.Parsec (
	GenParser, (<|>), (<?>), sepEndBy, eof, manyTill, getPosition,
	sourceLine, sourceColumn, option, optionMaybe, try, many, skipMany,
	skipMany1, setState, getState,)
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Prim (pzero)
import Control.Monad (foldM)
import Data.Cks.MaybeE

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Bits (shiftL, (.|.))
import qualified Data.Char as Char
import Data.Char (ord, chr)

type Par a = GenParser Char Bool a
-- state = whether the last whitespace run had an EOL

lexeme p = do
	x <- p
	ws
	return x

ident = lexeme $ do
	c <- identStart
	cs <- many identLetter
	return (c:cs)
	where
		identStart = letter
		identLetter = alphaNum

integer = lexeme $ do
	f <- option id (do char '-' ; return neg)
	d <- digit
	ds <- many digit
	let num = foldl (\x d -> 10*x + d) (c2i d) (map c2i ds)
	return $ f num
	where
		c2i = toInteger.digitToInt
		neg i = -i

op c = lexeme $ do char c ; return ()

ws = do
	l1 <- getLine
	skipMany (simpleSpace <|> oneLineComment <|> multiLineComment <?> "")
	l2 <- getLine
	setState (l1 < l2)
	where
		getLine = do
			pos <- getPosition
			return $ sourceLine pos

simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment = do
	try (string "//")
	skipMany (satisfy (/= '\n'))
	return ()

multiLineComment = do
	try (string "/*")
	inComment
	where
		inComment =
			do { try (string "*/") ; return () }
			<|> do { skipMany1 (noneOf "*/") ; inComment }
			<|> do { oneOf "*/" ; inComment }
			<?> "end of comment"


pBrackets :: Par a -> Par a
pBrackets p = do
	op '['
	r <- p
	op ']'
	return r

pBraces :: Par a -> Par a
pBraces p = do
	op '{'
	r <- p
	op '}'
	return r

getLoc :: Par MLoc
getLoc = do
	pos <- getPosition
	return $ Just (sourceLine pos, sourceColumn pos)

---------------------------------------------------------

parse :: Par ValueL
parse = do 
	ws
	v <- pValue
	eof
	return v

pValue :: Par ValueL
pValue = pRecord <|> pVariant <|> pString <|> pInteger <|> pVoid <|> pCollection

pRecord :: Par ValueL
pRecord = do 
	l <- getLoc
	fs <- pBraces (commaSep pField)
	case foldM addField Map.empty fs of
		Result m -> return (l, VRecord m)
		Error (ErrorMessage (_, m)) -> fail m

insertLookup :: (Ord k) => k -> v -> Map k v -> (Maybe v, Map k v)
insertLookup = Map.insertLookupWithKey (\_ a _ -> a)

type FieldEntry = (String, (MLoc, ValueL))
type FieldMap = Map String (MLoc, ValueL)

addField :: FieldMap -> FieldEntry -> MaybeE ErrorMessage FieldMap
addField map (name, (loc, v)) = 
	let (oldValM, newMap) = insertLookup name (loc, v) map in
	case oldValM of
		Just (oldLoc, _) -> Error $ ErrorMessage (oldLoc, "duplicate entry for field \"" ++ name ++ "\"")
		Nothing -> Result newMap

pField :: Par (String, (MLoc, ValueL))
pField = do
	l <- getLoc
	name <- ident
	op '='
	value <- pValue
	return $ (name, (l, value))

pVariant :: Par ValueL
pVariant = do
	l <- getLoc
	name <- ident
	e <- option (l, VVoid) $ do
		op ':'
		pValue
	return $ (l, VVariant name e)

pString :: Par ValueL
pString = do
	l <- getLoc
	char '"'
	s <- manyTill pStringChar (do char '"')
	ws
	return $ (l, VString s)

pCollection :: Par ValueL
pCollection = do
	l <- getLoc
	es <- pBrackets (commaSep pCollectionElement)
	return $ (l, VCollection es)

pCollectionElement :: Par VCollectionElement
pCollectionElement = do
	v1 <- pValue
	e <- optionMaybe (do
		arrowLoc <- getLoc
		try (lexeme $ string "->")
		v2 <- pValue
		return (arrowLoc, v2))
	return $ (v1, e)

pStringChar = pEscapeSequence <|> pNormalChar

simpleEscapes = [('n', '\n'), ('t', '\t'), ('r', '\r'), ('\\', '\\'), ('"', '"'), ('\'', '\''), ('0', '\0')]
simpleEscapeParsers = map (\(c,r) -> do char c ; return r) simpleEscapes

pHexDigit :: Par Int
pHexDigit = do
	c <- letter
	if c >= '0' && c <= '9'
		then return $ ord c - ord '0'
		else if c >= 'A' && c <= 'F'
			then return $ 10 + ord c - ord 'A'
			else if c >= 'a' && c <= 'F'
				then return $ 10 + ord c - ord 'a'
				else fail "expecting a hexadecimal digit"

hexEscapeParser = do
	char 'x'
	h1 <- pHexDigit
	h2 <- pHexDigit
	return $ chr $ (h1 `shiftL` 4) .|. h2

uEscapeParser = do
	char 'u'
	h1 <- pHexDigit
	h2 <- pHexDigit
	h3 <- pHexDigit
	h4 <- pHexDigit
	return $ chr $
		(h1 `shiftL` 12) .|.
		(h2 `shiftL` 8) .|.
		(h3 `shiftL` 4) .|.
		(h4 `shiftL` 0)

uPlusEscapeParser = do
	char '+'
	h1 <- pHexDigit
	h2 <- pHexDigit
	h3 <- pHexDigit
	h4 <- pHexDigit
	h5 <- pHexDigit
	h6 <- pHexDigit
	return $ chr $
		(h1 `shiftL` 20) .|.
		(h2 `shiftL` 16) .|.
		(h3 `shiftL` 12) .|.
		(h4 `shiftL` 8) .|.
		(h5 `shiftL` 4) .|.
		(h6 `shiftL` 0)

escapeParsers = simpleEscapeParsers ++ [hexEscapeParser, uEscapeParser, uPlusEscapeParser]
pEscapeSequence = char '\\' >> foldl (<|>) pzero escapeParsers

pNormalChar = satisfy isNormalChar
isNormalChar c = (Char.generalCategory c /= Char.Control)
	&& (c /= '\\') && (c /= '"')

pInteger :: Par ValueL
pInteger = do
	l <- getLoc
	i <- integer
	return $ (l, VInteger i)

pVoid :: Par ValueL
pVoid = do
	l <- getLoc
	op '('
	op ')'
	return (l, VVoid)

commaSep :: Par a -> Par [a]
commaSep p = do
	p `sepEndBy` sep
	where
		sep = do
			haveEol <- getState
			if haveEol
				then option () (op ',')
				else op ','
