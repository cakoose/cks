module Data.Cks.TextReader (
	TextMarshaller, readText,
	marshalText_Bool, marshalText_Void, marshalText_Maybe,
	marshalText_List, marshalText_Set, marshalText_Map,
	marshalText_String, marshalText_Int, marshalText_Nat,
	marshalText_Int64, marshalText_Int32, marshalText_Int16, marshalText_Int8, 
	marshalText_Nat64, marshalText_Nat32, marshalText_Nat16, marshalText_Nat8, 
	marshalText_Variant, marshalText_RecordField, checkExtraFields, extractFields,
) where

import Data.Cks.TextModel
import qualified Data.Cks.TextParser as TextParser
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (foldM)
import Data.Word
import Data.Int
import Data.Bits (shiftL)
import Data.Cks.MaybeE

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Error as Parsec

type TextMarshaller a = (Maybe a, ValueL -> MaybeE ErrorMessage a)

readText :: TextMarshaller a -> String -> MaybeE ErrorMessage a
readText tr input =
	case Parsec.runParser TextParser.parse False "" input of
		Left pe -> Error $ convertParsecError pe
		Right ast -> snd tr ast

convertParsecError :: Parsec.ParseError -> ErrorMessage
convertParsecError pe = ErrorMessage (Just (line, col), message)
	where
		pos = Parsec.errorPos pe
		line = Parsec.sourceLine pos
		col = Parsec.sourceColumn pos
		message = Parsec.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (Parsec.errorMessages pe)

err loc msg = Error $ ErrorMessage (loc, msg)

-- Default value injector.
dv a m = (Just a, m)
dv' m = (Nothing, m)

marshalText_Bool :: TextMarshaller Bool
marshalText_Bool = dv' $ \v -> case v of
	(_, VVariant "True" (_, VVoid)) -> Result True
	(_, VVariant "False" (_, VVoid)) -> Result False
	(loc, _) -> err loc ("expecting a boolean (which means \"True\" or \"False\" with a void value)")

marshalText_String :: TextMarshaller String
marshalText_String = dv' $ \v -> case v of
	(_, VString s) -> Result s
	(loc, _) -> err loc ("expecting a string")

marshalText_Void :: TextMarshaller ()
marshalText_Void = dv () $ \v -> case v of
	(_, VVoid) -> Result ()
	(loc, _) -> err loc ("expecting a void value (\"()\")")

marshalText_List :: TextMarshaller a -> TextMarshaller [a]
marshalText_List (_,m) = dv' $ \v -> case v of
	(_, VCollection l) -> mapM checkElement l
	(loc, _) -> err loc ("expecting a collection")
	where
		checkElement (v, Nothing) = m v
		checkElement (_, Just (arrow, _)) = err arrow "expecting list element, found map entry"

marshalText_Set :: Ord a => TextMarshaller a -> TextMarshaller (Set a)
marshalText_Set (_,m) = dv' $ \v -> case v of
	(_, VCollection l) -> foldM setAdd Set.empty l
	(loc, _) -> err loc ("expecting a collection")
	where
		setAdd set (v@(vl, _), Nothing) = do
			v' <- m v
			if Set.member v' set
				then err vl "duplicate entry in set"
				else Result (Set.insert v' set)
		setAdd _ (_, Just (arrow, _)) = err arrow "expecting set element, found map entry"

marshalText_Map :: Ord k => TextMarshaller k -> TextMarshaller v -> TextMarshaller (Map k v)
marshalText_Map (_,mK) (dV,mV) = dv' $ \v -> case v of
	(_, VCollection l) -> foldM mapAdd Map.empty l
	(loc, _) -> err loc ("expecting a collection")
	where
		mapAdd map e = do
			k' <- getKey map e
			v' <- getValue e
			Result $ Map.insert k' v' map
		getKey map ((kl, kv), _) = do
			k' <- mK (kl, kv)
			if Map.member k' map
				then err kl "duplicate key in map"
				else Result k'
		getValue ((kl, _), Nothing) = case dV of
			Nothing -> err kl "missing value for map entry"
			Just d -> Result d
		getValue (_, Just (_, v)) = mV v
			
marshalText_Maybe :: TextMarshaller a -> TextMarshaller (Maybe a)
marshalText_Maybe (_,m) = dv Nothing $ \v -> case v of
	(_, VVariant "Set" ev) -> do
		e <- m ev
		Result $ Just e
	(_, VVariant "None" (_, VVoid)) -> Result Nothing
	(loc, _) -> err loc ("expecting an optional value, which means \"Set\" with a value or \"None\"")

marshalText_IntH :: String -> (Integer -> MaybeE String t) -> TextMarshaller t
marshalText_IntH noun checker = dv' $ \v -> case v of
	(loc, VInteger i) -> case checker i of
		Error errMsg -> err loc ("expecting " ++ noun ++ "; " ++ errMsg)
		Result n -> Result n
	(loc, _) -> err loc ("expecting " ++ noun)

marshalText_Int :: TextMarshaller Integer
marshalText_Int = marshalText_IntH "an integer" Result

marshalText_Nat :: TextMarshaller Integer
marshalText_Nat = marshalText_IntH "a natural number" (\i -> if i < 0
	then Error "negative integers are not allowed"
	else Result i)

marshalText_Nat64 :: TextMarshaller Word64
marshalText_Nat64 = marshalText_IntH "a 64-bit natural number" $ natBoundChecker (shiftL 1 64 - 1)

marshalText_Nat32 :: TextMarshaller Word32
marshalText_Nat32 = marshalText_IntH "a 32-bit natural number" $ natBoundChecker (shiftL 1 32 - 1)

marshalText_Nat16 :: TextMarshaller Word16
marshalText_Nat16 = marshalText_IntH "a 16-bit natural number" $ natBoundChecker (shiftL 1 16 - 1)

marshalText_Nat8 :: TextMarshaller Word8
marshalText_Nat8 = marshalText_IntH "a 8-bit natural number" $ natBoundChecker (shiftL 1 8 - 1)

marshalText_Int64 :: TextMarshaller Int64
marshalText_Int64 = marshalText_IntH "a 64-bit integer" $ intBoundChecker (negate $ shiftL 1 63) (shiftL 1 63 - 1)

marshalText_Int32 :: TextMarshaller Int32
marshalText_Int32 = marshalText_IntH "a 32-bit integer" $ intBoundChecker (negate $ shiftL 1 32) (shiftL 1 32 - 1)

marshalText_Int16 :: TextMarshaller Int16
marshalText_Int16 = marshalText_IntH "a 16-bit integer" $ intBoundChecker (negate $ shiftL 1 16) (shiftL 1 16 - 1)

marshalText_Int8 :: TextMarshaller Int8
marshalText_Int8 = marshalText_IntH "a 8-bit integer" $ intBoundChecker (negate $ shiftL 1 8) (shiftL 1 8 - 1)

intBoundChecker :: (Num t, Integral t) => Integer -> Integer -> Integer -> MaybeE String t
intBoundChecker minBound maxBound i = if i < (toInteger minBound)
		then Error ("value is less than " ++ show minBound)
		else if i > (toInteger maxBound)
			then Error ("value is greater than " ++ show maxBound)
			else Result $ fromInteger i

natBoundChecker :: (Num t, Integral t) => Integer -> Integer -> MaybeE String t
natBoundChecker maxBound i = if i < 0
	then Error ("negative integers are not allowed")
	else if i > (toInteger maxBound)
		then Error ("value is greater than " ++ show maxBound)
		else Result $ fromInteger i

marshalText_Variant :: Map String (ValueL -> MaybeE ErrorMessage a) -> TextMarshaller a
marshalText_Variant map = dv' $ \v -> case v of
	(loc, VVariant n e) -> case Map.lookup n map of
		Just m -> m e
		Nothing -> err loc ("invalid option \"" ++ n ++ "\"")
	(loc, _) -> err loc ("expecting a variant value")

type FieldMap = Map String (MLoc, ValueL)

marshalText_RecordField :: MLoc -> String -> TextMarshaller a -> FieldMap -> MaybeE ErrorMessage (a, FieldMap)
marshalText_RecordField rloc name (defVal,m) fields = case Map.lookup name fields of
	Just (_, val) -> do
		v <- m val
		Result (v, Map.delete name fields)
	Nothing -> case defVal of
		Just v -> Result (v, Map.delete name fields)
		Nothing -> err rloc ("missing field \"" ++ name ++ "\"")

checkExtraFields :: FieldMap -> MaybeE ErrorMessage ()
checkExtraFields m = case Map.toList m of
	(name, (loc, _)):_ -> err loc ("unknown field \"" ++ name ++ "\"")
	[] -> Result ()

extractFields :: ValueL -> MaybeE ErrorMessage (MLoc, FieldMap)
extractFields (loc, v) = case v of
	VRecord fields -> Result (loc, fields)
	_ -> err loc ("expecting a record")

