module Data.Cks.TextWriter (
	TextWriter, TextIndentedWriter, ErrorMessage,
	writeText_String, writeText_Bool, writeText_Void,
	writeText_Int, writeText_Nat,
	writeText_Int64, writeText_Int32, writeText_Int16, writeText_Int8, 
	writeText_Nat64, writeText_Nat32, writeText_Nat16, writeText_Nat8, 
	writeText_List, writeText_Maybe, writeText_Set, writeText_Map,
	writeTextIndented_String, writeTextIndented_Bool, writeTextIndented_Void,
	writeTextIndented_Int, writeTextIndented_Nat,
	writeTextIndented_Int64, writeTextIndented_Int32, writeTextIndented_Int16, writeTextIndented_Int8, 
	writeTextIndented_Nat64, writeTextIndented_Nat32, writeTextIndented_Nat16, writeTextIndented_Nat8, 
	writeTextIndented_List, writeTextIndented_Maybe, writeTextIndented_Set, writeTextIndented_Map,
) where

import Data.Cks.TextModel (quoteString, commaSep)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Cks.TreeDoc as TD
import Data.Cks.TreeDoc (TreeDoc)
import qualified Data.Set as Set
import Data.Word
import Data.Int
import Data.Cks.MaybeE

type ErrorMessage = String

type TextWriter a = a -> MaybeE ErrorMessage String
type TextIndentedWriter a = a -> MaybeE ErrorMessage TreeDoc

--------------------------------------------------------

writeText_String :: TextWriter String
writeText_String = Result . quoteString

writeText_Int :: TextWriter Integer
writeText_Int = Result . show

writeText_Nat :: TextWriter Integer
writeText_Nat = Result . show

writeText_Nat64 :: TextWriter Word64
writeText_Nat64 = Result . show
writeText_Nat32 :: TextWriter Word32
writeText_Nat32 = Result . show
writeText_Nat16 :: TextWriter Word16
writeText_Nat16 = Result . show
writeText_Nat8 :: TextWriter Word8
writeText_Nat8 = Result . show

writeText_Int64 :: TextWriter Int64
writeText_Int64 = Result . show
writeText_Int32 :: TextWriter Int32
writeText_Int32 = Result . show
writeText_Int16 :: TextWriter Int16
writeText_Int16 = Result . show
writeText_Int8 :: TextWriter Int8
writeText_Int8 = Result . show

writeText_Bool :: TextWriter Bool
writeText_Bool = Result . show

writeText_Void :: TextWriter ()
writeText_Void _ = Result $ "()"

-- Indented

ind :: TextWriter a -> TextIndentedWriter a
ind tw v = do
	v' <- tw v
	Result $ TD.line v'

writeTextIndented_String :: TextIndentedWriter String
writeTextIndented_String = ind writeText_String

writeTextIndented_Int :: TextIndentedWriter Integer
writeTextIndented_Int = ind writeText_Int

writeTextIndented_Nat :: TextIndentedWriter Integer
writeTextIndented_Nat = ind writeText_Nat

writeTextIndented_Nat64 :: TextIndentedWriter Word64
writeTextIndented_Nat64 = ind writeText_Nat64
writeTextIndented_Nat32 :: TextIndentedWriter Word32
writeTextIndented_Nat32 = ind writeText_Nat32
writeTextIndented_Nat16 :: TextIndentedWriter Word16
writeTextIndented_Nat16 = ind writeText_Nat16
writeTextIndented_Nat8 :: TextIndentedWriter Word8
writeTextIndented_Nat8 = ind writeText_Nat8

writeTextIndented_Int64 :: TextIndentedWriter Int64
writeTextIndented_Int64 = ind writeText_Int64
writeTextIndented_Int32 :: TextIndentedWriter Int32
writeTextIndented_Int32 = ind writeText_Int32
writeTextIndented_Int16 :: TextIndentedWriter Int16
writeTextIndented_Int16 = ind writeText_Int16
writeTextIndented_Int8 :: TextIndentedWriter Int8
writeTextIndented_Int8 = ind writeText_Int8

writeTextIndented_Bool :: TextIndentedWriter Bool
writeTextIndented_Bool = ind writeText_Bool

writeTextIndented_Void :: TextIndentedWriter ()
writeTextIndented_Void = ind writeText_Void

--------------------------------------------------------

writeText_List :: TextWriter e -> TextWriter [e]
writeText_List mA l = do
	contents <- mapM mA l
	Result $ "[" ++ commaSep contents ++ "]"

writeTextIndented_List :: TextIndentedWriter e -> TextIndentedWriter [e]
writeTextIndented_List mA l = do
	contents <- mapM mA l
	Result $ (TD.branchS' "[" "]" contents)

writeText_Set :: TextWriter e -> TextWriter (Set e)
writeText_Set mA set = do
	contents <- mapM mA (Set.toList set)
	Result $ "[" ++ commaSep contents ++ "]"

writeTextIndented_Set :: TextIndentedWriter e -> TextIndentedWriter (Set e)
writeTextIndented_Set mA set = do
	contents <- mapM mA (Set.toList set)
	Result $ TD.branchS' "[" "]" contents

writeText_Map :: TextWriter k -> TextWriter v -> TextWriter (Map k v)
writeText_Map mK mV map = do
	contents <- mapM stringify (Map.toList map)
	Result $ "[" ++ commaSep contents ++ "]"
	where
		stringify (k, v) = do
			k' <- mK k
			v' <- mV v
			Result $ k' ++ " -> " ++ v'

writeTextIndented_Map :: TextIndentedWriter k -> TextIndentedWriter v -> TextIndentedWriter (Map k v)
writeTextIndented_Map mK mV map = do
	contents <- mapM stringify (Map.toList map)
	Result $ TD.branchS' "[" "]" contents
	where
		stringify (k, v) = do
			k' <- mK k
			v' <- mV v
			Result $ TD.branch k' [TD.prefix "-> " v']

writeText_Maybe :: TextWriter e -> TextWriter (Maybe e)
writeText_Maybe mA v = case v of
	Just e -> do
		e' <- mA e
		Result $ "Set: " ++ e'
	Nothing -> Result $ "None"

writeTextIndented_Maybe :: TextIndentedWriter e -> TextIndentedWriter (Maybe e)
writeTextIndented_Maybe mA v = case v of
	Just e -> do
		e' <- mA e
		Result $ TD.prefix "Set: " e'
	Nothing -> Result $ TD.line "None"

