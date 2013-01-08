module Data.Cks.TreeDoc (
	TreeDoc, line, branch, branch', branchS, branchS', prefix, print,
) where

import Prelude ((++), String, Show, Maybe(..), show, concatMap)

data TreeDoc
	= Line String
	| Branch TreeDoc (Maybe String) [TreeDoc]

line :: String -> TreeDoc
line = Line

branch :: TreeDoc -> [TreeDoc] -> TreeDoc
branch head children = Branch head Nothing children

branch' :: TreeDoc -> String -> [TreeDoc] -> TreeDoc
branch' head after children = Branch head (Just after) children

branchS :: String -> [TreeDoc] -> TreeDoc
branchS head children = Branch (Line head) Nothing children

branchS' :: String -> String -> [TreeDoc] -> TreeDoc
branchS' head after children = Branch (Line head) (Just after) children

eol = "\n"

prefix :: String -> TreeDoc -> TreeDoc
prefix p (Line s) = Line (p++s)
prefix p (Branch head after children) = Branch (prefix p head) after children

print :: String -> String -> TreeDoc -> String
print indent lead d = case d of
	Line s -> lead ++ s ++ eol
	Branch head after children ->
		print indent lead head ++
			concatMap (print indent (lead++indent)) children ++
			(case after of
				Just s -> lead ++ s ++ eol
				Nothing -> "")

instance Show TreeDoc where
	show = print ("  ") ("")
