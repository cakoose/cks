module Harness (run) where

import Prelude hiding (catch)
import qualified Data.Cks.TreeDoc as TD
import qualified Data.Cks.TextParser as TextParser
import qualified Data.Cks.TextReader as TextReader
import qualified Data.Cks.TextModel as TextModel
import qualified Data.Cks.BinaryReader as BinaryReader
import qualified Data.Cks.BinaryWriter as BinaryWriter
import Data.Cks.TextReader (TextMarshaller)
import Data.Cks.TextWriter (TextWriter, TextIndentedWriter)
import Data.Cks.BinaryReader (BinaryReader)
import Data.Cks.BinaryWriter (BinaryWriter)
import Data.Cks.MaybeE

import qualified Data.ByteString as B

import Control.Exception (catch, finally)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStr, hPutStrLn, hGetContents, stdin, stderr, stdout,
	withFile, withBinaryFile, IOMode(WriteMode, ReadMode), hIsClosed, hFlush)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (runParser)
import Data.List (elemIndex)

run :: Ord a =>
	   BinaryReader a
	-> BinaryWriter a
	-> TextMarshaller a
	-> TextWriter a
	-> TextIndentedWriter a
	-> IO ()
run br bw tr tw tiw = do
	closed <- hIsClosed stdin
	if closed
		then return ()
		else do
			(run' br bw tr tw tiw `catch` ignore) `finally` (do
				hPutStrLn stdout ">."
				hFlush stdout
				hPutStrLn stderr "!."
				hFlush stderr)
			run br bw tr tw tiw
	where
		ignore (e :: ExitCode) = return ()

run' :: Ord a =>
	   BinaryReader a
	-> BinaryWriter a
	-> TextMarshaller a
	-> TextWriter a
	-> TextIndentedWriter a
	-> IO ()
run' binaryReader binaryWriter textReader textWriter textIndentedWriter = do
	command <- getLine
	case command of
		"convert" -> do
			inputString <- getLine
			outputString <- getLine
			inputSpec <- parseOrDie "input-spec" (parseInputSpec inputString)
			outputSpec <- parseOrDie "output-spec" (parseOutputSpec outputString)
			value <- readValue inputSpec
			writeValue outputSpec value
		"compare" -> do
			inputString1 <- getLine
			inputString2 <- getLine
			inputSpec1 <- parseOrDie "input-spec-1" (parseInputSpec inputString1)
			inputSpec2 <- parseOrDie "input-spec-2" (parseInputSpec inputString2)
			value1 <- readValue inputSpec1
			value2 <- readValue inputSpec2
			hPutStrLn stdout $ "> " ++ (if value1 == value2 then "equal" else "not equal")
		_ -> do
			hPutStrLn stderr $ "! Invalid command: \"" ++ command ++ "\""
			exitWith $ ExitFailure 1

	where
		readValue (format, file) = case format of
			IF_Text -> withFile file ReadMode (readText file)
			IF_Binary -> withBinaryFile file ReadMode (readBinary file)
		writeValue (format, file) v = case format of
			OF_Text -> withFile file WriteMode (writeText v)
			OF_TextI -> withFile file WriteMode (writeTextIndented v)
			OF_Binary -> withBinaryFile file WriteMode (writeBinary v)
		writeBinary v f = case (binaryWriter v) of
			Error err -> do
				hPutStrLn stderr $ "! Error writing value in binary format: " ++ err
				exitWith $ ExitFailure 1
			Result bytes -> do B.hPutStr f bytes
		writeText v f = case (textWriter v) of
			Error err -> do
				hPutStrLn stderr $ "! Error writing value in text format: " ++ err
				exitWith $ ExitFailure 1
			Result chars -> do hPutStr f chars
		writeTextIndented v f = case (textIndentedWriter v) of
			Error err -> do
				hPutStrLn stderr $ "! Error writing value in text format: " ++ err
				exitWith $ ExitFailure 1
			Result doc -> do hPutStr f (TD.print "\t" "" doc)
		readBinary fn f = do
			bytes <- B.hGetContents f
			case (BinaryReader.run binaryReader bytes) of
				Error err -> do
					hPutStrLn stderr $ "! " ++ fn ++ ": " ++ err
					exitWith $ ExitFailure 1
				Result v -> return v
		readText fn f = do
			chars <- hGetContents f
			case TextReader.readText textReader chars of
				Error err -> do
					hPutStrLn stderr $ "! " ++ fn ++ ": " ++ show err
					exitWith $ ExitFailure 1
				Result v -> return v

type ErrorMessage = String

parseOrDie :: String -> MaybeE ErrorMessage a -> IO a
parseOrDie name result = case result of
	Error err -> do
		hPutStrLn stderr $ "! Invalid " ++ name ++ ": " ++ err
		exitWith $ ExitFailure 1
	Result v -> return v

parseInputSpec :: String -> MaybeE ErrorMessage InputSpec
parseInputSpec s =
	case elemIndex ':' s of
		Just colonPos ->
			let
				format = take colonPos s
				file = drop (colonPos+1) s
			in
				case format of
					"text" -> Result (IF_Text, file)
					"binary" -> Result (IF_Binary, file)
					_ -> Error $ "invalid input format: \"" ++ format ++ "\""
		Nothing -> Error "missing colon"

parseOutputSpec :: String -> MaybeE ErrorMessage OutputSpec
parseOutputSpec s =
	case elemIndex ':' s of
		Just colonPos ->
			let
				format = take colonPos s
				file = drop (colonPos+1) s
			in
				case format of
					"text" -> Result (OF_Text, file)
					"texti" -> Result (OF_TextI, file)
					"binary" -> Result (OF_Binary, file)
					_ -> Error $ "invalid output format: \"" ++ format ++ "\""
		Nothing -> Error "missing colon"

type InputSpec = (InputFormat, FilePath)
type OutputSpec = (OutputFormat, FilePath)

data InputFormat = IF_Text | IF_Binary
data OutputFormat = OF_Text | OF_TextI | OF_Binary
