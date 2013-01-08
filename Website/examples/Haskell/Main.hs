module Main where

import Students (Students, Student(..), Sex(..),
                 writeBinary_Students, readBinary_Students)

import Data.Cks.BinaryReader as BinaryReader (run)
import Data.Cks.MaybeE (MaybeE(Error, Result))

import Data.ByteString as BS (readFile, writeFile, null)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

readCks :: FilePath -> IO (Either String Students)
readCks file = do
	-- Parse a CKS value from the file contents.
	bytes <- BS.readFile file
	return $ case BinaryReader.run readBinary_Students bytes of
		Error err -> Left ("Format error: " ++ err)
		Result students -> Right students

writeCks :: FilePath -> Students -> IO (Either String ())
writeCks file students =
	-- Serialize the CKS value
	case writeBinary_Students students of
		Error err -> return $ Left ("Unable to serialize: " ++ err)
		Result bytes -> do
			BS.writeFile file bytes
			return $ Right ()

die err = hPutStrLn stderr err >> exitFailure

main = do
	args <- getArgs
	file <- case args of
		[file] -> return file
		_ -> die "Expecting exactly one argument."

	-- Read in the list of students.
	readResult <- readCks file
	students <- case readResult of
		Left err -> die err
		Right students -> return students

	-- Append Shaggy to the list.
	let shaggy = Student_
		{ name'Student = "Shaggy Rogers"
		, age'Student = 42
		, sex'Student = Sex_Male }
	let students' = students ++ [shaggy]

	-- Write the updated list out to the same file.
	writeResult <- writeCks file students'
	case writeResult of
		Left err -> die err
		Right () -> return ()
