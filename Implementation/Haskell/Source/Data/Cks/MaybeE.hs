module Data.Cks.MaybeE (
	MaybeE(..),
) where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

data MaybeE e r = Result r | Error e

instance Monad (MaybeE e) where
	(Result r) >>= k  = k r
	(Error e)  >>= _  = Error e
	(Result _) >>  k  = k
	(Error e)  >>  _  = Error e
	return           = Result
	fail _           = Error undefined

-- Future-proofing for the Applicative-Monad merge in GHC 7.10.
instance Functor (MaybeE e) where
    fmap = liftM
instance Applicative (MaybeE e) where
    pure = return
    (<*>) = ap

