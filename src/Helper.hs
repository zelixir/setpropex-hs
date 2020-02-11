module Helper
  ( module Helper
  , module Data.Maybe
  , module Data.List
  , module Data.Either
  , module Data.Functor
  , module Data.Bifunctor
  , module Data.Function
  , module Data.Foldable
  , module Data.String
  , module Data.Monoid
  , module Data.Word
  , module Data.Tuple
  , module Data.Char
  , module Data.Bool
  , module System.Directory
  , module System.Exit
  , module System.Environment
  , module GHC.Word
  , module Control.Arrow
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Debug.Trace
  )
where

import           System.Directory
import           System.Exit
import           System.Environment
import           Data.Maybe
import           Data.List
import           Data.Either
import           Data.Tuple
import           Data.Bool
import           Data.Char
import           Data.Functor
import           Data.Bifunctor                 ( bimap )
import           Data.Function
import           Data.Foldable
import           Data.String
import           Data.Monoid
import           Data.Word
import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Word
import           Debug.Trace             hiding ( trace )
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString               as B
import           Foreign

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
comp2 a b = (a .) . b

type LS = L.ByteString
type BS = B.ByteString

extractWord :: BS -> Word
extractWord = B.foldr (\w n -> n `shiftL` 8 .|. fromIntegral w) 0

byteSwap :: (Integral a, Storable a) => a -> a
byteSwap x = case sizeOf x of
  2 -> ff byteSwap16
  4 -> ff byteSwap32
  8 -> ff byteSwap64
  where ff f = fromIntegral $ f (fromIntegral x)
