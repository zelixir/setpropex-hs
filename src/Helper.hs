module Helper
  ( module Helper
  -- , module Data.Proxy
  , module Data.Maybe
  , module Data.List
  , module Data.Either
  , module Data.Functor
  , module Data.Bifunctor
  , module Data.Function
  , module Data.Foldable
  , module Data.String
  , module Data.Monoid
  , module Data.Tuple
  , module Data.Char
  , module Data.Bool
  , module System.Directory
  , module System.Exit
  , module System.Environment
  , module System.FilePath
  , module GHC.Generics
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
import           System.FilePath
-- import           Data.Proxy
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
import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Generics
import           GHC.Word
import           Debug.Trace             hiding ( trace )
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString               as B
import           GHC.Exts                       ( groupWith )
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString               as B

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
comp2 a b = (a .) . b

-- headMaybe = fmap fst . uncons

groupOn f = groupBy ((==) `on` f)
toGroup :: Ord a => [(a, b)] -> [(a, [b])]
toGroup = map (fst . head &&& map snd) . groupWith fst

type LS = L.ByteString
type BS = B.ByteString

