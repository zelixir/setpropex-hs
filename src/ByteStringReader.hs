{-# LANGUAGE MultiParamTypeClasses, ViewPatterns #-}
module ByteStringReader where
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import           Helper
class GenBS s where
  bSplitAt :: Int -> s -> (s, s)
  bUnpack :: s -> [Word8]

class BRead a where
  bRead :: GenBS s => s -> (a, s)
  bReadMany :: GenBS s => Int -> s -> ([a], s)
  bReadMany 0 s = ([], s)
  bReadMany n s = let (x, bReadMany (n-1) -> (xs, s')) = bRead s in ((x:xs), s')

instance GenBS B.ByteString where
  bSplitAt = B.splitAt
  bUnpack  = B.unpack

instance GenBS L.ByteString where
  bSplitAt = L.splitAt . fromIntegral
  bUnpack  = L.unpack

-- little endian
instance BRead Word32 where
  bRead = first (foldr (\x acc -> acc * 256 + fromIntegral x) 0 . bUnpack)
    . bSplitAt 4

