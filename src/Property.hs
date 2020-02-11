{-# LANGUAGE ViewPatterns, TypeApplications, OverloadedStrings, ScopedTypeVariables #-}
module Property where
import           Helper
import           System.Directory
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Lazy          as L
import           PTrace
import           Numeric
import           ByteStringReader

type Properties = (FilePath, BS)

headerSize = 128

fromFile :: FilePath -> IO (Maybe Properties)
fromFile path = do
  d <- L.readFile path
  let magic  = L.take 8 $ L.drop 8 d
  let magic0 = "\x50\x52\x4F\x50\xAB\xD0\x6E\xFC"
  return $ if magic /= magic0
    then Nothing
    else Just (path, L.toStrict $ L.drop 0x80 d)

findProperty :: BS -> Properties -> Maybe (BS, Int)
findProperty name = findProperty' names 0
  where names = "" : B.splitWith (== fromIntegral (ord '.')) name

findProperty' :: [BS] -> Int -> Properties -> Maybe (BS, Int)
findProperty' [] x prop = undefined
findProperty' (x : xs) offset (file, mem) | name == x && children == 0 = Just
  (value, offset + 20 + namelen_align)
 where
  ([fromIntegral -> namelen, _propid, left, right, children :: Word32], s) =
    bReadMany 5 (B.drop offset mem)
  (name1, s') = B.splitAt namelen_align s
  name = B.take namelen name1
  value = undefined 
  namelen_align =
    uncurry (+) $ bimap (* 4) ((* 4) . signum) $ divMod (namelen + 1) 4
  


findMemoryRegion :: FilePath -> IO Int
findMemoryRegion = findMemoryRegion' "/proc/1/maps"

findMemoryRegion' :: FilePath -> FilePath -> IO Int
findMemoryRegion' maps file = do
  list <- lines <$> readFile maps
  let r   = find (isSuffixOf file) list >>= uncons . readHex
  let err = "Memory region of " ++ file ++ " not found!"
  maybe (die err) (return . fromInteger . fst . fst) r

writeProperty :: String -> String -> IO ()
writeProperty name@(fromString -> nameB) (fromString -> newValue) = do
  files      <- propertyFiles
  properties <- catMaybes <$> mapM fromFile files
  case asum $ findProperty nameB <$> properties of
    Nothing ->
      die $ "Cannot find property [" ++ name ++ "], search files: \n" ++ unlines
        (map fst properties)
    Just (value, _) | value == newValue ->
      putStrLn "Not changed." >> exitSuccess
    Just (_, offset) -> with_process 1 $ \p -> do
      let len = B.length newValue
      let pad = 92 - len
      when (pad < 0) $ die "Value is too long!"
      poke_data @Word32 p offset (fromIntegral len)
      write_mem p (offset + 4) (newValue <> B.pack (replicate pad 0))

propertyFiles :: IO [FilePath]
propertyFiles =
  ifM (doesFileExist path0) (return [path0])
    $ ifM (doesDirectoryExist path0) (listDirectory path0)
    $ return []
 where
  path0 = "/dev/__properties__"
  ifM b t f = liftM3 bool f t b



