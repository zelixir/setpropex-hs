{-# LANGUAGE ViewPatterns, TupleSections, TypeApplications, OverloadedStrings, ScopedTypeVariables, FlexibleContexts  #-}
module Property where
import           Helper
import           System.Directory
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Lazy          as L
import           PTrace
import           Numeric
import           State
import           System.IO.Unsafe
import           Foreign
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

findProperty :: BS -> Properties -> Maybe (FilePath, BS, Int)
findProperty name (file, mem) = findProperty' names 0
 where
  names = "" : B.splitWith (== fromIntegral (ord '.')) name

  findProperty' []       _      = Nothing
  findProperty' (x : xs) offset = flip evalState (B.drop offset mem) $ do
    namelen  <- readi32l
    _propid  <- readi32l
    left     <- readi32l
    right    <- readi32l
    children <- readi32l
    let namelen_align =
          uncurry (+) $ bimap (* 4) ((* 4) . signum) $ divMod (namelen + 1) 4
    let valueOffset = offset + 20 + namelen_align + headerSize
    name <- B.take namelen <$> state (B.splitAt namelen_align)
    case (compare `on` (B.length &&& id)) x name of
      EQ | xs == [] && children == 0 ->
        Just . (file, , valueOffset) <$> (readi32b >>= state . B.splitAt)
      EQ | children /= 0 -> return $ findProperty' xs children
      LT | left /= 0     -> return $ findProperty' (x : xs) left
      GT | right /= 0    -> return $ findProperty' (x : xs) right
      _                  -> return Nothing

readi32l, readi32b :: State BS Int
readi32l = state $ first (fromIntegral . extractWord) . B.splitAt 4
readi32b = byteSwap <$> readi32l

findMemoryRegion :: FilePath -> IO Integer
findMemoryRegion = findMemoryRegion' "/proc/1/maps"

findMemoryRegion' :: FilePath -> FilePath -> IO Integer
findMemoryRegion' maps file = do
  list <- lines <$> readFile maps
  let r   = find (isSuffixOf file) list >>= uncons . readHex
  let err = "Memory region of " ++ file ++ " not found!"
  maybe (die err) (return . fst . fst) r

writeProperty :: String -> String -> IO ()
writeProperty name@(fromString -> nameB) (fromString -> newValue) = do
  files      <- propertyFiles
  properties <- catMaybes <$> mapM fromFile files
  case asum $ findProperty nameB <$> properties of
    Nothing ->
      die $ "Cannot find property [" ++ name ++ "], search files: \n" ++ unlines
        (map fst properties)
    Just (_, value, _) | value == newValue ->
      putStrLn "Not changed." >> exitSuccess
    Just (file, _, offset0) -> with_process 1 $ \p -> do
      mem_region <- findMemoryRegion file
      let offset = fromInteger $ mem_region + fromIntegral offset0
      let len    = B.length newValue
      let pad    = 92 - len
      when (pad < 0) $ die "Value is too long!"
      poke_data @Word32 p offset (byteSwap $ fromIntegral len)
      write_mem p (offset + 4) (newValue <> B.pack (replicate pad 0))
      putStrLn "Value changed."

propertyFiles :: IO [FilePath]
propertyFiles =
  ifM (doesFileExist path0) (return [path0])
    $ ifM (doesDirectoryExist path0) (listDirectory path0)
    $ return []
 where
  path0 = "/dev/__properties__"
  ifM b t f = liftM3 bool (unsafeInterleaveIO f) (unsafeInterleaveIO t) b



