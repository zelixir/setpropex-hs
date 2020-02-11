{-# LANGUAGE ForeignFunctionInterface, ViewPatterns, RankNTypes, ScopedTypeVariables #-}

module PTrace where
import           Foreign
import           Foreign.C
import           Control.Exception
import           Foreign.Marshal.Alloc
import           GHC.Word
import           Helper
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as B
type PID = Int
type RemoteAddress = Int
type TRACE_REQUEST = Int
newtype Process = Process { pid :: PID }

pTRACE_TRACEME = 0 :: TRACE_REQUEST
pTRACE_PEEKTEXT = 1 :: TRACE_REQUEST
pTRACE_PEEKDATA = 2 :: TRACE_REQUEST
pTRACE_PEEKUSR = 3 :: TRACE_REQUEST
pTRACE_POKETEXT = 4 :: TRACE_REQUEST
pTRACE_POKEDATA = 5 :: TRACE_REQUEST
pTRACE_POKEUSR = 6 :: TRACE_REQUEST
pTRACE_CONT = 7 :: TRACE_REQUEST
pTRACE_KILL = 8 :: TRACE_REQUEST
pTRACE_SINGLESTEP = 9 :: TRACE_REQUEST
pTRACE_ATTACH = 16 :: TRACE_REQUEST
pTRACE_DETACH = 17 :: TRACE_REQUEST
pTRACE_SYSCALL = 24 :: TRACE_REQUEST

-- object_r_default_prop_s0

foreign import ccall unsafe "ptrace" c_ptrace :: CInt -> CInt -> CLong -> Ptr a -> IO CLong


ptrace :: TRACE_REQUEST -> PID -> RemoteAddress -> Ptr a -> IO Word
ptrace req pid addr = fmap fromIntegral
  . c_ptrace (fromIntegral req) (fromIntegral pid) (fromIntegral addr)

ptrace_peekdata :: PID -> RemoteAddress -> IO Word
ptrace_peekdata pid addr = ptrace pTRACE_PEEKDATA pid addr nullPtr

ptrace_pokedata :: PID -> RemoteAddress -> Word -> IO ()
ptrace_pokedata pid addr w = alloca $ \ptr -> do
  poke ptr w
  ptrace pTRACE_PEEKDATA pid addr ptr
  return ()

with_process :: PID -> (Process -> IO r) -> IO r
with_process p = bracket attach detach
 where
  attach = ptrace pTRACE_ATTACH p 0 nullPtr >> return (Process p)
  detach _ = ptrace pTRACE_DETACH p 0 nullPtr

peek_data :: forall  a . Storable a => Process -> RemoteAddress -> IO a
peek_data (pid -> p) addr = do
  (ptr, off, _) <- B.toForeignPtr <$> read_mem p addr (sizeOf (undefined :: a))
  withForeignPtr ptr (\p -> peekByteOff p off)

poke_data :: forall  a . Storable a => Process -> RemoteAddress -> a -> IO ()
poke_data (pid -> p) addr a = write_mem p addr
  $ B.unsafeCreate (sizeOf (undefined :: a)) (flip poke a . castPtr)


read_mem :: PID -> RemoteAddress -> Int -> IO BS
read_mem _ _ 0 = return B.empty
read_mem pid addr size =
  (B.take size . B.drop extraBytes . joinWords)
    `fmap` mapM (ptrace_peekdata pid) readPtrs
 where
  wordSize    = fromIntegral $ sizeOf addr
  alignedAddr = addr .&. complement (wordSize - 1)
  extraBytes  = fromIntegral $ addr - alignedAddr
  totalBytes  = fromIntegral $ size + extraBytes
  readPtrs    = map
    fromIntegral
    [alignedAddr, alignedAddr + wordSize .. alignedAddr + totalBytes - 1]
  joinWords = B.pack . (extractBytes =<<)
  -- Assuming little-endian :O Could use peekByteOff instead?
  extractBytes n = map (fromIntegral . (0xff .&.) . (n `shiftR`))
                       [0, 8 .. fromIntegral $ 8 * wordSize - 1]

-- FIXME: Is it more efficient to keep /proc/<...>/mem open and write to that?
--        Does the kernel even support that?
write_mem :: PID -> RemoteAddress -> BS -> IO ()
write_mem pid addr bs = do
  s <- start
  e <- end
  doWrite (s `mappend` bs `mappend` e)
 where
  size        = B.length bs
  wordSize    = sizeOf addr
  alignedAddr = addr .&. complement (fromIntegral wordSize - 1)
  startBytes  = fromIntegral $ addr - alignedAddr
  endBytes    = -(size + startBytes) .&. complement (wordSize - 1)
  totalBytes  = size + startBytes + endBytes
  start       = read_mem pid alignedAddr startBytes
  end         = read_mem pid (alignedAddr + fromIntegral startBytes) endBytes

  writePtrs   = map
    fromIntegral
    [alignedAddr, alignedAddr + fromIntegral wordSize .. alignedAddr
    + fromIntegral totalBytes
    - 1]
  splitWords  = map extractWord . chunksOf wordSize
  -- Assuming little-endian :O Could use pokeByteOff instead?
  extractWord = B.foldl' (\n w -> n `shiftL` 8 .|. fromIntegral w) 0
  doWrite     = sequence_ . zipWith (ptrace_pokedata pid) writePtrs . splitWords


chunksOf :: Int -> BS -> [BS]
chunksOf n bs
  | B.null bs = []
  | otherwise = let (chunk, bs') = B.splitAt n bs in chunk : chunksOf n bs'

