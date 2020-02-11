{-# LANGUAGE ForeignFunctionInterface, ViewPatterns, RankNTypes, ScopedTypeVariables #-}

module PTrace where
import           Foreign
import           Foreign.C
import           Control.Exception
import           Foreign.Marshal.Alloc
import           Helper
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as B
type PID = Int
type RemoteAddress = Word
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

foreign import ccall unsafe "ptrace" c_ptrace :: CInt -> CInt -> CLong -> CLong -> IO CLong


ptrace :: TRACE_REQUEST -> Process -> RemoteAddress -> Word -> IO Word
ptrace req (Process pid) addr =
  fmap fromIntegral
    . c_ptrace (fromIntegral req) (fromIntegral pid) (fromIntegral addr)
    . fromIntegral

ptrace_peekdata :: Process -> RemoteAddress -> IO Word
ptrace_peekdata pid addr = ptrace pTRACE_PEEKDATA pid addr 0

ptrace_pokedata :: Process -> RemoteAddress -> Word -> IO ()
ptrace_pokedata pid addr w = ptrace pTRACE_POKEDATA pid addr w >> return ()

with_process :: PID -> (Process -> IO r) -> IO r
with_process (Process -> p) = bracket attach detach
 where
  attach = ptrace pTRACE_ATTACH p 0 0 >> return p
  detach _ = ptrace pTRACE_DETACH p 0 0

peek_data :: forall  a . Storable a => Process -> RemoteAddress -> IO a
peek_data p addr = do
  (ptr, off, _) <- B.toForeignPtr <$> read_mem p addr (sizeOf (undefined :: a))
  withForeignPtr ptr (\p -> peekByteOff p off)

poke_data :: forall  a . Storable a => Process -> RemoteAddress -> a -> IO ()
poke_data p addr a = write_mem p addr
  $ B.unsafeCreate (sizeOf (undefined :: a)) (flip poke a . castPtr)

-- https://hackage.haskell.org/package/linux-ptrace-0.1.2/docs/src/System-Linux-Ptrace.html#peekBytes
read_mem :: Process -> RemoteAddress -> Int -> IO BS
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
write_mem :: Process -> RemoteAddress -> BS -> IO ()
write_mem pid addr bs = do
  s <- start
  e <- end
  doWrite (s `mappend` bs `mappend` e)
 where
  size        = B.length bs
  wordSize    = sizeOf addr
  alignedAddr = addr .&. complement (fromIntegral wordSize - 1)
  startBytes  = fromIntegral $ addr - alignedAddr
  endBytes    = mod size wordSize
  totalBytes  = size + startBytes + endBytes
  start       = read_mem pid alignedAddr startBytes
  end         = read_mem pid (alignedAddr + fromIntegral startBytes) endBytes

  writePtrs   = map
    fromIntegral
    [alignedAddr, alignedAddr + fromIntegral wordSize .. alignedAddr
    + fromIntegral totalBytes
    - 1]
  splitWords = map extractWord . chunksOf wordSize
  doWrite    = sequence_ . zipWith (ptrace_pokedata pid) writePtrs . splitWords

chunksOf :: Int -> BS -> [BS]
chunksOf n bs
  | B.null bs = []
  | otherwise = let (chunk, bs') = B.splitAt n bs in chunk : chunksOf n bs'

