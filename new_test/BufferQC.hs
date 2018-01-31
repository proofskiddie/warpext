{-# LANGUAGE BangPatterns #-}

module BufferQC where

--import Test.QuickCheck
import Test.QuickCheck.Monadic

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types
import qualified Data.ByteString as BS
import Data.ByteString.Internal (memcpy)
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Streaming.ByteString.Builder.Buffer as B (Buffer (..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (mallocBytes, free, finalizerFree)
import Foreign.Ptr (castPtr, plusPtr)

-- added
import Foreign.Marshal.Utils (copyBytes)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (castPtr, plusPtr)

-- utils 

-- | Allocating a buffer with malloc().
allocateBuffer :: Int -> IO Buffer
allocateBuffer = mallocBytes


-- | Copying the bytestring to the buffer.
--   This function returns the point where the next copy should start.
copyPure :: Buffer -> ByteString -> IO Buffer
copyPure !ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $! ptr `plusPtr` l
{-# INLINE copyPure #-}

copy :: Buffer -> ByteString -> IO Buffer
copy !ptr bs = BS.useAsCStringLen bs $ \(src, len) -> do
    copyBytes ptr (castPtr src) len
    return $! ptr `plusPtr` (BS.length bs)
{-# INLINE copy #-}

-- tricky to test. Buffer is just a c style ptr to word8 and so 
-- does not really contain much information about what it contains
-- i.e. size
-- although there is a 'bufsize' defined in either Types or Buffer.hs
--
-- need to define a size and pass the buffer to a PS constructor

propCopy :: Buffer -> ByteString -> PropertyM m0 ()
propCopy ptr bs = monadicIO $ do
  bPure <- copyPure ptr bs
  b  <- copy ptr bs
--  cPure <- newForeignPtr_ bPure
--  c <- newForeignPtr_ b
  let x1 = PS bPure 0 (length bs)
      x2 = PS b 0 (length bs)
  assert $ (x1 == x2)
mainCopy = quickCheck propCopy

mallocBSPure :: Int -> IO ByteString
mallocBSPure size = do
    ptr <- allocateBuffer size
    fptr <- newForeignPtr finalizerFree ptr
    return $! PS fptr 0 size
{-# INLINE mallocBSPure #-}

mallocBS :: Int -> IO ByteString
mallocBS size = do
    ptr <- allocateBuffer size
    BS.packCStringLen (castPtr ptr, size)
{-# INLINE mallocBS #-}

bufferIOPure :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIOPure ptr siz io = do
    fptr <- newForeignPtr_ ptr
    io $ PS fptr 0 siz

bufferIO :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIO ptr siz io = do
    bs <- BS.packCStringLen (castPtr ptr, siz)
    io bs

