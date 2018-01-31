{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

--module ResponseHeader where
module Main where

import Test.QuickCheck

import qualified Data.ByteString as S
import qualified Data.CaseInsensitive as CI
import Foreign.Ptr
import GHC.Storable
import qualified Network.HTTP.Types as H

import Data.ByteString.Internal (create)

import Network.Wai.Handler.Warp.Buffer (copy)
import Network.Wai.Handler.Warp.Imports

----------------------------------------------------------------

httpVer11 :: ByteString
httpVer11 = "HTTP/1.1 "

httpVer10 :: ByteString
httpVer10 = "HTTP/1.0 "

--bytestring builder could aviod extraneous allocations
--Note composeHeaderPure calls copyStatusPure
--
-- Is is enough to test composeHeader? I have changed the types of the helper functions
composeHeaderPure :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeaderPure !httpversion !status !responseHeaders = return $
    copyCRLFPure $ ((copyStatusPure httpversion status) `S.append`
                    copyHeadersPure responseHeaders)

composeHeader :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeader !httpversion !status !responseHeaders = create len $ \ptr -> do
    ptr1 <- copyStatus ptr httpversion status
    ptr2 <- copyHeaders ptr1 responseHeaders
    void $ copyCRLF ptr2
  where
    !len = 17 + slen + foldl' fieldLength 0 responseHeaders
    fieldLength !l (!k,!v) = l + S.length (CI.original k) + S.length v + 4
    !slen = S.length $ H.statusMessage status

{-# INLINE copyStatusPure #-}
copyStatusPure :: H.HttpVersion -> H.Status -> ByteString
copyStatusPure !httpversion !status = copyCRLFPure $
    httpVer `S.append` rest 
  where
    rest = 
      (S.pack $ map (\x -> zero + fromIntegral x) [r2, r1, r0]) `S.append`
      (spc `S.cons` H.statusMessage status)
    httpVer
      | httpversion == H.HttpVersion 1 1 = httpVer11
      | otherwise = httpVer10
    (q0,r0) = H.statusCode status `divMod` 10
    (q1,r1) = q0 `divMod` 10
    r2 = q1 `mod` 10

{-# INLINE copyStatus #-}
copyStatus :: Ptr Word8 -> H.HttpVersion -> H.Status -> IO (Ptr Word8)
copyStatus !ptr !httpversion !status = do
    ptr1 <- copy ptr httpVer
    writeWord8OffPtr ptr1 0 (zero + fromIntegral r2)
    writeWord8OffPtr ptr1 1 (zero + fromIntegral r1)
    writeWord8OffPtr ptr1 2 (zero + fromIntegral r0)
    writeWord8OffPtr ptr1 3 spc
    ptr2 <- copy (ptr1 `plusPtr` 4) (H.statusMessage status)
    copyCRLF ptr2
  where
    httpVer
      | httpversion == H.HttpVersion 1 1 = httpVer11
      | otherwise = httpVer10
    (q0,r0) = H.statusCode status `divMod` 10
    (q1,r1) = q0 `divMod` 10
    r2 = q1 `mod` 10

{-# INLINE copyHeadersPure #-}
copyHeadersPure :: [H.Header] -> ByteString
copyHeadersPure [] = S.empty
copyHeadersPure (h:hs) = copyHeaderPure h `S.append` copyHeadersPure hs

{-# INLINE copyHeaders #-}
copyHeaders :: Ptr Word8 -> [H.Header] -> IO (Ptr Word8)
copyHeaders !ptr [] = return ptr
copyHeaders !ptr (h:hs) = do
    ptr1 <- copyHeader ptr h
    copyHeaders ptr1 hs


{-# INLINE copyHeaderPure #-}
copyHeaderPure :: H.Header -> ByteString
copyHeaderPure (k,v) = 
    let bs = CI.original k `S.append` (colon `S.cons` ( spc `S.cons` v)) in
    copyCRLFPure bs


{-# INLINE copyHeader #-}
copyHeader :: Ptr Word8 -> H.Header -> IO (Ptr Word8)
copyHeader !ptr (k,v) = do
    ptr1 <- copy ptr (CI.original k)
    writeWord8OffPtr ptr1 0 colon
    writeWord8OffPtr ptr1 1 spc
    ptr2 <- copy (ptr1 `plusPtr` 2) v
    copyCRLF ptr2


{-# INLINE copyCRLFPure #-}
copyCRLFPure :: ByteString -> ByteString
copyCRLFPure bs = (bs `S.snoc` cr) `S.snoc` lf

{-# INLINE copyCRLF #-}
copyCRLF :: Ptr Word8 -> IO (Ptr Word8)
copyCRLF !ptr = do
    writeWord8OffPtr ptr 0 cr
    writeWord8OffPtr ptr 1 lf
    return $! ptr `plusPtr` 2


zero :: Word8
zero = 48
spc :: Word8
spc = 32
colon :: Word8
colon = 58
cr :: Word8
cr = 13
lf :: Word8
lf = 10


main = quickCheckAll
