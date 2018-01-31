{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.ResponseHeader (composeHeader) where

import qualified Data.ByteString as S
import qualified Data.CaseInsensitive as CI
import Foreign.Ptr
import GHC.Storable
import qualified Network.HTTP.Types as H

import Network.Wai.Handler.Warp.Buffer (copy)
import Network.Wai.Handler.Warp.Imports

----------------------------------------------------------------

httpVer11 :: ByteString
httpVer11 = "HTTP/1.1 "

httpVer10 :: ByteString
httpVer10 = "HTTP/1.0 "


--bytestring builder could aviod extraneous allocations
composeHeader :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeader !httpversion !status !responseHeaders = return $
    copyCRLF $ ((copyStatus httpversion status) `S.append`
                    copyHeaders responseHeaders)

{-# INLINE copyStatus #-}
copyStatus :: H.HttpVersion -> H.Status -> ByteString
copyStatus !httpversion !status = copyCRLF $
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

{-# INLINE copyHeaders #-}
copyHeaders :: [H.Header] -> ByteString
copyHeaders [] = S.empty
copyHeaders (h:hs) = copyHeader h `S.append` copyHeaders hs

{-# INLINE copyHeader #-}
copyHeader :: H.Header -> ByteString
copyHeader (k,v) = 
    let bs = CI.original k `S.append` (colon `S.cons` ( spc `S.cons` v)) in
    copyCRLF bs

{-# INLINE copyCRLF #-}
copyCRLF :: ByteString -> ByteString
copyCRLF bs = (bs `S.snoc` cr) `S.snoc` lf

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


