{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Network.Wai.Handler.Warp.RequestHeader (
      parseHeaderLines
    ) where

import Control.Exception (throwIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8 (unpack)
import qualified Data.CaseInsensitive as CI
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.Storable (peek)
import qualified Network.HTTP.Types as H

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

parseHeaderLines :: [ByteString]
                 -> IO (H.Method
                       ,ByteString  --  Path
                       ,ByteString  --  Path, parsed
                       ,ByteString  --  Query
                       ,H.HttpVersion
                       ,H.RequestHeaders
                       )
parseHeaderLines [] = throwIO $ NotEnoughLines []
parseHeaderLines (firstLine:otherLines) = do
    (method, path', query, httpversion) <- parseRequestLine firstLine
    let path = H.extractPath path'
        hdr = map parseHeader otherLines
    return (method, path', path, query, httpversion, hdr)

----------------------------------------------------------------
parseRequestLine :: ByteString
                  -> IO (H.Method
                        ,ByteString -- Path
                        ,ByteString -- Query
                        ,H.HttpVersion)
parseRequestLine requestLine = do
    let (method,rest) = S.break (==32) requestLine -- ' '
        (pathQuery,httpVer') = S.break (==32) (S.drop 1 rest) -- ' '
        httpVer = S.drop 1 httpVer'
    when (rest == "" || httpVer == "") $
        throwIO $ BadFirstLine $ C8.unpack requestLine
    let (path,query) = S.break (==63) pathQuery -- '?'
        (http,ver)   = S.break (==47) httpVer -- '/'
    when (http /= "HTTP") $ throwIO NonHttp
    let hv | ver == "/1.1" = H.http11
           | otherwise     = H.http10
    return $ (method,path,query,hv)

----------------------------------------------------------------

-- |
--
-- >>> parseHeader "Content-Length:47"
-- ("Content-Length","47")
-- >>> parseHeader "Accept-Ranges: bytes"
-- ("Accept-Ranges","bytes")
-- >>> parseHeader "Host:  example.com:8080"
-- ("Host","example.com:8080")
-- >>> parseHeader "NoSemiColon"
-- ("NoSemiColon","")

parseHeader :: ByteString -> H.Header
parseHeader s =
    let (k, rest) = S.break (== 58) s -- ':'
        rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
     in (CI.mk k, rest')
