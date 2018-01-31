--file--
-- Buffer
   -- copy
   -- BufferIO
   -- withForeignBuffer
   -- mallocBS

{- Dependency on ByteString.Internal in the form of PS (ByteString constructor) and memcpy (C lang function
   supplied by ByteString.Internal -}

copy :: Buffer -> ByteString -> IO Buffer
copy !ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $! ptr `plusPtr` l
{-# INLINE copy #-}

{- function that gave me the most trouble, seemed that memory had to be directly accessed to implement.
   solved by using the bytestring funciton 'useAsCStringLen' which 
   "O(n) construction Use a ByteString with a function requiring a CStringLen. As for useAsCString 
    this function makes a copy of the original ByteString. It must not be stored or used after the 
    subcomputation finishes."
   Function returns the position in the buffer after writing. Even though writing to the buffer is impure
   purity is kept whithin the bytestring interface. -}

copyPure :: Buffer -> ByteString -> IO Buffer
copyPure !ptr bs = BS.useAsCStringLen bs $ \(src, len) -> do
    copyBytes ptr (castPtr src) len
    return $! ptr `plusPtr` (BS.length bs)
{-# INLINE copyPure #-}

mallocBS :: Int -> IO ByteString
mallocBS size = do
    ptr <- allocateBuffer size
    fptr <- newForeignPtr finalizerFree ptr
    return $! PS fptr 0 size
{-# INLINE mallocBS #-}

{- using packCStringLen allows safe creation of a bytestring from a c string (a buffer object in this case)
   casting the Buffer:: Ptr Word8 allows use of the memory as a c string (Array of bytes) -}

mallocBSPure :: Int -> IO ByteString
mallocBSPure size = do
    ptr <- allocateBuffer size
    BS.packCStringLen (castPtr ptr, size)
{-# INLINE mallocBSPure #-}

bufferIO :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIO ptr siz io = do
    fptr <- newForeignPtr_ ptr
    io $ PS fptr 0 siz

{- preforms basically the same action as mallocBS but calls the supplied (ByteString -> IO()) action 
   before returning -}

bufferIOPure :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIOPure ptr siz io = do
    bs <- BS.packCStringLen (castPtr ptr, siz)
    io bs

withForeignBuffer :: ByteString -> ((Buffer, BufSize) -> IO Int) -> IO Int
withForeignBuffer (PS ps s l) f = withForeignPtr ps $ \p -> f (castPtr p `plusPtr` s, l)
{-# INLINE withForeignBuffer #-}

{- passing BufSize proved redundant as that information is available in BS.length bs
   might be able to use BufSize directly if the types match up -}

withForeignBufferPure :: ByteString -> ((Buffer, BufSize) -> IO Int) -> IO Int
withForeignBufferPure bs f = BS.useAsCStringLen bs $ \p -> f ((castPtr . fst) p, BS.length bs)
{-# INLINE withForeignBufferPure #-}

--file--
-- PackInt

-- $setup
-- >>> import Data.ByteString.Char8 as B
-- >>> import Test.QuickCheck (Large(..))

-- |
--
-- prop> packIntegral (abs n) == B.pack (show (abs n))
-- prop> \(Large n) -> let n' = fromIntegral (abs n :: Int) in packIntegral n' == B.pack (show n')

packIntegral :: Integral a => a -> ByteString
packIntegral 0 = "0"
packIntegral n | n < 0 = error "packIntegral"
packIntegral n = unsafeCreate len go0
  where
    n' = fromIntegral n + 1 :: Double
    len = ceiling $ logBase 10 n'
    go0 p = go n $ p `plusPtr` (len - 1)
    go :: Integral a => a -> Ptr Word8 -> IO ()
    go i p = do
        let (d,r) = i `divMod` 10
        poke p (48 + fromIntegral r)
        when (d /= 0) $ go d (p `plusPtr` (-1))

{-# SPECIALIZE packIntegral :: Int -> ByteString #-}
{-# SPECIALIZE packIntegral :: Integer -> ByteString #-}

{- implementation runs faster than the unsafe version above, with apparently less memory usage -}

packIntegral :: Integral a => a -> ByteString
packIntegral 0 = "0"
packIntegral n | n < 0 = error "packIntegral"
packIntegral n = Data.ByteString.unfoldr ana n
  where
    ana 0 = Nothing
    ana n = Just (fromIntegral $ 48 + (rem n 10), div n 10)

{-# SPECIALIZE packIntegral :: Int -> ByteString #-}
{-# SPECIALIZE packIntegral :: Integer -> ByteString #-}

-- |
--
-- >>> packStatus H.status200
-- "200"
-- >>> packStatus H.preconditionFailed412
-- "412"

packStatus :: H.Status -> ByteString
packStatus status = unsafeCreate 3 $ \p -> do
    poke p               (toW8 r2)
    poke (p `plusPtr` 1) (toW8 r1)
    poke (p `plusPtr` 2) (toW8 r0)
  where
    toW8 :: Int -> Word8
    toW8 n = 48 + fromIntegral n
    !s = fromIntegral $ H.statusCode status
    (!q0,!r0) = s `divMod` 10
    (!q1,!r1) = q0 `divMod` 10
    !r2 = q1 `mod` 10

-- |
--
-- >>> packStatus H.status200
-- "200"
-- >>> packStatus H.preconditionFailed412
-- "412"

packStatus :: H.Status -> ByteString
packStatus status = foldr Data.ByteString.cons Data.ByteString.empty [r2,r1,r0]
  where
    !s = fromIntegral $ H.statusCode status
    (!q0,!r0) = s `divMod` 10
    (!q1,!r1) = q0 `divMod` 10
    !r2 = q1 `mod` 10

--file--
-- RequestHeader

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

parseHeaderLinesPure :: [ByteString]
                 -> IO (H.Method
                       ,ByteString  --  Path
                       ,ByteString  --  Path, parsed
                       ,ByteString  --  Query
                       ,H.HttpVersion
                       ,H.RequestHeaders
                       )
parseHeaderLinesPure [] = throwIO $ NotEnoughLines []
parseHeaderLinesPure (firstLine:otherLines) = do
    (method, path', query, httpversion) <- parseRequestLinePure firstLine
    let path = H.extractPath path'
        hdr = map parseHeader otherLines
    return (method, path', path, query, httpversion, hdr)

-- |
--
-- >>> parseRequestLine "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine "GET "
-- *** Exception: Warp: Invalid first line of request: "GET "
-- >>> parseRequestLine "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: Warp: Request line specified a non-HTTP request
-- >>> parseRequestLine "PRI * HTTP/2.0"
-- ("PRI","*","",HTTP/2.0)
parseRequestLine :: ByteString
                 -> IO (H.Method
                       ,ByteString -- Path
                       ,ByteString -- Query
                       ,H.HttpVersion)
parseRequestLine requestLine@(PS fptr off len) = withForeignPtr fptr $ \ptr -> do
    when (len < 14) $ throwIO baderr
    let methodptr = ptr `plusPtr` off
        limptr = methodptr `plusPtr` len
        lim0 = fromIntegral len

    pathptr0 <- memchr methodptr 32 lim0 -- ' '
    when (pathptr0 == nullPtr || (limptr `minusPtr` pathptr0) < 11) $
        throwIO baderr
    let pathptr = pathptr0 `plusPtr` 1
        lim1 = fromIntegral (limptr `minusPtr` pathptr0)

    httpptr0 <- memchr pathptr 32 lim1 -- ' '
    when (httpptr0 == nullPtr || (limptr `minusPtr` httpptr0) < 9) $
        throwIO baderr
    let httpptr = httpptr0 `plusPtr` 1
        lim2 = fromIntegral (httpptr0 `minusPtr` pathptr)

    checkHTTP httpptr
    !hv <- httpVersion httpptr
    queryptr <- memchr pathptr 63 lim2 -- '?'

    let !method = bs ptr methodptr pathptr0
        !path
          | queryptr == nullPtr = bs ptr pathptr httpptr0
          | otherwise           = bs ptr pathptr queryptr
        !query
          | queryptr == nullPtr = S.empty
          | otherwise           = bs ptr queryptr httpptr0

    return (method,path,query,hv)
  where
    baderr = BadFirstLine $ B.unpack requestLine
    check :: Ptr Word8 -> Int -> Word8 -> IO ()
    check p n w = do
        w0 <- peek $ p `plusPtr` n
        when (w0 /= w) $ throwIO NonHttp
    checkHTTP httpptr = do
        check httpptr 0 72 -- 'H'
        check httpptr 1 84 -- 'T'
        check httpptr 2 84 -- 'T'
        check httpptr 3 80 -- 'P'
        check httpptr 4 47 -- '/'
        check httpptr 6 46 -- '.'
    httpVersion httpptr = do
        major <- peek (httpptr `plusPtr` 5) :: IO Word8
        minor <- peek (httpptr `plusPtr` 7) :: IO Word8
        let version
              | major == 49 = if minor == 49 then H.http11 else H.http10
              | major == 50 && minor == 48 = H.HttpVersion 2 0
              | otherwise   = H.http10
        return version
    bs ptr p0 p1 = PS fptr o l
      where
        o = p0 `minusPtr` ptr
        l = p1 `minusPtr` p0



parseRequestLinePure :: ByteString
                  -> IO (H.Method
                        ,ByteString -- Path
                        ,ByteString -- Query
                        ,H.HttpVersion)
parseRequestLinePure requestLine = do
    let (method,rest) = S.break (==32) requestLine -- ' '
        (pathQuery,httpVer') = S.break (==32) (S.drop 1 rest) -- ' '
        httpVer = S.drop 1 httpVer'
    when (rest == "" || httpVer == "") $
        throwIO $ BadFirstLine $ B.unpack requestLine
    let (path,query) = S.break (==63) pathQuery -- '?'
        (http,ver)   = S.break (==47) httpVer -- '/'
    when (http /= "HTTP") $ throwIO NonHttp
    let hv | ver == "/1.1" = H.http11
           | otherwise     = H.http10
    return $ (method,path,query,hv)


--file--
-- ResponseHeader


composeHeader :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeader !httpversion !status !responseHeaders = create len $ \ptr -> do
    ptr1 <- copyStatus ptr httpversion status
    ptr2 <- copyHeaders ptr1 responseHeaders
    void $ copyCRLF ptr2
  where
    !len = 17 + slen + foldl' fieldLength 0 responseHeaders
    fieldLength !l !(k,v) = l + S.length (CI.original k) + S.length v + 4
    !slen = S.length $ H.statusMessage status

composeHeaderPure :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeaderPure !httpversion !status !responseHeaders = return $
    copyCRLFPure $ ((copyStatusPure httpversion status) `S.append`
                    copyHeadersPure responseHeaders)


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

{-# INLINE copyHeaders #-}
copyHeaders :: Ptr Word8 -> [H.Header] -> IO (Ptr Word8)
copyHeaders !ptr [] = return ptr
copyHeaders !ptr (h:hs) = do
    ptr1 <- copyHeader ptr h
    copyHeaders ptr1 hs

{-# INLINE copyHeadersPure #-}
copyHeadersPure :: [H.Header] -> ByteString
copyHeadersPure [] = S.empty
copyHeadersPure (h:hs) = copyHeaderPure h `S.append` copyHeadersPure hs

{-# INLINE copyHeader #-}
copyHeader :: Ptr Word8 -> H.Header -> IO (Ptr Word8)
copyHeader !ptr (k,v) = do
    ptr1 <- copy ptr (CI.original k)
    writeWord8OffPtr ptr1 0 colon
    writeWord8OffPtr ptr1 1 spc
    ptr2 <- copy (ptr1 `plusPtr` 2) v
    copyCRLF ptr2

{-# INLINE copyHeaderPure #-}
copyHeaderPure :: H.Header -> ByteString
copyHeaderPure (k,v) = 
    let bs = CI.original k `S.append` (colon `S.cons` ( spc `S.cons` v)) in
    copyCRLFPure bs


{-# INLINE copyCRLF #-}
copyCRLF :: Ptr Word8 -> IO (Ptr Word8)
copyCRLF !ptr = do
    writeWord8OffPtr ptr 0 cr
    writeWord8OffPtr ptr 1 lf
    return $! ptr `plusPtr` 2

{-# INLINE copyCRLFPure #-}
copyCRLFPure :: ByteString -> ByteString
copyCRLFPure bs = (bs `S.snoc` cr) `S.snoc` lf

--file-- 
--   SendFile

readSendFile :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFile buf siz send fid off0 len0 hook headers = do
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    IO.withBinaryFile path IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek off0
        n <- IO.hGetBufSome h buf' (mini room len0)
        bufferIO buf (hn + n) send
        hook
        let n' = fromIntegral n
        fptr <- newForeignPtr_ buf
        loop h fptr (len0 - n')
  where
    path = fileIdPath fid
    loop h fptr len
      | len <= 0  = return ()
      | otherwise = do
        n <- IO.hGetBufSome h buf (mini siz len)
        when (n /= 0) $ do
            let bs = PS fptr 0 n
                n' = fromIntegral n
            send bs
            hook
            loop h fptr (len - n')

-- only the windows version of this file was impure

readSendFilePure :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFilePure buf siz send fid off0 len0 hook headers = do
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    IO.withBinaryFile path IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek off0
        n <- IO.hGetBufSome h buf' (mini room len0)
        bufferIO buf (hn + n) send
        hook
        let n' = fromIntegral n
        -- fptr <- newForeignPtr_ buf
	let fptr = buf
        loop h fptr (len0 - n')
  where
    path = fileIdPath fid
    loop h fptr len
      | len <= 0  = return ()
      | otherwise = do
        n <- IO.hGetBufSome h buf (mini siz len)
        when (n /= 0) $ do
            bs <- BS.packCStringLen (castPtr fptr, n)
            let n' = fromIntegral n
            send bs
            hook
            loop h fptr (len - n')

--file--
-- Recv

spell :: ByteString -> BufSize -> IO ByteString -> RecvBuf -> IO (ByteString, ByteString)
spell init0 siz0 recv recvBuf
  | siz0 <= len0 = return $ BS.splitAt siz0 init0
  -- fixme: hard coding 4096
  | siz0 <= 4096 = loop [init0] (siz0 - len0)
  | otherwise    = do
      bs@(PS fptr _ _) <- mallocBS siz0
      withForeignPtr fptr $ \ptr -> do
          ptr' <- copy ptr init0
          full <- recvBuf ptr' (siz0 - len0)
          if full then
              return (bs, "")
            else
              return ("", "") -- fixme
  where
    len0 = BS.length init0
    loop bss siz = do
        bs <- recv
        let len = BS.length bs
        if len == 0 then
            return ("", "")
          else if len >= siz then do
            let (consume, leftover) = BS.splitAt siz bs
                ret = BS.concat $ reverse (consume : bss)
            return (ret, leftover)
          else do
            let bss' = bs : bss
                siz' = siz - len
            loop bss' siz'


spellPure :: ByteString -> BufSize -> IO ByteString -> RecvBuf -> IO (ByteString, ByteString)
spellPure init0 siz0 recv recvBuf
  | siz0 <= len0 = return $ BS.splitAt siz0 init0
  -- fixme: hard coding 4096
  | siz0 <= 4096 = loop [init0] (siz0 - len0)
  | otherwise    = do
      bs <- mallocBS siz0
      BS.useAsCStringLen bs $ \ptr -> do 
     -- withForeignPtr fptr $ \ptr -> do
          ptr' <- copy ((castPtr . fst) ptr) init0
          full <- recvBuf ptr' (siz0 - len0)
          if full then
              return (bs, "")
            else
              return ("", "") -- fixme
  where
    len0 = BS.length init0
    loop bss siz = do
        bs <- recv
        let len = BS.length bs
        if len == 0 then
            return ("", "")
          else if len >= siz then do
            let (consume, leftover) = BS.splitAt siz bs
                ret = BS.concat $ reverse (consume : bss)
            return (ret, leftover)
          else do
            let bss' = bs : bss
                siz' = siz - len
            loop bss' siz'

