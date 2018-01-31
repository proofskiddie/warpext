{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Network.Wai.Handler.Warp.PackInt where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)
import qualified Network.HTTP.Types as H
import qualified Data.ByteString as B (cons, empty, unfoldr, reverse) 

import Network.Wai.Handler.Warp.Imports



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
packIntegral n = B.unfoldr ana n
  where
    ana :: Integral a => a -> Maybe (Word8, a)
    ana 0 = Nothing
    ana n = Just . first (fromIntegral . (+) 48) $ firstDigit n
    firstDigit n = divMod n (10 ^ (floor . logBase 10 . fromIntegral $ n))
    first f (x,y) = (f x, y)

{-# SPECIALIZE packIntegral :: Int -> ByteString #-}
{-# SPECIALIZE packIntegral :: Integer -> ByteString #-}

-- |
--
-- >>> packStatus H.status200
-- "200"
-- >>> packStatus H.preconditionFailed412
-- "412"

packStatus :: H.Status -> ByteString
packStatus status = foldr B.cons B.empty [r2,r1,r0]
  where
    !s = fromIntegral $ H.statusCode status
    (!q0,!r0) = s `divMod` 10
    (!q1,!r1) = q0 `divMod` 10
    !r2 = q1 `mod` 10
