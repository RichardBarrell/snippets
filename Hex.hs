-- can't think why I wrote this
-- surely there's already a library on hackage

import Data.ByteString as B
import Data.Word (Word8)

c2w :: Char -> Word8
c2w = toEnum . fromEnum

rh :: Word8 -> Word8
rh c | c >= (c2w '0') && c <= (c2w '9') = c - (c2w '0') + 0
     | c >= (c2w 'a') && c <= (c2w 'f') = c - (c2w 'a') + 10
     | c >= (c2w 'A') && c <= (c2w 'F') = c - (c2w 'A') + 10
     | otherwise = error "c is not a digit"

hexStep :: B.ByteString -> Maybe (Word8, B.ByteString)
hexStep b = if B.null b then Nothing else Just (c, B.drop 2 b) where
    c = 16 * rh (b `B.index` 0) + rh (b `B.index` 1)

hexDecode :: B.ByteString -> B.ByteString
hexDecode str = fst $ B.unfoldrN (B.length str `div` 2) hexStep str
