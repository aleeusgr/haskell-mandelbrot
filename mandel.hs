import Data.Complex
import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)

purple :: [Word8]
purple = [128, 0, 128, 64]

bitmapData :: ByteString
bitmapData = pack $ take 3999996 (cycle purple)

ourPicture :: Picture
ourPicture = bitmapOfByteString 1000 1000 (BitmapFormat TopToBottom PxRGBA) bitmapData True

main = display (InWindow "Nice Window" (1000, 1000) (10, 10)) white ourPicture

----------------------------------------------------------------------------------------------------------------------------------

mandel :: RealFloat a => Complex a -> Complex a -> Complex a
mandel z c = (z*z) + c

pixelcheck :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Int -> Int -> Complex a -> Complex a -> Int
pixelcheck f current max_it z c
  | current >= max_it = current
  | magnitude(z) > 2 = current
  | otherwise = pixelcheck f (current+1) max_it (f z c) c

superpixelcheck :: RealFloat a => Complex a -> Int
superpixelcheck z = pixelcheck mandel 0 255 z z

cord_to_comp :: RealFloat a =>(a, a) -> Complex a
cord_to_comp (x,y) = ((x/20) :+ (y/20))

asd :: (Int, Int) -> [Int]
asd (50,-51) = []
asd (x,y) 
    | x < 50 = iterations : asd (x+1,y)
    | x == 50 = iterations : asd (-50, y-1)
  where iterations = superpixelcheck (cord_to_comp (x,y))

asdf :: [Int] -> [Word8]
asdf (x:xs) = [255,0,0,(fromIntegral(x::Int)::Word8)] ++ asdf(xs)

picture = bitmapOfByteString 100 100 (BitmapFormat TopToBottom PxRGBA) (pack (asdf( asd(-50,50) ) ) ) True
main2 = display (InWindow "Nice Window" (100, 100) (10, 10)) black picture
