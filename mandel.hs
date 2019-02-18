import Data.Complex
import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)

---------------------------------------------------------------------------------------------------------------------------------

purple :: [Word8]
purple = [128, 0, 128, 64]

bitmapData :: ByteString
bitmapData = pack $ take 3999996 (cycle purple)

ourPicture :: Picture
ourPicture = bitmapOfByteString 1000 1000 (BitmapFormat TopToBottom PxRGBA) bitmapData True

--main = display (InWindow "Nice Window" (1000, 1000) (10, 10)) white ourPicture

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


cord_to_comp :: RealFloat a => (a, a) -> a -> Complex a
cord_to_comp (x,y) res = ((x/res) :+ (y/res))


iterationList :: (Int, Int) -> [Int]
iterationList (399,-401) = []
iterationList (x,y) 
    | x < 399 = iterations : iterationList (x+1,y)
    | x == 399 = iterations : iterationList (-400, y-1)
  where iterations = superpixelcheck (cord_to_comp ((fromIntegral x)::Float,(fromIntegral y)::Float) 200)


createRGBA :: [Int] -> [Word8]
createRGBA [] = []
createRGBA (x:xs) = [0,0,0,(fromIntegral(x::Int)::Word8)] ++ createRGBA (xs)


picture = bitmapOfByteString 800 800 (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList(-400,400)))) True
main = display (InWindow "Nice Window" (800, 800) (10, 10)) white picture

