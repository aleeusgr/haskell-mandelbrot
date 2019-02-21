import Data.Complex
import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)
--import Graphics.Rasterific.Texture

type Word8Color = (Word8,Word8,Word8,Word8)


{- mandel z c 
   DESCRIPTION: Calculates one iteration of the formula for numbers in the Mandelbrot set
   PRE: TRUE
   RETURNS: A number in the Mandelbrot set, based on z and c
   EXAMPLES: mandel (4 :+ 2) (2 :+ 3) -> 14.0 :+ 19.0
-}
mandel :: RealFloat a => Complex a -> Complex a -> Complex a
mandel z c = (z*z) + c

{- pixelCheck z  
   DESCRIPTION: Calculates how many iterations of the Mandelbrot formula it takes for z to ?????????????????????????????????????
   PRE: TRUE
   RETURNS: The number of iterations it takes for z to grow beyond the size of what is allowed in a Mandelbrot set
   EXAMPLES: pixelCheck (0.05 :+ 0.9) -> 5
-}
pixelCheck :: RealFloat a => Complex a -> Int
pixelCheck z = pixelCheckAux mandel 0 255 z z

{- pixelCheckAux f currIt maxIt z c
   DESCRIPTION: Calculates how many iterations of f it takes for z to ???????????????????????????????????????????????????????????
   PRE: TRUE
   RETURNS: currIt, i.e. the number of iterations passed
   EXAMPLES: pixelCheckAux mandel 0 255 (0.05 :+ 0.9) (0.05 :+ 0) -> 255
   VARIANT: ???????????????????????????????????????????????????????????
-}
pixelCheckAux :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Int -> Int -> Complex a -> Complex a -> Int
pixelCheckAux f currIt maxIt z c
  | currIt >= maxIt = currIt
  | magnitude(z) > 2 = currIt
  | otherwise = pixelCheckAux f (currIt+1) maxIt (f z c) c

{- cordToComp (x,y) res
   DESCRIPTION: Creates a complex number based on the coordinates (x,y), where x and y are specific pixels
   PRE: res is positive ????????????????????????????????????????????????????????????????????
   RETURNS: A comlex number representing coordinates
   EXAMPLES: cordToComp (250,100) 400 -> 0.625 :+ 0.25
-}
cordToComp :: RealFloat a => (a, a) -> a -> Complex a
cordToComp (x,y) res = ((x/res) :+ (y/res))

{- iterationList (x,y)
   DESCRIPTION: 
   PRE: x < 399, y > -401 ??????????????????????????????????????????
   RETURNS: 
   EXAMPLES: 
   VARIANT: y ?????????????????????????????????????????????????
-}
iterationList :: (Int, Int) -> [Int]
iterationList (399,-401) = []
iterationList (x,y) 
    | x < 399 = iterations : iterationList (x+1,y)
    | x == 399 = iterations : iterationList (-400, y-1)
  where iterations = pixelCheck (cordToComp ((fromIntegral x)::Float,(fromIntegral y)::Float) 200)

{- createRGBA (x:xs) ls
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
createRGBA :: [Int] -> [Word8]-> [Word8]
createRGBA [] _ = []
createRGBA (x:xs) ls = (ls !! (x*4)):(ls !! (x*4+1)):(ls !! (x*4+2)):(ls !! (x*4+3)):(createRGBA xs ls)
  

{- twoCGradient c1 c2
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
twoCGradient :: Word8Color -> Word8Color -> [Word8]
twoCGradient c1@(r1,g1,b1,a1) c2@(r2,g2,b2,a2)
  | (r1,g1,b1) == (r2,g2,b2) = [r1,g1,b1,a1]
  | otherwise = r1:g1:b1:255:(twoCGradient ((stepTo r1 r2), (stepTo g1 g2), (stepTo b1 b2), 255) c2)

{- stepTo w1 w2
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
stepTo :: Word8 -> Word8 -> Word8
stepTo x y
  | x == y = x
  | x < y = x + 1
  | otherwise = x - 1

--------------------------------------------------------------------------------------------------------------------------------------

picture = bitmapOfByteString 800 800 (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList (-400,400)) (twoCGradient (255,255,255,255) (0,0,0,255)))) True
main = display (InWindow "Epic Gamer Window" (800, 800) (10, 10)) white picture

