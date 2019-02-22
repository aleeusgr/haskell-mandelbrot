import Data.Complex
import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)
--import Graphics.Rasterific.Texture

type Word8Color = (Word8,Word8,Word8,Word8)


{- mandel z c 
   DESCRIPTION: Calculates one iteration of the formula for numbers in the Mandelbrot set
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
pixelCheck :: RealFloat a => Complex a -> Int -> Int
pixelCheck z it = pixelCheckAux mandel 0 it z z

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

{- coordToComp pix cam res zm
   DESCRIPTION: Converts an on-screen pixel to a coordinate in the complex number plane.
   PRE: ????????????????????????????????????????
   RETURNS: The complex number located at position pix on a view of the complex number plane
     with resolution res centered on the complex number cam with zoom factor zm. ??????????
   EXAMPLES: cordToComp (250,100) 400 -> 0.625 :+ 0.25
-}
coordToComp :: RealFloat a => (a, a) -> (a, a) -> (a, a) -> a -> Complex a
coordToComp (px,py) (cx,cy) (rx,ry) zm = 
  let
    aux p r c z = (p * 2) / (r * z) + c
    rm = max rx ry
  in
    (aux px rm cx zm) :+ (aux py rm cy zm)

iterationList :: RealFloat a => (Int, Int) -> (a, a) -> a -> Int -> [Int]
iterationList r@(rx, ry) c z it = iterationListAux (-rx `div` 2, ry `div` 2 - 1 + (ry `mod` 2)) c r z it

{- iterationListAux (px,py) (cx,cy) (rx,ry) zm
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
iterationListAux :: RealFloat a => (Int, Int) -> (a, a) -> (Int, Int) -> a -> Int -> [Int]
iterationListAux p@(px,py) c@(cx,cy) r@(rx,ry) zm it 
  | py < (-ry) `div` 2 = []
  | px >= rx `div` 2 + (rx `mod` 2) = iterationListAux (-px + (rx `mod` 2), py - 1) c r zm it
  | otherwise = (pixelCheck (coordToComp (fromIntegral px, fromIntegral py) c (fromIntegral rx,fromIntegral ry) zm) it):(iterationListAux (px+1,py) c r zm it)

{- createRGBA (x:xs) ls
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
createRGBA :: [Int] -> Int -> [Word8] -> Word8Color -> [Word8]
createRGBA [] _ _ _ = []
createRGBA (x:xs) max_it ls c@(r,g,b,_)
  | x == max_it = r:g:b:255:(createRGBA xs max_it ls c)
  | otherwise = (ls !! (x*4)):(ls !! (x*4+1)):(ls !! (x*4+2)):(ls !! (x*4+3)):(createRGBA xs max_it ls c)
 

gradient :: [Word8Color] -> Word8 -> [Word8]
gradient [c1,c2] s = twoCGradient c1 c2 s
gradient (c1:c2:cs) s = (twoCGradient c1 c2 s) ++ (gradient (c2:cs) s)


gradientCycler :: [Word8Color] -> Word8 -> [Word8]
gradientCycler l@(c:cs) s = cycle $ (gradient l s) ++ (twoCGradient (last cs) c s)


{- twoCGradient c1 c2
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
twoCGradient :: Word8Color -> Word8Color -> Word8 -> [Word8]
twoCGradient c1@(r1,g1,b1,_) c2@(r2,g2,b2,_) s
  | (r1,g1,b1) == (r2,g2,b2) = []
  | otherwise = r1:g1:b1:255:(twoCGradient ((stepTo r1 r2 s), (stepTo g1 g2 s), (stepTo b1 b2 s), 255) c2 s)

{- stepTo w1 w2
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
stepTo :: Word8 -> Word8 -> Word8 -> Word8
stepTo x y s
  | abs ((toInteger x) - (toInteger y)) <= (toInteger s) = y
  | x > y = x - s
  | otherwise = x + s

--------------------------------------------------------------------------------------------------------------------------------------

picture it = bitmapOfByteString 400 400 (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList (400, 400) (-1.7495, 0) 256 it) it (gradientCycler [(255,255,255,255),(255,0,0,255),(255,255,0,255),(0,255,0,255),(0,255,255,255),(0,0,255,255),(255,0,255,255)] 64) (0,0,0,255))) True
main = display (InWindow "Epic Insane Gamer Window" (400, 400) (10, 10)) white $ picture 127 

