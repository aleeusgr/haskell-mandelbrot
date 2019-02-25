import Data.Complex
import Graphics.Gloss
import Data.Word
import Graphics.Gloss.Interface.Pure.Game
import Data.ByteString (ByteString, pack)

type Word8Color = (Word8,Word8,Word8,Word8)


{- mandel z c 
   DESCRIPTION: Calculates one iteration of the formula for numbers in the Mandelbrot set
   RETURNS: A number in the Mandelbrot set, based on z and c
   EXAMPLES: mandel (4 :+ 2) (2 :+ 3) -> 14.0 :+ 19.0
-}
mandel :: RealFloat a => Complex a -> Complex a -> Complex a
mandel z c = (z*z) + c

{- iterationCheck z maxIt
   DESCRIPTION: Calculates how many iterations of the Mandelbrot formula it takes for z to approach infinity
   PRE: maxIt > 0
   RETURNS: The number of iterations it takes for z to grow beyond the size of what is allowed in a Mandelbrot set
   EXAMPLES: iterationCheck (0.05 :+ 0.9) 255 -> 5
-}
iterationCheck :: RealFloat a => Complex a -> Int -> Int
iterationCheck z maxIt = iterationCheckAux mandel 0 maxIt z z

{- iterationCheckAux f currIt maxIt z c
   DESCRIPTION: Calculates how many iterations of f it takes for z to approach infinity
   PRE: maxIt > 0, 
   RETURNS: currIt, i.e. the number of iterations passed
   EXAMPLES: iterationCheckAux mandel 0 255 (0.05 :+ 0.9) (0.05 :+ 0) -> 255
   VARIANT: maxIt - currIt
-}
iterationCheckAux :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Int -> Int -> Complex a -> Complex a -> Int
iterationCheckAux f currIt maxIt z c
  | currIt >= maxIt = currIt
  | magnitude(z) > 2 = currIt
  | otherwise = iterationCheckAux f (currIt+1) maxIt (f z c) c

{- coordToComp pix cent res@(rx,ry) zm
   DESCRIPTION: Converts an on-screen pixel to a coordinate in the complex number plane.
   PRE: rx /= 0, ry /= 0, zm /= 0
   RETURNS: The complex number located at position pix on a view of the complex number plane
     with resolution res centered on the complex number cam with zoom factor zm. ??????????
   EXAMPLES: cordToComp (250,100) 400 -> 0.625 :+ 0.25
-}
coordToComp :: RealFloat a => (a, a) -> (a, a) -> (a, a) -> a -> Complex a
coordToComp (px,py) (cx,cy) (rx,ry) zm = 
  let
    aux p r c z = ((p - (r / 2)) * 2) / (r * z) + c
    rm = max rx ry
  in
    (aux px rm cx zm) :+ (-(aux py rm cy zm))

{- iterationList res cent zoom max_it
   DESCRIPTION: Applies the Mandelbrot formula to each pixel on a screen and gives the amount of iterations it takes for each pixel to begin approaching infinity.
   PRE: max_it > 0
   RETURNS: A list of each pixel represented by the number of iterations it takes to reach it
   EXAMPLES: iterationList (10,10) (0,0) 0.5 255 -> [0,0,0,0,0,1,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,1,1,2,2,2,1,1,1,1,0,2,2,3,5,17,3,1,1,1,0,2,6,6,255,255,8,2,1,1,255,255,255,255,255,255,6,2,1,1,0,2,6,6,255,255,8,2,1,1,0,2,2,3,5,17,3,1,1,1,0,1,1,2,2,2,1,1,1,1,0,0,1,1,1,1,1,1,1,0]
-}
iterationList :: RealFloat a => (Int, Int) -> (a, a) -> a -> Int -> [Int]
iterationList res@(rx,ry) cent zoom max_it = iterationListAux (0,0) cent res zoom max_it

{- iterationListAux p cent res zoom
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: iterationListAux 
   VARIANT: 
-}
iterationListAux :: RealFloat a => (Int, Int) -> (a, a) -> (Int, Int) -> a -> Int -> [Int]
iterationListAux p@(px,py) cent@(cx,cy) res@(rx,ry) zoom it 
  | py >= ry = []
  | px >= rx = iterationListAux (0, py+1) cent res zoom it
  | otherwise = (iterationCheck (coordToComp (fromIntegral px, fromIntegral py) cent (fromIntegral rx,fromIntegral ry) zoom) it) : (iterationListAux (px+1,py) cent res zoom it)

{- createRGBA iter ls
   DESCRIPTION: Converts a list of iterations to a graphical representation 
   PRE: 0 <= x < length ls for all x that are elements of iter
   RETURNS: ByteString of rgba-colors from ls matched to elements of iter.
   EXAMPLES: createRGBA [1,0,2,2,1] [(255,0,0,255),(0,255,0,255),(0,0,255,255)] -> [0,255,0,255,255,0,0,255,0,0,255,255,0,0,255,255,0,255,0,255]
   VARIANT: length iter
-}
createRGBA :: [Int] -> [Word8Color] -> [Word8]
createRGBA [] _ = []
createRGBA (x:xs) ls =
  let
    (r,g,b,a) = (ls !! x)
  in
    r : g : b : a : (createRGBA xs ls)

{- cap xs x i
   DESCRIPTION: Ends a list at a certain index with a specific element.
   PRE: length xs >= i
   RETURNS: The first i elements of xs, with x appended.
   EXAMPLES: [7,5,6,4,5] 0 2 -> [7,5,2]
 -}
cap :: [a] -> a -> Int -> [a]
cap xs x i = take i xs ++ [x]


{- cycleGrad l s
   DESCRIPTION: Takes a gradient, connects the last color to the first, and cycles it infinitely
   PRE: length l >= 2 
   RETURNS: An infinite list of a repeating color gradient 
-}
cycleGrad :: [Word8Color] -> Word8 -> [Word8Color]
cycleGrad l@(c:cs) s = cycle $ (gradient l s) ++ (twoCGradient (last cs) c s)

{- gradient c s
   DESCRIPTION: Creates a complete gradient between multiple colors
   PRE: length c >= 2
   RETURNS: A color gradient between all the colors of c
   EXAMPLES: gradient [(0,0,0,255),(50,40,60,255),(90,100,110,255)] 10 -> [0,0,0,255,10,10,10,255,20,20,20,255,30,30,30,255,40,40,40,255,50,40,50,255,50,40,60,255,60,50,70,255,70,60,80,255,80,70,90,255,90,80,100,255,90,90,110,255]
   VARIANT: length c
-}
gradient :: [Word8Color] -> Word8 -> [Word8Color]
gradient [c1,c2] s = twoCGradient c1 c2 s
gradient c@(c1:c2:cs) s = (twoCGradient c1 c2 s) ++ (gradient (c2:cs) s)

{- twoCGradient c1 c2 s
   DESCRIPTION: Creates a gradient between two colors
   PRE: s > 0
   RETURNS: A list of colors (rgba-format) as a spectrum from c1 to c2 with difference s of every r,g,b values between the different colors
   EXAMPLES: twoCGradient (50,100,200,255) (60,80,100,255) 5 -> [50,100,200,255,55,95,195,255,60,90,190,255,60,85,185,255,60,80,180,255,60,80,175,255,60,80,170,255,60,80,165,255,60,80,160,255,60,80,155,255,60,80,150,255,60,80,145,255,60,80,140,255,60,80,135,255,60,80,130,255,60,80,125,255,60,80,120,255,60,80,115,255,60,80,110,255,60,80,105,255]
   VARIANT: The greatest difference between the components of c1 and c2
-}
twoCGradient :: Word8Color -> Word8Color -> Word8 -> [Word8Color]
twoCGradient c1@(r1,g1,b1,a1) c2@(r2,g2,b2,a2) s
  | (r1,g1,b1,a1) == (r2,g2,b2,a1) = []
  | otherwise = c1 : (twoCGradient ((stepTo r1 r2 s), (stepTo g1 g2 s), (stepTo b1 b2 s), (stepTo a1 a2 s)) c2 s)

{- stepTo x y s 
   DESCRIPTION: Makes x approach y through adding a certain step-size s
   PRE: s > 0
   RETURNS: x + s if x < y, x - s if x > y
   EXAMPLES: stepTo 1 6 2 -> 3
-}
stepTo :: Word8 -> Word8 -> Word8 -> Word8
stepTo x y s
  | abs ((toInteger x) - (toInteger y)) <= (toInteger s) = y
  | x > y = x - s
  | otherwise = x + s

--------------------------------------------------------------------------------------------------------------------------------------

--picture it = bitmapOfByteString 600 600 (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList (600, 600) (0,0) 0.5 it) (cap (cycleGrad [(255,0,0,255),(255,255,0,255),(0,255,0,255),(0,255,255,255),(0,0,255,255),(255,0,255,255)] 8) (0,0,0,255) it))) True
--main = display (InWindow "Epic Insane Gamer Window" (600, 600) (20, 20)) white $ picture 127 


picture (it,zoom,x,y) = bitmapOfByteString 400 400 (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList (400, 400) (x, y) zoom it) (cap (cycleGrad [(255,255,255,255),(255,0,0,255),(255,255,0,255),(0,255,0,255),(0,255,255,255),(0,0,255,255),(255,0,255,255)] 8) (0,0,0,255) it))) True 

window :: Display
window = InWindow "Epic Insane Gamer Window" (400, 400) (10, 10)

handlekeys (EventKey (MouseButton LeftButton) Down _ (x',y')) (it, zoom,x'',y'') =
  let x= realPart(coordToComp (x',y') (x'',y'') (400,400) zoom)
      y= imagPart(coordToComp (x',y') (x'',y'') (400,400) zoom)
    in(127, zoom*1.25,x,y)
handlekeys _ current = current

main = play window white 30 (127,0.5,0,0) (picture) (handlekeys) (const id)
