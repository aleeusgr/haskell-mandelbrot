import Data.Complex
import Graphics.Gloss
import Data.Word
import Graphics.Gloss.Interface.Pure.Game
import Data.ByteString (ByteString, pack)
import Data.Char

type Word8Color = (Word8,Word8,Word8,Word8)
type Settings = (Int, String, String,String, Float, Bool)


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

{- iterationList res cent zoom max_it
   DESCRIPTION:
   PRE:
   RETURNS:
   EXAMPLES: iterationList
   VARIANT:
-}
iterationList :: RealFloat a => (Int, Int) -> (a, a) -> a -> Int -> [Int]
iterationList r@(rx,ry) c z it = iterationListAux (-rx `div` 2, ry `div` 2 - 1 + (ry `mod` 2)) c r z it

{- iterationListAux (px,py) (cx,cy) (rx,ry) zm
   DESCRIPTION:
   PRE: ?????????????????????????????????????????????????????????????????
   RETURNS:
   EXAMPLES: iterationListAux
   VARIANT:
-}
iterationListAux :: RealFloat a => (Int, Int) -> (a, a) -> (Int, Int) -> a -> Int -> [Int]
iterationListAux p@(px,py) c@(cx,cy) r@(rx,ry) zm it
  | py < (-ry) `div` 2 = []
  | px >= rx `div` 2 + (rx `mod` 2) = iterationListAux (-px + (rx `mod` 2), py - 1) c r zm it
  | otherwise = (pixelCheck (coordToComp (fromIntegral px, fromIntegral py) c (fromIntegral rx,fromIntegral ry) zm) it) : (iterationListAux (px+1,py) c r zm it)

{- createRGBA iter ls
   DESCRIPTION: Converts a list of iterations to a graphical representation
   PRE: 0 <= x < length ls for all x that are elements of iter
   RETURNS: ByteString of rgba-colors from ls matched to elements of iter.
   EXAMPLES: createRGBA [1,3,2,1,2,3] 3 [80,90,100,255,150,150,150,255,43,67,21,255] (0,0,0,255) -> [150,150,150,255,0,0,0,255,43,67,21,255,150,150,150,255,43,67,21,255,0,0,0,255]
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

picture :: Settings -> Picture
picture (it, xcord, ycord, zoom,c,r)
                | r == False = pictures
                  [translate (0) (350-(150*c))$ Color yellow (rectangleSolid 700 120),
                  translate (-340) (150) $  Text $"X:"++ xcord,
                  translate (-340) (0) $  Text $"Y:"++ycord,
                  translate (-340) (-150) $ Text $"Z:"++zoom]
                | otherwise = bitmapOfByteString 700 700 (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList (700, 700) (read (xcord), read(ycord)) read(zoom) it) (cap (cycleGrad [(255,255,255,255),(255,0,0,255),(255,255,0,255),(0,255,0,255),(0,255,255,255),(0,0,255,255),(255,0,255,255)] 8) (0,0,0,255) it))) True



window :: Display
window = InWindow "Epic Insane Gamer Window" (700, 700) (10, 10)

handlekeys :: Event -> Settings -> Settings
handlekeys (EventKey (MouseButton LeftButton) Down _ (x',y')) (it,x'',y'',zoom,c,r) =
  let x= realPart(coordToComp (x',y') (x'',y'') (700,700) zoom)
      y= imagPart(coordToComp (x',y') (x'',y'') (700,700) zoom)
    in(it, x,y,zoom*1.25,c,r)
handlekeys (EventKey (SpecialKey KeyUp) Down _ _)  current@(it,x,y,z,c,r) =if(c > 1) then (it,x,y,z,c-1,r) else current
handlekeys (EventKey (SpecialKey KeyDown) Down _ _)  current@(it,x,y,z,c,r) = if(c < 3) then (it,x,y,z,c+1,r) else current
handlekeys (EventKey (SpecialKey KeyEnter) Down _ _) (it,x,y,z,c,r) = (it,x,y,z,c,r==False)
handlekeys (EventKey (Char k) Down _ _) current@(it,x,y,z,c,r)
  | r == True = current
  | c == 1 && k == '\b' = if(length(x) < 1) then current else (it,init(x),y,z,c,r)
  | c == 2 && k == '\b' = if(length(y) < 1) then current else(it,x,init(y),z,c,r)
  | c == 3 && k == '\b' = if(length(z) < 1) then current else(it,x,y,init(z),c,r)
  | isDigit k == False && k /= '.' && k /= '-' = current
  | c == 1 = (it,x++[k],y,z,c,r)
  | c == 2 = (it,x,y++[k],z,c,r)
  | c == 3 = (it,x,y,z++[k],c,r)
handlekeys _ current = current

main :: IO()
main = play window white 1 (127,"0","0", "0.5", 1, False) (picture) (handlekeys) (const id)
