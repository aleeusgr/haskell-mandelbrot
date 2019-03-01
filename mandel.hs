import Data.Complex
import Graphics.Gloss
import Data.Word
import Graphics.Gloss.Interface.Pure.Game
import Data.ByteString (ByteString, pack)
import Data.Char
import Test.HUnit

{- This datatype represents a color on the RGBA-format 
     One complete RGBA-color contains four values and is therefore represented as a 4-tuple
     INVARIANT:  ?????????????????????????????????????????????????????????????????????
-}
type Word8Color = (Word8,Word8,Word8,Word8)

 {- ... description of what the data type represents ... 
     ... description of how the datatype represents data ...
     INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}

{- ??????????????????????????? 
   ???????????????????????????
   ?????????????????????????
-}

type Settings = (String, String, String,String, Float, Bool, String, String,String,String)

---------------------------------------------------------------------------------------------------------------------------------------

{- mandelSet z c e
   DESCRIPTION: Calculates one iteration of the formula for numbers in the Mandelbrot set
   RETURNS: A number in the Mandelbrot set, based on z and c
   EXAMPLES: mandel (4 :+ 2) (2 :+ 3) -> 14.0 :+ 19.0
-}
mandelSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
mandelSet z c e = z ** e + c

{-burningShipSet z c e
  DESCRIPTION: 
  RETURNS: 
  EXAMPLES: 
-}
burningShipSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
burningShipSet z c e = (abs(realPart z) :+ abs(imagPart z)) ** e + c

{-tricornSet z c e
  DESCRIPTION: 
  RETURNS: 
  EXAMPLES: 
-}
tricornSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
tricornSet z c e = (conjugate z) ** e + c

{-juliaSet z c e
  DESCRIPTION: Calculates one iteration of the formula for numbers in the Julia set
  RETURNS: 
  EXAMPLES: 
-}
juliaSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
juliaSet z _ e = z ** e + 0.279

{-epicSet z c e
  DESCRIPTION:
  RETURNS: 
  EXAMPLES: 
-}
epicSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
epicSet z c e = z ** 2 + z ** 3 + c

{- iterationCheck z maxIt fractalSet
   DESCRIPTION: Calculates how many iterations of the Mandelbrot formula it takes for z to approach infinity
   PRE: maxIt > 0
   RETURNS: The number of iterations it takes for z to grow beyond the size of what is allowed in a Mandelbrot set
   EXAMPLES: iterationCheck (0.05 :+ 0.9) 255 -> 5
-}
iterationCheck :: RealFloat a => Complex a -> Int -> String -> Complex a -> Int
iterationCheck z maxIt fractalSet e
  | fractalSet == "burningShipSet" = iterationCheckAux burningShipSet 0 maxIt z z e
  | fractalSet == "tricornSet" = iterationCheckAux tricornSet 0 maxIt z z e
  | fractalSet == "juliaSet" = iterationCheckAux juliaSet 0 maxIt z z e
  | fractalSet == "epicSet" = iterationCheckAux epicSet 0 maxIt z z e
  | otherwise = iterationCheckAux mandelSet 0 maxIt z z e

{- iterationCheckAux f currIt maxIt z c
   DESCRIPTION: Calculates how many iterations of f it takes for z to approach infinity
   PRE: maxIt > 0,
   RETURNS: currIt, i.e. the number of iterations passed
   EXAMPLES: iterationCheckAux mandel 0 255 (0.05 :+ 0.9) (0.05 :+ 0) -> 255
   VARIANT: maxIt - currIt
-}
iterationCheckAux :: RealFloat a => (Complex a -> Complex a -> Complex a -> Complex a) -> Int -> Int -> Complex a -> Complex a -> Complex a -> Int
iterationCheckAux f currIt maxIt z c e
  | currIt >= maxIt = currIt
  | magnitude(z) > 2 = currIt
  | otherwise = iterationCheckAux f (currIt+1) maxIt (f z c e) c e

{- coordToComp pix cent res@(rx,ry) zm
   DESCRIPTION: Converts an on-screen pixel to a coordinate in the complex number plane.
   PRE: rx /= 0, ry /= 0, zm /= 0
   RETURNS: The complex number located at position pix on a view of the complex number plane
     with resolution res centered on the complex number cam with zoom factor zm. ??????????
   EXAMPLES: coordToComp (20,20) (0,0) (50,50) 0.5 -> 1.6 :+ 1.6
-}
coordToComp :: RealFloat a => (a, a) -> (a, a) -> (a, a) -> a -> Complex a
coordToComp (px,py) (cx,cy) (rx,ry) zm =
  let
    aux p r c z = (p * 2) / (r * z) + c
    rm = max rx ry
  in
    (aux px rm cx zm) :+ ((aux py rm cy zm))

{- iterationList res cent zoom max_it
   DESCRIPTION:
   PRE:
   RETURNS:
   EXAMPLES:
-}
iterationList :: RealFloat a => (Int, Int) -> (a, a) -> a -> Int -> String -> Complex a -> [Int]
iterationList r@(rx,ry) c z it f e = iterationListAux (-rx `div` 2, ry `div` 2 - 1 + (ry `mod` 2)) c r z it f e

{- iterationListAux
   DESCRIPTION:
   PRE:
   RETURNS:
   EXAMPLES:
   VARIANT:
-}
iterationListAux :: RealFloat a => (Int, Int) -> (a, a) -> (Int, Int) -> a -> Int -> String -> Complex a -> [Int]
iterationListAux p@(px,py) c@(cx,cy) r@(rx,ry) zm it f e
  | py < (-ry) `div` 2 = []
  | px >= rx `div` 2 + (rx `mod` 2) = iterationListAux (-px + (rx `mod` 2), py - 1) c r zm it f e
  | otherwise =
      let
        toRF x = fromIntegral x
      in
        (iterationCheck (coordToComp (toRF px, toRF py) c (toRF rx,toRF ry) zm) it f e) : (iterationListAux (px+1,py) c r zm it f e)


{- createRGBA iter ls
   DESCRIPTION: Converts a list of iterations to a graphical representation
   PRE: 0 <= x < length ls for all x that are elements of iter
   RETURNS: ByteString of rgba-colors from ls matched to elements of iter. ???????????????????????????????????????
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
{-picture settings@(it, xcord, ycord, zoom,c,r,xres,yres,exp,set)
DESCRIPTION: Defines the picture for either the menu where you can choose values or the fractal based on those values
PRE: The sting varibles in it, xcord, ycord, zoom, xres, yres and exp need to be properly formatted numbers that can be run through the function read
RETURNS: if r = False we return the picture of the menu with the current values and the yellow box behind the the currently selected feild
EXAMPLES: How am I even supposed to do this?
-}
picture :: Settings -> Picture
picture (it, xcord, ycord, zoom,c,r,xres,yres,exp,set)
                | r == False = pictures
                  [translate (0) (280-(75*c))$ Color yellow (rectangleSolid 4500 80),
                  translate (-340) (250) $  Scale 0.5 0.5 (Text $ "It:"++ it),
                  translate (-340) (175) $  Scale 0.5 0.5 (Text $ "X:"++ xcord),
                  translate (-340) (100) $  Scale 0.5 0.5 (Text $ "Y:"++ycord),
                  translate (-340) (25) $ Scale 0.5 0.5 (Text $ "Zoom:"++zoom),
                  translate (-340) (-50) $ Scale 0.5 0.5 (Text $ "Xres:"++xres),
                  translate (-340) (-125) $ Scale 0.5 0.5 (Text $ "Yres:"++yres),
                  translate (-340) (-200) $ Scale 0.5 0.5 (Text $ "Set:"++set),
                  translate (-340) (-275) $ Scale 0.5 0.5 (Text $ "Exp:"++exp)]
                | otherwise = let
                                x = read(xcord)
                                y = read(ycord)
                                zoomer = read(zoom)
                                iter = read(it)
                              in 
                                pictures[rectangleSolid ((fromIntegral (read xres))+10) ((fromIntegral (read yres))+10),bitmapOfByteString (fromIntegral (read xres)) (fromIntegral (read yres)) (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList ((fromIntegral (read xres)), (fromIntegral (read yres))) (x, y) zoomer iter set ((read exp) :+ 0)) (cap (cycleGrad [(255,255,255,255),(255,0,0,255),(255,255,0,255),(0,255,0,255),(0,255,255,255),(0,0,255,255),(255,0,255,255)] 8) (0,0,0,255) iter))) True]

window :: Display
window = InWindow "Epic Insane Gamer Window" (700, 700) (10, 10)

{-handlekeys event settigns@(it,x,y,zoom,c,r,xres,yres,exp,set)
   DESCRIPTION: handles any and all user input from the user including mouseclicks, any letter/number button press and the two specialkeys KeyUp and KeyDown
   PRE: The sting varibles in x, y, zoom, xres and yres need to be properly formatted numbers that can be run through the function read
   RETURNS:
          cases:
            If keypress is backslash it returns same settings but missing the last character of the string corresponding to the value of c counting in the menu from top to top to bottom.
            If keypress is a number a dot or a minus symbol it adds it to the string corrisponding to the value of c counting in the menu from top to top to bottom.
            If keypress is is a letter i returns the same settings unless c is 6 in which case we add it like it was a number.
            If keypress is up button and c is more than 0 we return same settings but c is one less. if c is less than or equal to 0 we return unchanged settings.
            If keypress is down button and c is less than 7 we return same settings but c is one more. if c is more than or equal to 0 we return unchanged settings.
            If keypress is enter button r boolean is toggled between True and False.
            If keypress is a mousebutton and r is True it returns settings where x and y is the cordinateds clicked complex numbers counterpart. and soom is changed with a factor of 1.5 or a factor of 0.75 depending on left or right click
   EXAMPLES: Few examples:
            1. handlekeys (EventKey (Char '\b') Down (Modifiers Up Up Up) (0,0)) ("255","0","0","0.5",0,False,"100","100","2","mandel") -> ("25","0","0","0.5",0,False,"100","100","2","mandel")
            2. handlekeys (EventKey (Char '\b') Down (Modifiers Up Up Up) (0,0)) ("","0","0","0.5",0,False,"100","100","2","mandel") -> ("","0","0","0.5",0,False,"100","100","2","mandel")
            3. handlekeys (EventKey (Char '1') Down (Modifiers Up Up Up) (0,0)) ("255","1","0","0.5",1,False,"100","100","2","mandel") -> ("255","11","0","0.5",0,False,"100","100","2","mandel")
            4. handlekeys (EventKey (Char 'b') Down (Modifiers Up Up Up) (0,0)) ("255","1","0","0.5",1,False,"100","100","2","mandel") -> ("255","1","0","0.5",0,False,"100","100","2","mandel")
            5. handlekeys (EventKey (Char 'b') Down (Modifiers Up Up Up) (0,0)) ("255","1","0","0.5",6,False,"100","100","2","mandel") -> ("255","1","0","0.5",6,False,"100","100","2","mandelb")
            6. handlekeys (EventKey (SpecialKey KeyUp) Down (Modifiers Up Up Up) (0,0)) ("255","1","0","0.5",6,False,"100","100","2","mandel") -> ("255","1","0","0.5",5,False,"100","100","2","mandel")
            7. handlekeys (EventKey (SpecialKey KeyEnter) Down (Modifiers Up Up Up) (0,0)) ("255","1","0","0.5",6,False,"100","100","2","mandel") -> ("255","1","0","0.5",6,True,"100","100","2","mandel")
            8. handlekeys (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) (-10,-50)) ("255","0","0","0.5",6,True,"100","100","2","mandel")->("255","-0.4","-2.0","0.75",6.0,True,"100","100","2","mandel")
-}
handlekeys :: Event -> Settings -> Settings
handlekeys (EventKey (MouseButton but) Down _ (x',y')) current@(it,x'',y'',zoom,c,r,xres,yres,exp,set) =
  let
      x= show(realPart(coordToComp (x',y') ((read x''),(read y'')) ((fromIntegral (read xres)),(fromIntegral (read yres))) (read zoom)))
      y= show(imagPart(coordToComp (x',y') ((read x''),(read y'')) ((fromIntegral (read xres)),(fromIntegral (read yres))) (read zoom)))
    in
    if(r && (but == LeftButton || but == RightButton))
      then
        (if (but == LeftButton)
          then (it, x, y, show((read zoom)*1.5), c, r,xres,yres,exp,set)
          else (it, x, y, show((read zoom)*0.75), c, r,xres,yres,exp,set))
    else current
handlekeys (EventKey (SpecialKey KeyUp) Down _ _)  current@(it,x,y,z,c,r,xres,yres,exp,set) =if(c > 0) then (it,x,y,z,c-1,r,xres,yres,exp,set) else current
handlekeys (EventKey (SpecialKey KeyDown) Down _ _)  current@(it,x,y,z,c,r,xres,yres,exp,set) = if(c < 7) then (it,x,y,z,c+1,r,xres,yres,exp,set) else current
handlekeys (EventKey (SpecialKey KeyEnter) Down _ _) (it,x,y,z,c,r,xres,yres,exp,set) = (it,x,y,z,c,r==False,xres,yres,exp,set)
handlekeys (EventKey (Char k) Down _ _) current@(it,x'',y'',zoom,c,r,xres,yres,exp,set)
  | r == True = current
  | k == '\b' = erase current
  | c == 6 = addChar current k
  | isDigit k == False && k /= '.' && k /= '-' = current
  | otherwise = addChar current k
handlekeys _ current = current

{- erase Settings@(it,x,y,z,c,r,xres,yres,exp,set)
  DESCRIPTION: Removes the last letter from the currently highlighted feild in menu
  Pre:True
  Returns: all the same settings except for the last character in the string corresponding to the value of c in the menu counted from top to bottom is removed
  Example: erase ("255","0","0","0.5",0,True,"100","100","2","mandel") -> ("25","0","0","0.5",0.0,True,"100","100","2","mandel")
-}
erase :: Settings -> Settings
erase current@(it,x,y,z,c,r,xres,yres,exp,set)
  | c == 0 = if(length(it) < 1) then current else (init(it),x,y,z,c,r,xres,yres,exp,set)
  | c == 1 = if(length(x) < 1) then current else (it,init(x),y,z,c,r,xres,yres,exp,set)
  | c == 2 = if(length(y) < 1) then current else(it,x,init(y),z,c,r,xres,yres,exp,set)
  | c == 3 = if(length(z) < 1) then current else(it,x,y,init(z),c,r,xres,yres,exp,set)
  | c == 4 = if(length(xres) < 1) then current else(it,x,y,z,c,r,init(xres),yres,exp,set)
  | c == 5 = if(length(yres) < 1) then current else(it,x,y,z,c,r,xres,init(yres),exp,set)
  | c == 6 = if(length(set) < 1) then current else(it,x,y,z,c,r,xres,yres,exp,init(set))
  | c == 7 = if(length(exp) < 1) then current else(it,x,y,z,c,r,xres,yres,init(exp),set)
  |otherwise = current

{- addChar Settings@(it,x,y,z,c,r,xres,yres,exp,set) char
  DESCRIPTION: adds char to the currently highlighted feild in menu
  Pre:True
  Returns: all the same settings except char is added to the end of the string corresponding to the value of c in the menu counted from top to bottom
  Example: addChar ("255","0","0","0.5",6,True,"100","100","2","mandel") 'S' -> ("255","0","0","0.5",6.0,True,"100","100","2","mandelS")
-}
addChar :: Settings -> Char -> Settings
addChar (it,x,y,z,c,r,xres,yres,exp,set) k
  | c == 0 = (it++[k],x,y,z,c,r,xres,yres,exp,set)
  | c == 1 = (it,x++[k],y,z,c,r,xres,yres,exp,set)
  | c == 2 = (it,x,y++[k],z,c,r,xres,yres,exp,set)
  | c == 3 = (it,x,y,z++[k],c,r,xres,yres,exp,set)
  | c == 4 = (it,x,y,z,c,r,xres++[k],yres,exp,set)
  | c == 5 = (it,x,y,z,c,r,xres,yres++[k],exp,set)
  | c == 6 = (it,x,y,z,c,r,xres,yres,exp,set++[k])
  | c == 7 = (it,x,y,z,c,r,xres,yres,exp++[k],set)

{-initialSettings
DESCRIPTION: The starting settings when window opens
RETURNS: The settings as follows ("255","0","0", "0.5", 0, False, "300", "300","2","mandelbrotset")
-}
initialSettings :: Settings
initialSettings = ("255","0","0", "0.5", 0, False, "300", "300","2","mandelbrotset")

main :: IO()
main = play window white 1 initialSettings (picture) (handlekeys) (const id)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

test1 = TestCase $ assertEqual "mandelSet (-1.75 + 0.05i)" (1.31 :+ (-0.125)) (mandelSet ((-1.75) :+ 0.05) ((-1.75) :+ 0.05) 2)
test2 = TestCase $ assertEqual "iterationCheck 0 127" 127 (iterationCheck 0 127 "mandelSet" 2)
test3 = TestCase $ assertEqual "iterationCheck ((-0.56) :+ 0.893333) 51" 3 (iterationCheck ((-0.56) :+ 0.893333) 51 "mandelSet" 2)
test4 = TestCase $ assertEqual "coordToComp (20,20) (0,0) (50,50) 0.5" (1.6 :+ 1.6) (coordToComp (20,20) (0,0) (50,50) 0.5)
test5 = TestCase $ assertEqual "cap [1,2..5] 6 3" [1,2,3,6] (cap [1,2,3,4,5] 6 3)
test6 = TestCase $ assertEqual "cap [1,2,3] 6 0" [6] (cap [1,2,3] 6 0)
test7 = TestCase $ assertEqual "stepTo 1 7 3" 4 (stepTo 1 7 3)
test8 = TestCase $ assertEqual "stepTo 7 2 4" 3 (stepTo 7 2 4)
test9 = TestCase $ assertEqual "stepTo 1 2 30" 2 (stepTo 1 2 30)
test10 = TestCase $ assertEqual "twoCGradient (0,0,0,255) (0,1,2,255) 1" [(0,0,0,255), (0,1,1,255)] (twoCGradient (0,0,0,255) (0,1,2,255) 1)
test11 = TestCase $ assertEqual "twoCGradient (0,0,0,255) (0,1,2,255) 2" [(0,0,0,255), (2,2,2,255), (4,4,4,255)] (twoCGradient (0,0,0,255) (6,6,6,255) 2)
test12 = TestCase $ assertEqual "gradient [(0,0,0,255),(1,2,3,255),(6,5,4,255)] 2" [(0,0,0,255),(1,2,2,255),(1,2,3,255),(3,4,4,255),(5,5,4,255)] (gradient [(0,0,0,255),(1,2,3,255),(6,5,4,255)] 2)


runTests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12]
