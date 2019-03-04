import Data.Complex
import Graphics.Gloss
import Data.Word
import Graphics.Gloss.Interface.Pure.Game
import Data.ByteString (ByteString, pack)
import Data.Char
import Test.HUnit

{- This datatype represents a color in RGBA format
     One complete RGBA-color contains four values and is therefore represented as a 4-tuple
-}
type Word8Color = (Word8,Word8,Word8,Word8)

{- All the settings that can be altered by the user is stored here.
   First element is the amount of iterations it takes before we decide any given pixel does not tend towards infinity. Second element is the x-coordinate that is center of screen.
   Third argument is y-coordinate center on screen. Forth element is the zoom factor. Fifth element is the active menu feild counted from top to top to bottom
   sixth element is what mode is currently in, False is Menu mode and True is fractal mode. Seventh is width of fractal. Eight is Height of fractal.
   Ninth is the exponent used to generate the fractal and tenth is the type of fractal.
   INVARIANT: element 1,7 and 8 can only contain characters that are digits. Element 2,3,4 and 9 can only be conatain characters that are digits or be either '-' or '.'
-}

type Settings = (String, String, String,String, Float, Bool, String, String,String,String)

{- View a = (res, cent, zm)
 - This datatype represents a view of the complex number plane.
 - res represents the resolution of the view in pixels. cent represents the number that the view is centered on.
 - zm represents the zoom factor, i.e. how much the view is zoomed in. For example, zoom factor of 1 gives a view that spans 2 length
 - units in the complex number plane, while a zoom factor of 4 gives a view that spans 0.5 length units.
 - INVARIANT: No element of res is less than 1. zm /= 0.
 -}
type View a = ((Int, Int), Complex a, a)

---------------------------------------------------------------------------------------------------------------------------------------

{- mandelSet z c e
   DESCRIPTION: Calculates one iteration of the function used for determining whether a number is in the mandelbrot set.
   RETURNS: The result of applying the "mandelbrot" function to z and c with exponent e
   EXAMPLES: mandelSet (1 :+ (-1)) ((-1) :+ 1) 2 -> (-1.0) :+ (-1.0)
-}
mandelSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
mandelSet z c e = z ** e + c

{- burningShipSet z c e
   DESCRIPTION: Calculates one iteration of the function used for determining whether a number is in the burning ship fractal.
   RETURNS: The result of applying the "burning ship" function to z and c with exponent e
   EXAMPLES: burningShipSet (1 :+ (-1)) ((-1) :+ 1) 2 -> (-1.0) :+ 3.0
-}
burningShipSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
burningShipSet z c e = (abs(realPart z) :+ abs(imagPart z)) ** e + c

{- tricornSet z c e
   DESCRIPTION: Calculates one iteration of the function used for determining whether a number is in the tricorn set.
   RETURNS: The result of applying the "tricorn" function to z and c with exponent e
   EXAMPLES: tricornSet (1 :+ (-1)) ((-1) :+ 1) 2 -> (-1.0) :+ 3.0
-}
tricornSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
tricornSet z c e = (conjugate z) ** e + c

{- juliaSet z c e
   DESCRIPTION: Calculates one iteration of the function used for determining whether a number is in the julia set with constant value 0.279.
   RETURNS: The result of applying the "julia" function to z and 0.279 with exponent e
   EXAMPLES: juliaSet (1 :+ (-1)) ((-1) :+ 1) 2 -> 0.279 :+ (-2.0)
-}
juliaSet :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a
juliaSet z _ e = z ** e + 0.279

{- readSet set
 - DESCRIPTION: Translates the name of a set to the appropriate iterative function.
 - RETURNS: The function that is used for determining whether a complex number is within set. Returns mandelset if set does not match the name of any known set.
 - EXAMPLES: readSet "juliaSet" = juliaSet
 -   readSet "aksjdh" = mandelSet
 -}
readSet :: RealFloat a => String -> Complex a -> Complex a -> Complex a -> Complex a
readSet "burningShipSet" = burningShipSet
readSet "tricornSet" = tricornSet
readSet "juliaSet" = juliaSet
readSet _ = mandelSet

{- iterationCheck z maxIt func exp
   DESCRIPTION: Calculates how many iterations of a function it takes for a complex number to begin diverging from 0.
   PRE: maxIt > 0
   RETURNS: Just the number of iterations it takes for z to begin diverging from 0 when iterated over the function func with exponent exp. If z does not diverge after
     maxIt iterations then Nothing is returned.
   EXAMPLES: iterationCheck (0.05 :+ 0.9) 255 -> 5
-}
iterationCheck :: RealFloat a => Complex a -> Int -> (Complex a -> Complex a -> Complex a -> Complex a) -> Complex a -> Maybe Int
iterationCheck z maxIt func exp = iterationCheckAux z maxIt func exp 0 z

{- iterationCheckAux z maxIt func exp currIt c 
   DESCRIPTION: Calculates how many iterations of a function it takes for a complex number to begin diverging from 0
   PRE: maxIt > 0
   RETURNS: Just the number of iterations it takes for z to begin diverging from 0, when iterated over the function func with exponent exp and constant value c. It is
   assumed that currIt iterations have already been applied to z. If z does not diverge after maxIt iterations then Nothing is returned.
   EXAMPLES: iterationCheckAux mandel 0 255 (0.05 :+ 0.9) (0.05 :+ 0) -> 255
   VARIANT: maxIt - currIt + 1
-}
iterationCheckAux :: RealFloat a => Complex a -> Int -> (Complex a -> Complex a -> Complex a -> Complex a) -> Complex a -> Int -> Complex a -> Maybe Int
iterationCheckAux z maxIt func exp currIt c
  | currIt > maxIt = Nothing
  | magnitude(z) > 2 = Just currIt
  | otherwise = iterationCheckAux (func z c exp) maxIt func exp (currIt+1) c 

{- coordToComp pix view
   DESCRIPTION: Gets the complex number located at a certain coordinate in a view of the complex number plane.
   RETURNS: The complex number located at position pix in view.
   EXAMPLES: coordToComp (15,15) ((30,30), (0 :+ 0), 0.5) -> 2.0 :+ 2.0
-}
coordToComp :: RealFloat a => (a, a) -> View a -> Complex a
coordToComp (px,py) ((rx, ry), (cx :+ cy), zm) =
  let
    aux p r c z = (p * 2) / (r * z) + c
    rm = fromIntegral $ min rx ry
  in
    (aux px rm cx zm) :+ ((aux py rm cy zm))

{- iterationList view maxIt func exp
   DESCRIPTION: Evaluates iterationCheck for each complex number visible in a certain view of the complex number plane.
   PRE: maxIt > 0
   RETURNS: A list of Just the number of iterations it takes for each complex number visible in view to begin diverging from 0 when iterated over the function func
     with exponent exp. A complex number that does not begin diverging from 0 after maxIt iterations result in a Nothing element in the returned list. The order of
     elements in the returned list is equivalent to traversing view left to right, top to bottom.
   EXAMPLES: ((2, 2), ((-1.747) :+ 0), 8) 100 mandelSet 2 -> [Just 70, Nothing, Just 3, Just 3]
-}
iterationList :: RealFloat a => View a -> Int -> (Complex a -> Complex a -> Complex a -> Complex a) -> Complex a -> [Maybe Int]
iterationList (r@(rx, ry), cent, zm) maxIt func exp =
  let
    aux p = coordToComp p ((fromIntegral rx, fromIntegral ry), cent, zm)
  in
    map (\p -> iterationCheck (aux p) maxIt func exp) (coordList r)

{- coordList res
 - DESCRIPTION: Gets a list of all coordinates in a Gloss display.
 - PRE: Al elements of res are greater than 0.
 - RETURNS: A list of all coordinates in a Gloss display of dimensions res. List order is equivalent to traversing the display left to right, top to bottom.
 - EXAMPLES: coordList (3, 3) -> [(-1.0, 1.0), (0.0, 1.0), (1.0, 1.0), (-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (-1.0, -1.0), (0.0, -1.0), (1.0, -1.0)]
 -}
coordList :: RealFloat a => (Int, Int) -> [(a, a)]
coordList res = coordListAux res (0, 0)

{- coordListAux res@(rx, ry) pix@(px, py)
 - DESCRIPTION: Gets a partial list of all coordinates in a Gloss display.
 - PRE: 0 <= px < rx, 0 <= py < ry
 - RETURNS: A list of the coordinates in a Gloss display of dimensions res. Only the coordinates following and including the one located at pixel pix are
 -   included. List order is equivalent to traversing the deisplay left to right, top to bottom.
 - EXAMPLES: coordListAux (3, 3) (0, 1) -> [(-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (-1.0, -1.0), (0.0, -1.0), (1.0, -1.0)]
 - VARIANT: rx * ry - px - py * rx
 -}
coordListAux :: RealFloat a => (Int, Int) -> (Int, Int) -> [(a, a)]
coordListAux r@(rx, ry) p@(px, py)
  | py >= ry = []
  | px >= rx = coordListAux r (0, py + 1)
  | otherwise =
    let
      (x, y) = pixToCoord r p
    in
      (fromIntegral x, fromIntegral y) : (coordListAux r (px + 1, py))

{- pixToCoord res pix
 - DESCRIPTION: Translates the location of a pixel to a coordinate in a Gloss display.
 - RETURNS: The Gloss coordinate located where the pixel pix is, relative to a Gloss display of dimensions res.
 - EXAMPLES: pixToCoord (5, 5) (2, 2) -> (0, 0)
 -   pixToCoord (5, 5) (0, 0) -> (-2, 2)
 -}
pixToCoord :: (Int, Int) -> (Int, Int) -> (Int, Int)
pixToCoord (rx, ry) (px, py) = (px - (rx `div` 2), ry - py - 1 - (ry `div` 2))

{- createRGBA iters cols setCol
   DESCRIPTION: Creates a visual representation of an iteration list.
   PRE: 0 <= x < length cols for all Just x that are elements of iter
   RETURNS: A Word8 list that can be converted to a bitmap image, where each color is picked from cols or setCol based on the elements of iter. More specifically,
     if an element is Just x, then an element of cols is picked based on the magnitude of x (the greater the magnitude, the further down the list cols).
     Otherwise setCol is picked. The order of colors in the returned list corresponds to the order of elements in iters.
   EXAMPLES: createRGBA [1,0,2,2,1] [(255,0,0,255),(0,255,0,255),(0,0,255,255)] -> [0,255,0,255,255,0,0,255,0,0,255,255,0,0,255,255,0,255,0,255]
   VARIANT: length iter
-}
createRGBA :: [Maybe Int] -> [Word8Color] -> Word8Color -> [Word8]
createRGBA [] _ _ = []
createRGBA (Nothing:xs) cols c@(r,g,b,a) = r : g : b : a : (createRGBA xs cols c)
createRGBA (Just x:xs) cols c =
  let
    (r,g,b,a) = (cols !! x)
  in
    r : g : b : a : (createRGBA xs cols c)

{- cycleGrad c s
   DESCRIPTION: Creates a repeating cycle of a gradient between multiple colors
   PRE: length l >= 2
   RETURNS: A list of colors that form a gradient from each color in an infinitely repeating version of c such that the components of two consecutive colors differ by
     no more than s.
   EXAMPLES: cycleGrad [(255, 255, 0, 255), (0, 255, 255, 255), (255, 0, 255, 255)] 255 ->
       [(255, 255, 0, 255), (0, 255, 255, 255), (255, 0, 255, 255), ad infinitum]
-}
cycleGrad :: [Word8Color] -> Word8 -> [Word8Color]
cycleGrad l s = gradient (cycle l) s

{- gradient c s
   DESCRIPTION: Creates a gradient between multiple colors
   PRE: length c >= 2
   RETURNS: A list of colors that form a gradient from each color in c to the next, such that the components of two consecutive colors differ by no more than s.
   EXAMPLES: gradient [(255, 127, 63, 255), (127, 255, 80, 255), (0, 127, 255, 255)] 64 -> 
       [(255, 127, 63, 255), (191, 191, 80, 255), (127, 255, 80, 255), (63, 191, 144, 255), (0, 127, 208, 255), (0, 127, 255, 255)]
   VARIANT: length c
-}
gradient :: [Word8Color] -> Word8 -> [Word8Color]
gradient [c] _ = [c]
gradient [c1,c2] s = twoCGradient c1 c2 s
gradient c@(c1:c2:cs) s = (twoCGradient c1 c2 s) ++ (tail $ gradient (c2:cs) s)

{- twoCGradient c1 c2 s
   DESCRIPTION: Creates a gradient between two colors
   PRE: s > 0
   RETURNS: A list of colors (rgba-format) that form a gradient from c1 to c2, such that the components of two consecutive colors differ by no more than s.
   EXAMPLES: twoCGradient (255, 127, 63, 255) (127, 255, 80, 255) 64 -> [(255, 127, 63, 255), (191, 191, 80, 255), (127, 255, 80, 255)]
   VARIANT: The greatest difference a component of c1 and its corresponding component in c2
-}
twoCGradient :: Word8Color -> Word8Color -> Word8 -> [Word8Color]
twoCGradient c1@(r1,g1,b1,a1) c2@(r2,g2,b2,a2) s
  | (r1,g1,b1,a1) == (r2,g2,b2,a1) = [c2]
  | otherwise = c1 : (twoCGradient ((stepTo r1 r2 s), (stepTo g1 g2 s), (stepTo b1 b2 s), (stepTo a1 a2 s)) c2 s)

{- stepTo x y s
   DESCRIPTION: Makes x approach y through adding a certain step-size s
   PRE: s > 0
   RETURNS: x, with a positive number no greater than s added or subtracted such that the result is as close to y as possible.
   EXAMPLES: stepTo 1 6 2 -> 3
     stepTo 1 2 3 -> 2
-}
stepTo :: Integral a => a -> a -> a -> a
stepTo x y s
  | abs ((toInteger x) - (toInteger y)) <= (toInteger s) = y
  | x > y = x - s
  | otherwise = x + s

--------------------------------------------------------------------------------------------------------------------------------------
{-picture settings@(it, xcord, ycord, zoom,c,r,xres,yres,exp,set)
DESCRIPTION: Defines the picture for either the menu where you can choose values or the fractal based on those values
PRE: The sting varibles in it, xcord, ycord, zoom, xres, yres and exp need to be properly formatted numbers that can be run through the function read
RETURNS: if r = False we return the picture of the menu with the current values and the yellow box behind the the currently selected feild
EXAMPLES: picture ("100","0","0","0.5",1,True,"100","100","2","mandel") -> Pictures [Polygon [(-55.0,-55.0),(-55.0,55.0),(55.0,55.0),(55.0,-55.0)],Bitmap BitmapData]
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
                                white = (255, 255, 255, 255)
                                red = (255, 0, 0, 255)
                                yellow = (255, 255, 0, 255)
                                green = (0, 255, 0, 255)
                                cyan = (0, 255, 255, 255)
                                blue = (0, 0, 255, 255)
                                violet = (255, 0, 255, 255)
                                black = (0, 0, 0, 255)
                                auraPalette = cycleGrad [white, red, yellow, green, cyan, blue, violet] 8
                                border = rectangleSolid (fromIntegral (read xres) + 10) (fromIntegral (read yres) + 10)
                                view = (((fromIntegral $ read xres), (fromIntegral $ read yres)), (x :+ y), zoomer)
                                iterList = iterationList view iter (readSet set) ((read exp) :+ 0)
                                format = (BitmapFormat TopToBottom PxRGBA)
                                fractalImgBytes = pack $ createRGBA iterList auraPalette black
                              in
                                pictures[border,bitmapOfByteString (fromIntegral (read xres)) (fromIntegral (read yres)) format fractalImgBytes True]

window :: Display
window = InWindow "Epic Insane Gamer Window" (700, 700) (10, 10)

{-handlekeys event settigns@(it,x,y,zoom,c,r,xres,yres,exp,set)
   DESCRIPTION: handles any and all user input from the user including mouseclicks, any letter/number button press and the two specialkeys KeyUp and KeyDown
   PRE: The sting varibles in x, y, zoom, xres and yres need to be properly formatted numbers that can be run through the function read
   RETURNS:
          cases:
            If keypress is backslash it returns same settings but missing the last character of the string corresponding to the value of c counting in the menu from top to bottom.
            If keypress is a number a dot or a minus symbol it adds it to the string corrisponding to the value of c counting in the menu from top to bottom
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
      x= show(realPart(coordToComp (x',y') (((fromIntegral (read xres)),(fromIntegral (read yres))), ((read x'') :+ (read y'')), (read zoom))))
      y= show(imagPart(coordToComp (x',y') (((fromIntegral (read xres)),(fromIntegral (read yres))), ((read x'') :+ (read y'')), (read zoom))))
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
handlekeys (EventKey (Char k) Down _ _) current@(_,_,_,_,c,r,_,_,_,_)
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
RETURNS: The settings as follows ("100","0","0", "0.5", 0, False, "250", "250","2","mandelbrotset")
-}
initialSettings :: Settings
initialSettings = ("100","0","0", "0.5", 0, False, "250", "250","2","mandelbrotset")

main :: IO()
main = play window white 1 initialSettings (picture) (handlekeys) (const id)

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

test1 = TestCase $ assertEqual "No diverge" Nothing (iterationCheck 0 127 mandelSet 2)
test2 = TestCase $ assertEqual "Diverge after 3" (Just 3) (iterationCheck ((-0.56) :+ 0.893333) 51 mandelSet 2)
test3 = TestCase $ assertEqual "coordToComp top right corner" (2.0 :+ 2.0) (coordToComp (25, 25) ((50, 50), (0 :+ 0), 0.5))
test4 = TestCase $ assertEqual "coordToComp bottom left corner" ((-2.0) :+ (-2.0)) (coordToComp ((-25), (-25)) ((50, 50), (0 :+ 0), 0.5))
test5 = TestCase $ assertEqual "coordToComp center" (0.0 :+ 0.0) (coordToComp (0, 0) ((50, 50), (0 :+ 0), 0.5))
test6 = TestCase $ assertEqual "coordToComp displacement" (1.5 :+ 2.0) (coordToComp (5, 5) ((20, 20), (1 :+ 1.5), 1))
test7 = TestCase $ assertEqual "stepTo positive" 4 (stepTo 1 7 3)
test8 = TestCase $ assertEqual "stepTo negative" 3 (stepTo 7 2 4)
test9 = TestCase $ assertEqual "stepTo no overshoot" 2 (stepTo 1 2 30)
test10 = TestCase $ assertEqual "twoCGradient (0,0,0,255) (0,1,2,255) 1" [(0,0,0,255), (0,1,1,255), (0,1,2,255)] (twoCGradient (0,0,0,255) (0,1,2,255) 1)
test11 = TestCase $ assertEqual "twoCGradient (0,0,0,255) (6,6,6,255) 2" [(0,0,0,255), (2,2,2,255), (4,4,4,255), (6,6,6,255)] (twoCGradient (0,0,0,255) (6,6,6,255) 2)
test12 = TestCase $ assertEqual "gradient [(0,0,0,255),(1,2,3,255),(6,5,4,255)] 2" [(0,0,0,255),(1,2,2,255),(1,2,3,255),(3,4,4,255),(5,5,4,255),(6,5,4,255)] (gradient [(0,0,0,255),(1,2,3,255),(6,5,4,255)] 2)


runTests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12]
