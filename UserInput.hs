module UserInput (
  Settings,
  readSet,
  handleInput,
  erase,
  addChar) where

--Authors: Petter Eckerbom, Axel Gällstedt, Thomas Mathiassen

import Graphics.Gloss.Interface.Pure.Game
import Data.ByteString (ByteString, pack)
import Data.Char
import Fractals
import Data.Complex

{- All the settings that can be altered by the user is stored here.
   First element is the amount of iterations it takes before we decide any given pixel does not tend towards infinity. Second element is the x-coordinate that is center of screen.
   Third argument is y-coordinate center on screen. Forth element is the zoom factor. Fifth element is the active menu feild counted from top to top to bottom
   sixth element is what mode is currently in, False is Menu mode and True is fractal mode. Seventh is width of fractal. Eight is Height of fractal.
   Ninth is the exponent used to generate the fractal and tenth is the type of fractal.
   INVARIANT: element 1,7 and 8 can only contain characters that are digits. Element 2,3,4 and 9 can only be conatain characters that are digits or be either '-' or '.'
-}
type Settings = (String, String, String,String, Float, Bool, String, String,String,String)


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

{-handleInput event settigns@(it,x,y,zoom,c,r,xres,yres,exp,set)
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
handleInput :: Event -> Settings -> Settings
handleInput (EventKey (MouseButton but) Down _ (x',y')) current@(it,x'',y'',zoom,c,r,xres,yres,exp,set) =
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
handleInput (EventKey (SpecialKey KeyUp) Down _ _)  current@(it,x,y,z,c,r,xres,yres,exp,set) =if(c > 0) then (it,x,y,z,c-1,r,xres,yres,exp,set) else current
handleInput (EventKey (SpecialKey KeyDown) Down _ _)  current@(it,x,y,z,c,r,xres,yres,exp,set) = if(c < 7) then (it,x,y,z,c+1,r,xres,yres,exp,set) else current
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) (it,x,y,z,c,r,xres,yres,exp,set) = (it,x,y,z,c,r==False,xres,yres,exp,set)
handleInput (EventKey (Char k) Down _ _) current@(_,_,_,_,c,r,_,_,_,_)
  | r == True = current
  | k == '\b' = erase current
  | c == 6 = addChar current k
  | isDigit k == False && k /= '.' && k /= '-' = current
  | otherwise = addChar current k
handleInput _ current = current

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
