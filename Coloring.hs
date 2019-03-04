module Coloring (
  createRGBA,
  cycleGrad,
  gradient,
  twoCGradient,
  stepTo) where
  
--Authors: Petter Eckerbom, Axel Gällstedt, Thomas Mathiassen

import Data.Word

{- This datatype represents a color in RGBA format
     One complete RGBA-color contains four values and is therefore represented as a 4-tuple
-}
type Word8Color = (Word8,Word8,Word8,Word8)

-----------------------------------------------------------------------------------

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
