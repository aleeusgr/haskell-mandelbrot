module Fractals (
  mandelSet,
  burningShipSet,
  tricornSet,
  juliaSet,
  iterationCheck,
  iterationCheckAux,
  coordToComp,
  iterationList,
  coordList,
  coordListAux,
  pixToCoord) where

--Authors: Petter Eckerbom, Axel Gällstedt, Thomas Mathiassen

import Data.Complex

{- View a = (res, cent, zm)
 - This datatype represents a view of the complex number plane.
 - res represents the resolution of the view in pixels. cent represents the number that the view is centered on.
 - zm represents the zoom factor, i.e. how much the view is zoomed in. For example, zoom factor of 1 gives a view that spans 2 length
 - units in the complex number plane, while a zoom factor of 4 gives a view that spans 0.5 length units.
 - INVARIANT: No element of res is less than 1. zm /= 0.
 -}
type View a = ((Int, Int), Complex a, a)

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
