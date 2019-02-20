import Data.Complex
import Graphics.Gloss
import Data.Word
import Data.ByteString (ByteString, pack)

{- mandel z c 
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
mandel :: RealFloat a => Complex a -> Complex a -> Complex a
mandel z c = (z*z) + c

{- pixelCheck z  
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
pixelCheck :: RealFloat a => Complex a -> Int
pixelCheck z = pixelCheckAux mandel 0 255 z z

{- pixelCheckAux f currIt maxIt z c
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
pixelCheckAux :: RealFloat a => (Complex a -> Complex a -> Complex a) -> Int -> Int -> Complex a -> Complex a -> Int
pixelCheckAux f currIt maxIt z c
  | currIt >= maxIt = currIt
  | magnitude(z) > 2 = currIt
  | otherwise = pixelCheckAux f (currIt+1) maxIt (f z c) c

{- cordToComp (x,y) res
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
cordToComp :: RealFloat a => (a, a) -> a -> Complex a
cordToComp (x,y) res = ((x/res) :+ (y/res))

{- iterationList (x,y)
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
iterationList :: (Int, Int) -> [Int]
iterationList (499,-501) = []
iterationList (x,y) 
    | x < 399 = iterations : iterationList (x+1,y)
    | x == 399 = iterations : iterationList (-500, y-1)
  where iterations = pixelCheck (cordToComp ((fromIntegral x)::Float,(fromIntegral y)::Float) 500)

{- createRGBA ls
   DESCRIPTION: 
   PRE: 
   RETURNS: 
   EXAMPLES: 
   VARIANT: 
-}
createRGBA :: [Int] -> [Word8]
createRGBA [] = []
createRGBA (x:xs) = [0,0,0,(fromIntegral(x::Int)::Word8)] ++ createRGBA (xs)


picture = bitmapOfByteString 1000 1000 (BitmapFormat TopToBottom PxRGBA) (pack (createRGBA (iterationList (-500,500)))) True
main = display (InWindow "Nice Window" (1000, 1000) (10, 10)) black picture

