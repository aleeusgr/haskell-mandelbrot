import Fractals
import Data.Complex
import Coloring
import Graphics.Gloss
import Data.ByteString (ByteString, pack)
import UserInput

--Authors: Petter Eckerbom, Axel Gällstedt, Thomas Mathiassen

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

{-initialSettings
DESCRIPTION: The starting settings when window opens
RETURNS: The settings as follows ("100","0","0", "0.5", 0, False, "250", "250","2","mandelbrotset")
-}
initialSettings :: Settings
initialSettings = ("100","0","0", "0.5", 0, False, "250", "250","2","mandelbrotset")

main :: IO()
main = play window white 1 initialSettings (picture) (handlekeys) (const id)
