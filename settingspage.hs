import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char

type Settings = (String, String, String, Int)

picture (xcord, ycord, zoom,c,r) = pictures
                [translate (-520) (150) $ Text ("X:   "++ xcord),
                 translate (-520) (0) $ Text ("Y:   "++ycord),
                 translate (-520) (-150) $ Text ("Zoom: "++zoom),
                 translate (420) (-200) $ Text (show c)]
                 

window :: Display
window = InWindow "Epic Insane Gamer Window" (1100, 600) (10, 10)

handlekeys (EventKey (Char k) Down _ (x',y')) (x,y,z,c,r)
  | k == 's' = (x,y,z,c,True)
  | k == 'a' && c > 1 = (x,y,z,c-1,r)
  | k == 'q' && c < 3 = (x,y,z,c+1,r)
  | c == 1 && k == '\b' = if(length(x) <= 1) then("0",y,z,c,r) else (init(x),y,z,c,r)
  | c == 2 && k == '\b' = if(length(y) <= 1) then(x,"0",z,c,r) else(x,init(y),z,c,r)
  | c == 3 && k == '\b' = if(length(z) <= 1) then(x,y,"0",c,r) else(x,y,init(z),c,r)
  | isDigit k == False && k /= '.' = (x,y,z,c,r)
  | c == 1 = (x++[k],y,z,c,r)
  | c == 2 = (x,y++[k],z,c,r)
  | c == 3 = (x,y,z++[k],c,r)

handlekeys _ current = current

main = play window yellow 1 ("-1,3321","0", "2556", 2, False) (picture) (handlekeys) (const id)
