{-
  Iago dos Santos
  Thiago Cavalcanti
  Vinicius Correa

-}

module Main where 

import SOE 

fillAnySquare :: Window -> Color -> Int -> Int -> Int -> IO()
fillAnySquare w c x y size
  = drawInWindow w $ withColor c (polygon [(x, y), (x+size, y), (x+size, y+size), (x, y+size)])

tSquare :: Window -> Int -> Int -> Int -> IO()
tSquare w x y size =
  if size <= 1
    then fillAnySquare w White x y size
  else
    let 
      size2 = size `div` 2
    in do
      fillAnySquare w White x y size
      tSquare w (x-div size2 2) (y- div size2 2) size2
      tSquare w (x +size2+ div size2 2) (y- div size2 2) size2
      tSquare w (x-div size2 2) (y+size2+ div size2 2) size2
      tSquare w (x +size2+ div size2 2) (y+size2+ div size2 2) size2


main = 
  runGraphics(
    do
      w <- openWindow "T_Fractal" (600,600)
      fillAnySquare w White 150 150 300
      tSquare w 150 150 300
      spaceClose w 
  )


spaceClose :: Window -> IO ()
spaceClose w = do
   k <- getKey w
   if k == ' '
    then closeWindow w
    else spaceClose w