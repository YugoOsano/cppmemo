module Geometry where

data Shape = Triangle { base,
                        height:: Double }
           | Rectangle { width,
                         height:: Double }

area :: Shape ->
        Double
area (Triangle base
               height)
  = base
  * height
  / 2
area (Rectangle base
                height) = base * height

