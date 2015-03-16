module Interpolate where

import Native.Smooth

type Clamp
  = Clamp
  | Zero
  | Periodic
  | Mirror

clampString c = case c of
  Clamp    -> "clamp"
  Zero     -> "zero"
  Periodic -> "periodic"
  Mirror   -> "mirror"

interpolate : Clamp -> List Float -> (Float -> Float)
interpolate c xs = Native.Smooth.smoothCubic (clampString c) xs

interpolate2 : Clamp -> List (Float, Float) -> (Float -> (Float, Float))
interpolate2 c xs = Native.Smooth.smoothCubicTup (clampString c) xs

