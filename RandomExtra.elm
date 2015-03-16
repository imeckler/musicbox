module RandomExtra (map, andThen, simply, zip) where

import Random

map : (a -> b) -> Random.Generator a -> Random.Generator b
map f g = Random.customGenerator (\s -> let (x, s') = Random.generate g s in (f x, s'))

andThen : Random.Generator a -> (a -> Random.Generator b) -> Random.Generator b
andThen g f = Random.customGenerator (\s ->
  let (x, s') = Random.generate g s in Random.generate (f x) s')

simply : a -> Random.Generator a
simply x = Random.customGenerator (\s -> (x, s))

zip : Random.Generator a -> Random.Generator b -> Random.Generator (a, b)
zip = Random.pair

