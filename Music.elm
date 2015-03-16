module Music where

import Debug
import Piece
import Piece.Infix(..)
import Piece (Piece, Forever, ForATime)
import Piece.Internal as PI
import Random
import Dict
import Dict (Dict)
import Signal
import Signal.Extra as Signal
import Maybe
import Time
import Time (second)
import Graphics.Collage (..)
import Transform2D as T
import List
import List((::))
import Interpolate (..)
import RandomExtra as Random
import Color
import Char
import Char (KeyCode)
import Easing(..)

type alias SoundCode = Maybe String
type alias Context = { width : Float, height : Float }
type alias State =
  { seed : Random.Seed
  }

type alias Filter = Context -> Piece Forever (Form -> Form)

-- noise and light
type alias Instrument = Context -> State -> (Piece ForATime Form, SoundCode, State)
-- actual type should be 
-- type alias Instrument = State -> (Piece ForATime (Context -> Form), SoundCode, State)

sight x s               = (x, Nothing, s)
sightAndSound x sound s = (x, Just sound, s)

withOpacity : Float -> Color.Color -> Color.Color
withOpacity a c =
  let {red,green,blue,alpha} = Color.toRgb c
  in Color.rgba red green blue a

growingCircle : Instrument
growingCircle ctx = 
  let sight = Piece.for (1 * second) (\t -> 
        let radius  = t * (100 / second)
            opacity = 1 - t * (1 / second)
        in
        circle radius
        |> filled (Color.blue |> withOpacity opacity))
  in
  sightAndSound sight "pew"

scintillatingLines ctx =
  let numLines = 100
      dur      = 1/7 * second
      spacing  = ctx.width / numLines
      mk k     = group <| listInit numLines (\i ->
        let x = toFloat i * spacing in
        if i % 4 == k
        then group []
        else traced defaultLine (segment (x, -ctx.width/2) (x, ctx.width/2)))
  in
  List.repeat 10
    (List.foldr1 (<>) <| List.map (Piece.stayFor dur << mk) [0..3])
  |> List.foldr1 (<>)
  |> sight

scaleXY x y img = groupTransform (T.matrix x 0 0 y 0 0) [img]

listInit n f =
  let go acc k = if k == n then acc else go (f k :: acc) (k + 1)
  in List.reverse (go [] 0)

gridulate : Filter
gridulate ctx = 
  let (numRows, numCols) = (4, 4)
      cellWidth          = ctx.width / numCols
      cellHeight         = ctx.height / numRows
  in
  Piece.stayForever <| -- I've got tiiime
  \img ->
    let cellImg = scaleXY (1/numCols) (1/numRows) img in
    group (listInit numRows (\i -> 
      group (listInit numCols (\j ->
        let xPos = i * cellWidth + (cellWidth - ctx.width) / 2
            yPos = i * cellHeight + (cellHeight - ctx.height) / 2
        in
        move (xPos, yPos) cellImg))))

rainbowCycle : Filter
rainbowCycle ctx =
  let hue = Piece.cycle (Piece.for (1 * second) (\t -> t * 2 * pi / second)) in
  Piece.map (\h -> \img ->
    group [img, rect ctx.width ctx.height |> filled (Color.hsla h 1 0.5 0.2)])
    hue

spinning : Filter
spinning ctx_ =
  Piece.cycle <| Piece.for (1 * second) (\t -> rotate (t * 2 * pi / second))

theBand : Dict KeyCode Instrument
theBand = Dict.fromList
  [ (Char.toCode 'c', growingCircle)
  , (Char.toCode 'm', theManifold)
  ]

filters : Dict KeyCode Filter
filters = Dict.fromList []

initialState : State
initialState = { seed = Random.initialSeed 0 }

sing x = [x]

main = 
  let w = 900 in
  Piece.run (Time.every 30)
    (let (blob, _, _) = theManifold {width=w, height=w} {seed=Random.initialSeed 0} in
    Signal.constant (Piece.sustain (Piece.map (collage w w << sing) blob)))
  {-
  Signal.foldps (\k -> Maybe.withDefault (\s -> (group [], Nothing, s)) (Dict.get k))
    ((group [], Nothing), initialState)
  |> Signal.map (\(
-}
-- I really wish I could have "integration" and even "effectful integration"
-- for Pieces so that I could make a random jitters function by just adding
-- a random step at every point

randomJitter maxj =
  let jit = Random.float -maxj maxj
      n   = 20
  in
  Random.list n jit
  |> Random.map (\xs -> interpolate Periodic xs << (\t -> (t / (4 * second)) * (n - 1)))

add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

atTheSameTime ps =
  let extract d = case d of
        PI.ForATime t -> Just t
        _             -> Nothing
      dur = List.minimum <| List.filterMap (\(PI.Piece d _) -> extract d) ps
  in
  Piece.for dur (\t -> List.map (\(PI.Piece _ f) -> f t) ps)

theManifold : Instrument
theManifold ctx s =
  let whomp            = Just "whomp"
      blackVelvet      = Color.black
      numPts           = 30
      samplesPerPt     = 10
      radius           = min ctx.width ctx.height / 2
      dur              = 2 * second
      (ratesAndJitters, seed') =
        Random.generate (Random.list numPts <| Random.zip (Random.float 1.8 2) (randomJitter 20))
          s.seed
      ptDatas : List (Float, Float, Float, Float -> Float)
      ptDatas =
        List.indexedMap (\i (rate, jit) -> 
          let theta  = 2 * pi * toFloat i / numPts
              r'     = radius * rate
              (c, s) = (cos theta, sin theta)
          in
          (r', c, s, jit))
          ratesAndJitters
      blob  =
        Piece.for dur <| \t ->
          let pts =
                List.map (\(r', c, s, jit) ->
                  let r_t = ease easeInExpo float 0 r' dur (t + 0.5 * second) + jit t in
                  (r_t * c, r_t * s))
                  ptDatas
              interped = interpolate2 Periodic pts
          in
          filled blackVelvet (
            polygon (
              listInit (numPts * samplesPerPt) (\i ->
                let pti = toFloat (i // samplesPerPt)
                    j   = toFloat i - samplesPerPt * pti
                in
                interped (pti + j / samplesPerPt))))
  in
  (blob, whomp, {s | seed <- seed'})

-- config
