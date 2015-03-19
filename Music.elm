module Music where

import Keyboard
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

type alias Contextual a  = Context -> a
type alias Stateful a    = State -> (a, State)
type alias SoundCode     = Maybe String
type alias Context       = { width : Float, height : Float }
type alias State         = { seed : Random.Seed }
type alias NoiseAndLight = { noise : SoundCode, light : Piece ForATime Form }
type alias AppState      =
  { state         : State
  , filter        : Filter
  , noiseAndLight : NoiseAndLight
  }

type alias Filter = Contextual (Piece Forever (Form -> Form))

-- noise and light
type alias Instrument = Contextual (Stateful NoiseAndLight)
-- actual type should be 
-- type alias Instrument = State -> (Piece ForATime (Context -> Form), SoundCode, State)

it'sAllAboutContext : Contextual Context
it'sAllAboutContext ctx = ctx

theContext : Context
theContext = { width = 300, height = 300 }

ignoreTheContext : a -> Contextual a
ignoreTheContext x = \_ -> x

don'tTouchThatState : a -> Stateful a
don'tTouchThatState x = \s -> (x, s)

idFilter = ignoreTheContext <| Piece.stayForever (\x -> x)

silently x = {noise=Nothing, light=x}
silentlyAndStatelessly = don'tTouchThatState << silently

sightAndSound : Piece ForATime Form -> String -> Stateful NoiseAndLight
sightAndSound x sound = don'tTouchThatState {noise=Just sound, light=x}

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
  |> silentlyAndStatelessly

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
filters = Dict.fromList
  [ (Char.toCode 'i', idFilter)
  , (Char.toCode 'r', spinning)
  , (Char.toCode 'g', gridulate)
  ]

sing x = [x]

keys = Signal.map2 (,) Keyboard.ctrl Keyboard.keysDown |> Signal.map (Debug.watch "keyses")

rest : Instrument
rest = ignoreTheContext <| don'tTouchThatState <| silently <| Piece.stayFor 0 <| group []

eternalNothingness = Piece.stayForever (group [])

main =
  let w = 300
      initialState =
        { state         = {seed = Random.initialSeed 0}
        , noiseAndLight = {noise=Nothing, light=Piece.stayFor 0 (group [])}
        , filter        = idFilter
        }
      update : (Bool, List KeyCode) -> AppState -> AppState
      update (ctrl, ks) s = case ks of
        []   -> s
        k::_ ->
          if ctrl
          then {s | filter <- Maybe.withDefault s.filter (Dict.get k filters)}
          else 
            let (nl, st') = Maybe.withDefault rest (Dict.get k theBand) theContext s.state in
            {s | state <- st', noiseAndLight <- nl}
      stateSig = Signal.foldp update initialState keys
      pics =
        Signal.map (\s -> s.noiseAndLight.light) stateSig
        |> Piece.layer (Time.every 30)
        |> Signal.map (collage (floor theContext.width) (floor theContext.height))
  in
  pics
  {-
  Piece.run (Time.every 30)
    (let (nl, _) = shootingArrow {width=w, height=w} {seed=Random.initialSeed 0} in
    Signal.constant (Piece.sustain (Piece.map (collage w w << sing) nl.light)))
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
      dur              = 6 * second
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
                let offset = 0 * second in
                List.map (\(r', c, s, jit) ->
                  let r_t = ease easeInQuint float 0 r' dur (t + offset) + jit t in
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
  ({noise=whomp, light=blob}, {s | seed <- seed'})

-- flying through the air type whistle
shootingArrow : Instrument
shootingArrow ctx =
  let arrow   = group
        [ rect 20 3 |> filled Color.black
        , ngon 3 5 |> filled Color.black |> moveX 10
        ]
      angle x = atan2 (-2 * ctx.height/((ctx.width/2)^2) * x) 1
      xpos t  = ease linear float (-ctx.width/2) (ctx.width/2) dur t
      ypos x  = -ctx.height / (ctx.width/2)^2 * x^2 + (ctx.height/2)
      dur     = 2 * second
  in
  sightAndSound
    (Piece.for dur (\t ->
      let x = xpos t in arrow |> rotate (angle x) |> move (x, ypos x)))
    "whistle"

