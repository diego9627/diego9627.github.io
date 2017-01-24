module Game exposing (..)

import Html exposing (Html, text)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Random exposing (..)
import Time exposing (Time)
import Helpers exposing (..)
import Svg exposing (svg,Svg)
import Svg.Attributes 
import List exposing (head, sort)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL

type alias Jumper = 
    { y : Float
    , vy : Float
    }

type alias Model =
    { jumper : Jumper
    , trees : List Float
    , lostGame : Bool
    , timePassed : Time
    , highScore : Time
    }


modelInit : Model
modelInit =
    { jumper = Jumper 0 0
    , trees = []
    , lostGame = False
    , timePassed = 0
    , highScore = 0
    }



init : ( Model, Cmd Msg )
init =
    ( modelInit, Cmd.none )



-- UPDATE


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | AddTree Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
      commandToDo =
        case lastElem model.trees of
            Nothing ->
                Random.generate AddTree (float 30 50)
            
            Just a ->
                if a < 20 then Random.generate AddTree (float (a+30) (a+50)) else Cmd.none

      modelNew =
        case msg of
          TimeUpdate dt -> 
              applyPhysics dt model 

          KeyDown keyCode ->
              keyDown keyCode model
          
          AddTree newTree ->
              addTree newTree model
        
    in 
      ( modelNew, commandToDo )

-- Controls

keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Helpers.fromCode keyCode of
        Space ->
            modelJump model

        KeyR ->
            restartGame model

        _ ->
            model

modelJump : Model -> Model
modelJump model =
    { model | jumper = jumperJump model.jumper }

jumpSpeed : Float
jumpSpeed = 2

jumperJump : Jumper -> Jumper
jumperJump jumper = 
    { jumper | vy = if jumper.y > 0 then jumper.vy else jumpSpeed }

-- "Physics" Engine

applyPhysics : Float -> Model -> Model
applyPhysics dt model =
    model
      |> (if model.lostGame then (\model -> model) else applyChanges dt)
      |> checkCollision

applyChanges : Float -> Model -> Model
applyChanges dt model =
    { model | 
        jumper = 
           model.jumper 
              |> changeVertical dt
              |> changeSpeedJumper dt,
        trees = 
            model.trees
              |> moveTrees dt
              |> filterBadTrees,
        timePassed = 
            model.timePassed + dt,
        highScore = 
            if model.timePassed + dt> model.highScore then model.timePassed + dt else model.highScore
    }

checkCollision : Model -> Model
checkCollision model =
    { model | 
        lostGame = 
          case head model.trees of
              Nothing ->
                  model.lostGame

              Just tree ->
                  (tree <= 1) && (tree >= -0.5) && (model.jumper.y < 5)
    }

treeSpeed : Float
treeSpeed = 0.01

moveTrees : Float -> List Float -> List Float
moveTrees dt trees = 
    List.map (\tree -> tree - dt*treeSpeed) trees

filterBadTrees : List Float -> List Float
filterBadTrees trees =
    List.filter (\tree -> tree > 0) trees

changeVertical : Float -> Jumper -> Jumper
changeVertical dt jumper =
    { jumper | y = max 0 (jumper.y + dt*jumper.vy)}

gravity = (-0.005)*jumpSpeed

changeSpeedJumper : Float -> Jumper -> Jumper
changeSpeedJumper dt jumper =
    { jumper | vy = if jumper.y > 0 then jumper.vy + gravity*dt else 0}


-- Other Logic

addTree : Float -> Model -> Model
addTree newTree model = 
    { model | trees = sort (model.trees ++ [newTree])}

restartGame : Model -> Model
restartGame model =
    { modelInit | highScore = model.highScore } 

-- VIEW


viewAlt : Model -> Html msg
viewAlt model =
   text (toString model)
    

view : Model -> Html msg
view model =
    svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ] 
      ([ Svg.text_ 
          [ Svg.Attributes.x "100"
          , Svg.Attributes.y "20"
          ] [ Svg.text (toString model.timePassed ++ " - " ++ toString model.highScore) ]
      , Svg.rect 
          [ Svg.Attributes.y (toString (500 - 50 - model.jumper.y))
          , Svg.Attributes.x "50"
          , Svg.Attributes.width "10"
          , Svg.Attributes.height "10"
          , Svg.Attributes.fill "#0B79CE"
          ] [ ]
      ] ++ List.map drawTree model.trees)

drawTree : Float -> Svg msg
drawTree treeDistance =
    Svg.rect 
      [ Svg.Attributes.y (toString (500 - 50))
      , Svg.Attributes.x (toString (50 + treeDistance*10))
      , Svg.Attributes.width "5"
      , Svg.Attributes.height "5"
      , Svg.Attributes.fill "#555555"
      ] [ ]

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        ]