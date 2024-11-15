port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E



-- MAIN


main : Program E.Value Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Model =
  { name : String
  , beruf : Beruf
  , stärke : Würfel
  , geschick : Würfel
  , weisheit : Würfel
  , mut : Würfel }

type alias Beruf =
    { name : String
    , gegenstand : String }

feuerwehrmann : Beruf
feuerwehrmann =
    { name = "Feuerwehrmann"
    , gegenstand = "Axt" }

obdachloser : Beruf
obdachloser =
    { name = "Obdachloser"
    , gegenstand = "Glasflasche" }

type Würfel = D4 | D6 | D8 | D10 | D12 | D20


-- Here we use "flags" to load information in from localStorage. The
-- data comes in as a JS value, so we define a `decoder` at the bottom
-- of this file to turn it into an Elm value.
--
-- Check out index.html to see the corresponding code on the JS side.
--
init : E.Value -> ( Model, Cmd Msg )
init flags =
  (
    case D.decodeValue decoder flags of
      Ok model -> model
      Err _ ->
        { name = ""
        , beruf = obdachloser
        , stärke = D8
        , geschick = D8
        , mut = D8
        , weisheit = D8 }
  , Cmd.none )



-- UPDATE


type Msg
  = NameGeändert String
  | BerufGeändert Beruf

zuBerufGeändert : String -> Msg
zuBerufGeändert beruf = BerufGeändert ( case beruf of
    "Feuerwehrmann" -> feuerwehrmann
    _ -> obdachloser )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NameGeändert name ->
      ( { model | name = name }
      , Cmd.none )

    BerufGeändert beruf ->
      ( { model | beruf = beruf }
      , Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input
        [ type_ "text"
        , placeholder "Name"
        , onInput NameGeändert
        , value model.name ]
        []
    , select
        [ placeholder "Beruf"
        , onInput zuBerufGeändert
        , value model.beruf.name ]
        [ option [ value "Feuerwehrmann" ] [ text "Feuerwehrmann" ] ]
    ]



-- PORTS


port setStorage : E.Value -> Cmd msg


-- We want to `setStorage` on every update, so this function adds
-- the setStorage command on each step of the update function.
--
-- Check out index.html to see how this is handled on the JS side.
--
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
  let
    ( newModel, cmds ) = update msg oldModel
  in
  ( newModel
  , Cmd.batch [ setStorage (encode newModel), cmds ]
  )



-- JSON ENCODE/DECODE


encode : Model -> E.Value
encode model =
  E.object
    [ ("name", E.string model.name)
    , ("beruf", encodeBeruf model.beruf)
    , ("stärke", encodeWürfel model.stärke) ]

encodeBeruf : Beruf -> E.Value
encodeBeruf beruf = E.object
    [ ("name", E.string beruf.name)
    , ("gegenstand", E.string beruf.gegenstand) ]

encodeWürfel : Würfel -> E.Value
encodeWürfel würfel = case würfel of
    D4 -> E.object [ ("type", E.string "D4") ]
    D6 -> E.object [ ("type", E.string "D6") ]
    D8 -> E.object [ ("type", E.string "D8") ]
    D10 -> E.object [ ("type", E.string "D10") ]
    D12 -> E.object [ ("type", E.string "D12") ]
    D20 -> E.object [ ("type", E.string "D20") ]



decoder : D.Decoder Model
decoder =
  D.map6 Model
    (D.field "name" D.string)
    (D.field "beruf" decodeBeruf)
    (D.field "stärke" decodeWürfel)
    (D.field "geschick" decodeWürfel)
    (D.field "weisheit" decodeWürfel)
    (D.field "mut" decodeWürfel)

decodeBeruf : D.Decoder Beruf
decodeBeruf = D.map2 Beruf
    (D.field "name" D.string)
    (D.field "gegenstand" D.string)

decodeWürfel : D.Decoder Würfel
decodeWürfel = D.string |> D.andThen (\typ ->
    case typ of
        "D4" -> D.succeed D4
        "D6" -> D.succeed D6
        "D8" -> D.succeed D8
        "D10" -> D.succeed D10
        "D12" -> D.succeed D12
        "D20" -> D.succeed D20
        _ -> D.fail ("Würfel kann nicht ermittelt werden: " ++ typ) )
