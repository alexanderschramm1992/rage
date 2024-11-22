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
  , alter: Maybe Int
  , lebenspunkte: Int
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

sanitäter : Beruf
sanitäter =
    { name = "Sanitäter"
    , gegenstand = "Erste Hilfe Set" }

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
        , alter = Nothing
        , lebenspunkte = 2
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
  | AlterGeändert (Maybe Int)
  | LebenspunkteGeändert Int

zuBerufGeändert : String -> Msg
zuBerufGeändert beruf = BerufGeändert ( case beruf of
    "Feuerwehrmann" -> feuerwehrmann
    "Sanitäter" -> sanitäter
    _ -> obdachloser )

zuAlterGeändert : String -> Msg
zuAlterGeändert alter = AlterGeändert (String.toInt alter)

zuLebenspunkteGeändert : String -> Msg
zuLebenspunkteGeändert lebenspunkte = lebenspunkte
    |> String.toInt
    |> Maybe.withDefault 2
    |> LebenspunkteGeändert

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NameGeändert name ->
      ( { model | name = name }
      , Cmd.none )

    BerufGeändert beruf ->
      ( { model | beruf = beruf }
      , Cmd.none )

    AlterGeändert alter ->
      ( { model | alter = alter }
      , Cmd.none )

    LebenspunkteGeändert lebenspunkte ->
      ( { model | lebenspunkte = lebenspunkte }
      , Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
  article [ class "main card" ]
    [ div []
        [ div [ class "flex two" ]
            [ div []
                [ label [ for "name-input" ]
                    [ text "Name" ]
                , input
                    [ id "name-input"
                    , type_ "text"
                    , placeholder "Name"
                    , onInput NameGeändert
                    , value model.name ]
                    [] ]
            , div []
                [ label [ for "beruf-input" ]
                    [ text "Beruf" ]
                , select
                    [ id "beruf-input"
                    , placeholder "Beruf"
                    , onInput zuBerufGeändert
                    , value model.beruf.name ]
                    [ option [ value "Feuerwehrmann" ] [ text "Feuerwehrmann" ]
                    , option [ value "Sanitäter" ] [ text "Sanitäter" ]
                    , option [ value "Obdachloser" ] [ text "Obdachloser" ] ] ] ]
        , div [ class "flex two" ]
            [ div []
                [ label [ for "alter-input" ]
                    [ text "Alter" ]
                , input
                    [ id "alter-input"
                    , type_ "number"
                    , placeholder "Alter"
                    , onInput zuAlterGeändert
                    , model.alter
                        |> Maybe.withDefault 25
                        |> String.fromInt
                        |> value ]
                    [] ]
            , div []
                [ label [ for "lebenspunkte-input" ]
                    [ text "Lebenspunkte" ]
                , input
                    [ type_ "number"
                    , placeholder "Lebenspunkte"
                    , onInput zuLebenspunkteGeändert
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "3"
                    , model.lebenspunkte
                        |> String.fromInt
                        |> value ]
                    [] ] ] ] ]



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
    , ("alter", E.int (Maybe.withDefault 30 model.alter))
    , ("lebenspunkte", E.int model.lebenspunkte)
    , ("beruf", encodeBeruf model.beruf)
    , ("stärke", encodeWürfel model.stärke)
    , ("geschick", encodeWürfel model.geschick)
    , ("weisheit", encodeWürfel model.weisheit)
    , ("mut", encodeWürfel model.mut)]

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
  D.map8 Model
    (D.field "name" D.string)
    (D.field "alter" (D.maybe D.int))
    (D.field "lebenspunkte" D.int)
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
