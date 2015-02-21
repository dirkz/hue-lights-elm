module Bridges where

import Http
import Html (..)
import Html.Attributes
import Html.Events (..)
import Signal
import Json.Decode (Decoder (..), object4, (:=), string, decodeString, list)
import Result

type alias Bridge =
    { internalipaddress: String
    , id: String
    , macaddress: String
    , name: String
    } 

type alias Model =
    { message : String
    , status : Int
    , bridges : List Bridge
    }

type Action = Waiting | Update String | Failure Int String

emptyModel : Model
emptyModel = { message = "", status = 0, bridges = [] }

update : Action -> Model -> Model
update action model =
    case action of
        Waiting -> { emptyModel | message <- "Waiting" }
        Update s -> { emptyModel | bridges <- stringToBridges s }
        Failure n s -> { emptyModel | message <- s, status <- n }

view : Model -> Html
view model =
    div [] [ text (toString model) ]

makeBridge4 : String -> String -> String -> String -> Bridge
makeBridge4 ip id mac name =
    { internalipaddress = ip, id = id, macaddress = mac, name = name }

stringToBridges : String -> List Bridge
stringToBridges s =
   let decoded = decodeString bridgesDecoder s
   in case decoded of
       Result.Ok s -> s
       otherwise -> []

bridgeDecoder : Decoder Bridge
bridgeDecoder =
    object4 makeBridge4
        ("internalipaddress" := string)
        ("id" := string)
        ("macaddress" := string)
        ("name" := string)

bridgesDecoder : Decoder (List Bridge)
bridgesDecoder = list bridgeDecoder

receiveBridges : Http.Response String -> Action
receiveBridges res = case res of
        Http.Success s -> Update s
        Http.Waiting -> Waiting
        Http.Failure n s-> Failure n s

loadBridges : Signal (Http.Response String)
loadBridges = Http.sendGet <| Signal.constant "https://www.meethue.com/api/nupnp"

loadBridgesUpdate : Signal Action
loadBridgesUpdate = (Signal.map receiveBridges loadBridges)

main : Signal Html
main =
  Signal.map view model

model : Signal Model
model =
  Signal.foldp update emptyModel
  <| Signal.merge (Signal.subscribe actionChannel) loadBridgesUpdate

actionChannel : Signal.Channel Action
actionChannel =
  Signal.channel Waiting

