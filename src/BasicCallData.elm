module BasicCallData exposing(..)

import Json.Decode
-- import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set


type alias DestParty  =
   { destNum: String
   , destName: (Maybe String)
   }

jsonDecDestParty : Json.Decode.Decoder ( DestParty )
jsonDecDestParty =
   ("destNum" := Json.Decode.string) >>= \pdestNum ->
   (Json.Decode.maybe ("destName" := Json.Decode.string)) >>= \pdestName ->
   Json.Decode.succeed {destNum = pdestNum, destName = pdestName}

jsonEncDestParty : DestParty -> Value
jsonEncDestParty  val =
   Json.Encode.object
   [ ("destNum", Json.Encode.string val.destNum)
   , ("destName", (maybeEncode (Json.Encode.string)) val.destName)
   ]



type BasicCallReq  =
    MakeCall DestParty
    | Dial DestParty
    | Hangup
    | DropCall

jsonDecBasicCallReq : Json.Decode.Decoder ( BasicCallReq )
jsonDecBasicCallReq =
    let jsonDecDictBasicCallReq = Dict.fromList
            [ ("MakeCall", Json.Decode.map MakeCall (jsonDecDestParty))
            , ("Dial", Json.Decode.map Dial (jsonDecDestParty))
            , ("Hangup", Json.Decode.succeed Hangup)
            , ("DropCall", Json.Decode.succeed DropCall)
            ]
    in  decodeSumObjectWithSingleField  "BasicCallReq" jsonDecDictBasicCallReq

jsonEncBasicCallReq : BasicCallReq -> Value
jsonEncBasicCallReq  val =
    let keyval v = case v of
                    MakeCall v1 -> ("MakeCall", encodeValue (jsonEncDestParty v1))
                    Dial v1 -> ("Dial", encodeValue (jsonEncDestParty v1))
                    Hangup  -> ("Hangup", encodeValue (Json.Encode.list []))
                    DropCall  -> ("DropCall", encodeValue (Json.Encode.list []))
    in encodeSumObjectWithSingleField keyval val



type BasicCallRS  =
    NoCall
    | Initiated DestParty
    | RingingCaller DestParty
    | CallerIdle
    | InitiatedCallee DestParty
    | RingingCallee DestParty
    | Connected DestParty

jsonDecBasicCallRS : Json.Decode.Decoder ( BasicCallRS )
jsonDecBasicCallRS =
    let jsonDecDictBasicCallRS = Dict.fromList
            [ ("NoCall", Json.Decode.succeed NoCall)
            , ("Initiated", Json.Decode.map Initiated (jsonDecDestParty))
            , ("RingingCaller", Json.Decode.map RingingCaller (jsonDecDestParty))
            , ("CallerIdle", Json.Decode.succeed CallerIdle)
            , ("InitiatedCallee", Json.Decode.map InitiatedCallee (jsonDecDestParty))
            , ("RingingCallee", Json.Decode.map RingingCallee (jsonDecDestParty))
            , ("Connected", Json.Decode.map Connected (jsonDecDestParty))
            ]
    in  decodeSumObjectWithSingleField  "BasicCallRS" jsonDecDictBasicCallRS

jsonEncBasicCallRS : BasicCallRS -> Value
jsonEncBasicCallRS  val =
    let keyval v = case v of
                    NoCall  -> ("NoCall", encodeValue (Json.Encode.list []))
                    Initiated v1 -> ("Initiated", encodeValue (jsonEncDestParty v1))
                    RingingCaller v1 -> ("RingingCaller", encodeValue (jsonEncDestParty v1))
                    CallerIdle  -> ("CallerIdle", encodeValue (Json.Encode.list []))
                    InitiatedCallee v1 -> ("InitiatedCallee", encodeValue (jsonEncDestParty v1))
                    RingingCallee v1 -> ("RingingCallee", encodeValue (jsonEncDestParty v1))
                    Connected v1 -> ("Connected", encodeValue (jsonEncDestParty v1))
    in encodeSumObjectWithSingleField keyval val
