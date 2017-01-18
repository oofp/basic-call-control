module SessionHandshake exposing(..)

import Json.Decode
-- import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict

type alias SWVersion  =
   { majorVersion: Int
   , midVersion: Int
   , minVersion: Int
   }

jsonDecSWVersion : Json.Decode.Decoder ( SWVersion )
jsonDecSWVersion =
   ("majorVersion" := Json.Decode.int) >>= \pmajorVersion ->
   ("midVersion" := Json.Decode.int) >>= \pmidVersion ->
   ("minVersion" := Json.Decode.int) >>= \pminVersion ->
   Json.Decode.succeed {majorVersion = pmajorVersion, midVersion = pmidVersion, minVersion = pminVersion}

jsonEncSWVersion : SWVersion -> Value
jsonEncSWVersion  val =
   Json.Encode.object
   [ ("majorVersion", Json.Encode.int val.majorVersion)
   , ("midVersion", Json.Encode.int val.midVersion)
   , ("minVersion", Json.Encode.int val.minVersion)
   ]



type SessionHandshake  =
    HaveSessionQ
    | NewSession SWVersion
    | SessionExist SWVersion String
    | UpgradeRequired
    | ContinueSession String
    | StartSession String

jsonDecSessionHandshake : Json.Decode.Decoder ( SessionHandshake )
jsonDecSessionHandshake =
    let jsonDecDictSessionHandshake = Dict.fromList
            [ ("HaveSessionQ", Json.Decode.succeed HaveSessionQ)
            , ("NewSession", Json.Decode.map NewSession (jsonDecSWVersion))
            , ("SessionExist", Json.Decode.map2 SessionExist (Json.Decode.index 0 (jsonDecSWVersion)) (Json.Decode.index 1 (Json.Decode.string)))
            , ("UpgradeRequired", Json.Decode.succeed UpgradeRequired)
            , ("ContinueSession", Json.Decode.map ContinueSession (Json.Decode.string))
            , ("StartSession", Json.Decode.map StartSession (Json.Decode.string))
            ]
    in  decodeSumObjectWithSingleField  "SessionHandshake" jsonDecDictSessionHandshake

jsonEncSessionHandshake : SessionHandshake -> Value
jsonEncSessionHandshake  val =
    let keyval v = case v of
                    HaveSessionQ  -> ("HaveSessionQ", encodeValue (Json.Encode.list []))
                    NewSession v1 -> ("NewSession", encodeValue (jsonEncSWVersion v1))
                    SessionExist v1 v2 -> ("SessionExist", encodeValue (Json.Encode.list [jsonEncSWVersion v1, Json.Encode.string v2]))
                    UpgradeRequired  -> ("UpgradeRequired", encodeValue (Json.Encode.list []))
                    ContinueSession v1 -> ("ContinueSession", encodeValue (Json.Encode.string v1))
                    StartSession v1 -> ("StartSession", encodeValue (Json.Encode.string v1))
    in encodeSumObjectWithSingleField keyval val
