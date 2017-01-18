module FlowAppData exposing(..)

import Json.Decode
--import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import BasicCallData exposing (..)


type ReservedData  =
    ReservedData

jsonDecReservedData : Json.Decode.Decoder ( ReservedData )
jsonDecReservedData =
    let jsonDecDictReservedData = Dict.fromList [("ReservedData", ReservedData)]
    in  decodeSumUnaries "ReservedData" jsonDecDictReservedData

jsonEncReservedData : ReservedData -> Value
jsonEncReservedData  val =
    case val of
        ReservedData -> Json.Encode.string "ReservedData"



type alias DialConfig  =
   { phoneNum: String
   , callerID: (Maybe String)
   , callerName: (Maybe String)
   , extraDialConfig: (Maybe (List (String, String)))
   }

jsonDecDialConfig : Json.Decode.Decoder ( DialConfig )
jsonDecDialConfig =
   ("phoneNum" := Json.Decode.string) >>= \pphoneNum ->
   (Json.Decode.maybe ("callerID" := Json.Decode.string)) >>= \pcallerID ->
   (Json.Decode.maybe ("callerName" := Json.Decode.string)) >>= \pcallerName ->
   (Json.Decode.maybe ("extraDialConfig" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))))) >>= \pextraDialConfig ->
   Json.Decode.succeed {phoneNum = pphoneNum, callerID = pcallerID, callerName = pcallerName, extraDialConfig = pextraDialConfig}

jsonEncDialConfig : DialConfig -> Value
jsonEncDialConfig  val =
   Json.Encode.object
   [ ("phoneNum", Json.Encode.string val.phoneNum)
   , ("callerID", (maybeEncode (Json.Encode.string)) val.callerID)
   , ("callerName", (maybeEncode (Json.Encode.string)) val.callerName)
   , ("extraDialConfig", (maybeEncode ((Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(Json.Encode.string) v1,(Json.Encode.string) v2])))) val.extraDialConfig)
   ]

type alias AccountConfig  =
   { sipUserName: String
   , sipPassword: String
   , sipDomain: String
   , sipDirectMedia: Bool
   , sipEnableRPID: Bool
   , sipExtraConfig: (Maybe (List (String, String)))
   }

jsonDecAccountConfig : Json.Decode.Decoder ( AccountConfig )
jsonDecAccountConfig =
   ("sipUserName" := Json.Decode.string) >>= \psipUserName ->
   ("sipPassword" := Json.Decode.string) >>= \psipPassword ->
   ("sipDomain" := Json.Decode.string) >>= \psipDomain ->
   ("sipDirectMedia" := Json.Decode.bool) >>= \psipDirectMedia ->
   ("sipEnableRPID" := Json.Decode.bool) >>= \psipEnableRPID ->
   (Json.Decode.maybe ("sipExtraConfig" := Json.Decode.list (Json.Decode.map2 (,) (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.string))))) >>= \psipExtraConfig ->
   Json.Decode.succeed {sipUserName = psipUserName, sipPassword = psipPassword, sipDomain = psipDomain, sipDirectMedia = psipDirectMedia, sipEnableRPID = psipEnableRPID, sipExtraConfig = psipExtraConfig}

jsonEncAccountConfig : AccountConfig -> Value
jsonEncAccountConfig  val =
   Json.Encode.object
   [ ("sipUserName", Json.Encode.string val.sipUserName)
   , ("sipPassword", Json.Encode.string val.sipPassword)
   , ("sipDomain", Json.Encode.string val.sipDomain)
   , ("sipDirectMedia", Json.Encode.bool val.sipDirectMedia)
   , ("sipEnableRPID", Json.Encode.bool val.sipEnableRPID)
   , ("sipExtraConfig", (maybeEncode ((Json.Encode.list << List.map (\(v1,v2) -> Json.Encode.list [(Json.Encode.string) v1,(Json.Encode.string) v2])))) val.sipExtraConfig)
   ]


type RequestData  =
    SetSIPAccount (Maybe AccountConfig)
    | SetDialConfig (Maybe DialConfig)
    | BCRequest BasicCallReq
    | GetSnapshot

jsonDecRequestData : Json.Decode.Decoder ( RequestData )
jsonDecRequestData =
    let jsonDecDictRequestData = Dict.fromList
            [ ("SetSIPAccount", Json.Decode.map SetSIPAccount (Json.Decode.maybe (jsonDecAccountConfig)))
            , ("SetDialConfig", Json.Decode.map SetDialConfig (Json.Decode.maybe (jsonDecDialConfig)))
            , ("BCRequest", Json.Decode.map BCRequest (jsonDecBasicCallReq))
            , ("GetSnapshot", Json.Decode.succeed GetSnapshot)
            ]
    in  decodeSumObjectWithSingleField  "RequestData" jsonDecDictRequestData

jsonEncRequestData : RequestData -> Value
jsonEncRequestData  val =
    let keyval v = case v of
                    SetSIPAccount v1 -> ("SetSIPAccount", encodeValue ((maybeEncode (jsonEncAccountConfig)) v1))
                    SetDialConfig v1 -> ("SetDialConfig", encodeValue ((maybeEncode (jsonEncDialConfig)) v1))
                    BCRequest v1 -> ("BCRequest", encodeValue (jsonEncBasicCallReq v1))
                    GetSnapshot  -> ("GetSnapshot", encodeValue (Json.Encode.list []))
    in encodeSumObjectWithSingleField keyval val



type alias Request  =
   { reqID: String
   , reqData: RequestData
   }

jsonDecRequest : Json.Decode.Decoder ( Request )
jsonDecRequest =
   ("reqID" := Json.Decode.string) >>= \preqID ->
   ("reqData" := jsonDecRequestData) >>= \preqData ->
   Json.Decode.succeed {reqID = preqID, reqData = preqData}

jsonEncRequest : Request -> Value
jsonEncRequest  val =
   Json.Encode.object
   [ ("reqID", Json.Encode.string val.reqID)
   , ("reqData", jsonEncRequestData val.reqData)
   ]



type Event  =
    RequestConf String
    | BCEvent BasicCallRS

jsonDecEvent : Json.Decode.Decoder ( Event )
jsonDecEvent =
    let jsonDecDictEvent = Dict.fromList
            [ ("RequestConf", Json.Decode.map RequestConf (Json.Decode.string))
            , ("BCEvent", Json.Decode.map BCEvent (jsonDecBasicCallRS))
            ]
    in  decodeSumObjectWithSingleField  "Event" jsonDecDictEvent

jsonEncEvent : Event -> Value
jsonEncEvent  val =
    let keyval v = case v of
                    RequestConf v1 -> ("RequestConf", encodeValue (Json.Encode.string v1))
                    BCEvent v1 -> ("BCEvent", encodeValue (jsonEncBasicCallRS v1))
    in encodeSumObjectWithSingleField keyval val
