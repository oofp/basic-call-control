port module CellSIP exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import WebSocket exposing (..)
import SessionHandshake exposing (..)
import BasicCallData exposing (..)
import FlowAppData exposing (..)
import MiscUtils exposing (..)
import ModelData exposing (..)
import UserData exposing (..)
import FlowAppData exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Messages exposing (..)
import Date exposing (..)
import Task exposing (..)
import ViewEditSIPAccount exposing (..)
import ViewEditDialParams exposing (..)
import ViewMainScreen exposing (..)
import ViewConnecting exposing (..)

main : Program (Maybe StoredData) Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = subscriptions
    }

port setStorage : StoredData -> Cmd msg
port pickContact : () -> Cmd msg
port contactPicker : (ContactDetails -> msg) -> Sub msg
--port contactPicker : (List String -> msg) -> Sub msg

appServer : String
appServer =
--  "ws://192.168.2.68:8012"
    -- "ws://localhost:8012"
--  "wss://echo.websocket.org"
--  "ws://192.168.2.72:8012"
--    "ws://192.168.2.83:80"
    "wss://sip.uno"

clientVersion : SWVersion
clientVersion = SWVersion 1 0 0

-- init : Maybe StoredData -> (Model, Cmd Msg)
init : Maybe StoredData -> (Model, Cmd Msg)
init storedDataMaybe = case (storedDataMaybe) of
  Just storedData -> (modelFromStoreData storedData , Cmd.none)
  Nothing         -> (emptyModel, Cmd.none)

-- UPDATE
-- just append setStorage to currently posted commands
updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, flStore, cmds) =
      update msg model
  in
    ( newModel
    , if flStore
        then Cmd.batch [ setStorage (storeDataFromModel newModel), cmds ]
        else cmds
    )

-- cmdWithDate : (Date -> Msg) -> Cmd Msg
-- cmdWithDate msgWithData = perform (\_ -> SystemError "Failed to get date") msgWithData now

-- cmdStartStartCall : String -> String -> Cmd Msg
-- cmdStartStartCall num name = StartCall {number = num , name = name}

cmdsOnStart : UserData -> List (Cmd Msg)
cmdsOnStart userData =
  start_l |> (al (prepareServerRequestCmd GetSnapshot))
          |> (al_if_just_map (List.head userData.sipAccounts) (\sipAccount -> prepareServerRequestCmd (SetSIPAccount (Just sipAccount))))
          |> (al_if_just_map userData.dialConfig (\dialConfig -> prepareServerRequestCmd (SetDialConfig (Just dialConfig))))

prepareServerRequestCmd : RequestData -> Cmd Msg
prepareServerRequestCmd reqData = WebSocket.send appServer (encode 0 (jsonEncRequest (Request "_" reqData)))

doServerRequest : Bool -> (Model, List (Cmd Msg)) -> (Model, Bool, Cmd Msg)
doServerRequest  flStore (model, cmds) = ({model | pendingRequests = (model.pendingRequests +(List.length cmds))}, flStore, Cmd.batch cmds)

handleServerRequestMsg : Model -> RequestData -> (Model, Bool, Cmd Msg)
handleServerRequestMsg model reqData = doServerRequest False (model, [prepareServerRequestCmd reqData])

startSession : String -> Model -> (Model, Bool, Cmd Msg)
startSession sessionID model = doServerRequest False (model |> setSessionEstablished sessionID |> resetPendingRequests |> setInitUIMode , (cmdsOnStart model.userData))

sessionHandshakeStr : SessionHandshake -> String
sessionHandshakeStr sessionHandshake = encode 0 (jsonEncSessionHandshake sessionHandshake)

getSessionResponse : Model -> SessionHandshake
getSessionResponse model = case getCurrentSessionID model.sessionState of
  Just sID -> SessionExist clientVersion sID
  Nothing -> NewSession clientVersion

doNothing : Model -> (Model, Bool, Cmd Msg)
doNothing model = (model, False, Cmd.none)

switchToMainScreen : Model -> (Model, Bool, Cmd Msg)
switchToMainScreen model = (setUIState MainScreen model, False, Cmd.none)

destPartyDigits : DestParty -> Maybe DestParty
destPartyDigits destPary = case extractDigits  destPary.destNum of
  Nothing -> Nothing
  Just newNum -> Just {destPary | destNum = newNum}

handleDialReq : Model -> DestParty -> (DestParty -> (Model, Bool, Cmd Msg)) -> (Model, Bool, Cmd Msg)
handleDialReq model destParty func =
  Maybe.map func (destPartyDigits destParty) |> Maybe.withDefault (model, False, Cmd.none)

update : Msg -> Model -> (Model, Bool, Cmd Msg)
update msg model= case msg of
  EditSipAccount StartEditting -> (startSIPAccountEditting model, False, Cmd.none)
  EditSipAccount (Editting f) -> (model |> updateEditSipAccount f, False, Cmd.none)
  EditSipAccount Cancel -> switchToMainScreen model
  EditSipAccount (Save sipAccount) -> doServerRequest True (model |> setSipAccount sipAccount |> storeSipAccountsHistory sipAccount |> setUIState MainScreen , [prepareServerRequestCmd (SetSIPAccount (Just sipAccount))])

  EditConfig StartEditting -> (startDialConfigEditting model, False, Cmd.none)
  EditConfig (Editting f) -> (model |> updateEditDialConfig f, False, Cmd.none)
  EditConfig Cancel -> switchToMainScreen model
  EditConfig (Save dialConfig) -> doServerRequest True (model |> setDialConfig dialConfig |> storeDialConfigHistory dialConfig |> setUIState MainScreen , [prepareServerRequestCmd (SetDialConfig (Just dialConfig))])

  EditMainScreen f -> (model |> updateEditMainScreen f, False, Cmd.none)

  ClearSIPAccountMsg -> doServerRequest True (model |> clearSIPAccount , [prepareServerRequestCmd (SetSIPAccount Nothing)])
  ClearDialConfigMsg -> doServerRequest True (model |> clearDialConfig , [prepareServerRequestCmd (SetDialConfig Nothing)])

  ClearHistoryEntryMsg he -> (model |> updateUserData (removeHistoryEntry he), True, Cmd.none)
  ClearAllHistoryMsg -> (model |> updateUserData clearHistory, True, Cmd.none)

  MakeCallMsg destParty dateNow -> handleDialReq model destParty (\digitsParty -> doServerRequest True (storeCallHistory destParty dateNow model , [prepareServerRequestCmd (BCRequest (MakeCall digitsParty))]))
  DialMsg destParty dateNow-> handleDialReq model destParty (\digitsParty -> doServerRequest True (storeCallHistory destParty dateNow model , [prepareServerRequestCmd (BCRequest (Dial destParty))]))
  HangupMsg -> doServerRequest False (model , [prepareServerRequestCmd (BCRequest Hangup)])
  DropCallMsg -> doServerRequest False (model , [prepareServerRequestCmd (BCRequest DropCall)])

  MsgWithDate dateMsgFunc -> (model, False, perform dateMsgFunc Date.now)

  SessionMsgReceived HaveSessionQ ->
    (model |> setWaitForSession, False, WebSocket.send appServer (sessionHandshakeStr (getSessionResponse model)))
  SessionMsgReceived (ContinueSession sID) ->
    startSession sID model
  SessionMsgReceived (StartSession sID) ->
    startSession sID model
  SessionMsgReceived (SessionExist _ _) -> doNothing model --cannot happen as always outgoing message
  SessionMsgReceived (NewSession _) -> doNothing model --cannot happen as always outgoing message
  SessionMsgReceived UpgradeRequired -> (model |> setUpgradeRequired, False, Cmd.none)

  ServerEventReceived (RequestConf _) -> (updateWithConfReceived model, False, Cmd.none)
  ServerEventReceived (BCEvent bcStateRS) -> (updateBCState bcStateRS model, False, Cmd.none)

  ContactPicked contactDetails ->
    let destParty = destPartyFromContactDetails contactDetails
        dialMsgMaybe = dialingMsg model
    in case dialMsgMaybe of
      Nothing -> (model, False, Cmd.none) -- cannot dial
      Just dialMsg -> (model , False, perform (dialMsg destParty)  Date.now)

  PickContact -> (model, False, pickContact ())

  SystemError errStr ->
    ({model | lastMessage = errStr} , False, Cmd.none)

destPartyFromContactDetails : ContactDetails -> DestParty
destPartyFromContactDetails cd = DestParty cd.contactNum (Just cd.contactName)

dialingMsg : Model -> Maybe (DestParty -> Date -> Msg)
dialingMsg model = case model.basicCallRS of
    NoCall -> Just MakeCallMsg
    CallerIdle -> Just DialMsg
    _ -> Nothing

createCallUpdateMsg : String -> Msg
createCallUpdateMsg str =  case  decodeString  jsonDecEvent str of
  Ok eventReceived ->          ServerEventReceived eventReceived
  Err err                  ->  createHandshakeInMsg str

createHandshakeInMsg : String -> Msg
createHandshakeInMsg str =  case  decodeString  jsonDecSessionHandshake str of
  Ok sessionHandshakeIn   ->  SessionMsgReceived sessionHandshakeIn
  Err err                 ->  SystemError ("createHandshakeInMsg:"++err)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
   [ WebSocket.listen appServer createCallUpdateMsg
   , contactPicker ContactPicked
   ]

{-
narrowStyle =
  style
    [ ("width", "1px")
    , ("white-space", "nowrap")
    ]
-}

pendingRequestsBadge : Model -> List (Html Msg)
pendingRequestsBadge model =
  if model.pendingRequests> 0
    then [span [class "badge"] [text (toString model.pendingRequests)]]
    else []

view : Model -> Html Msg
view model =
  div [class "container"]
    [div [class "row"]
      [ div [class "col-lg-3 col-md-2 col-sm-0"] []
      , div [class "col-lg-6 col-md-8 col-sm-12"]
          [ p [] []
          , div [ class "panel panel-info" ]
            [ div [ class "panel-heading" ]
              [h4 []  ((text "Mobile SIP Callback by sip.uno     ")::(pendingRequestsBadge model))]
            , div [ class "panel-body" ]
              (dispatchView model)
            ]
          ]
      , div [class "col-lg-3 col-md-2 col-sm-0"] []
      ]
    ]
    {-
    (
      div [class "page-header"] [h2 [] [span [class "label label-info center-block"] ((text "     sip.uno     ")::(pendingRequestsBadge model))]] ::
        [div [class "row"]
          [ div [class "col-lg-3 col-md-2 col-sm-0"] []
          , div [class "col-lg-6 col-md-8 col-sm-12"] (dispatchView model)
          , div [class "col-lg-3 col-md-2 col-sm-0"] []
          ]
        ]
    )
    -}

dispatchView : Model -> List (Html Msg)
dispatchView model =
  case model.sessionState of
    SessionEstablished _ -> case model.uiState of
      EditSIPAccount sipAccountUIState -> viewEditSIPAccount sipAccountUIState.accountConfig  sipAccountUIState.canCancel model.userData
      EditDialParams editParamsUIState -> viewEditDialParams editParamsUIState.dialConfig editParamsUIState.canCancel model.userData
      MainScreen -> viewMainScreen model
    _ -> viewConnectingToServer model
