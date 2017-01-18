module ModelData exposing(..)

import FlowAppData exposing (..)
import BasicCallData exposing (..)
import UserData exposing (..)
import Date exposing (..)

type alias SessionID = String

type SessionState
  = WaitingForServerReq
  | WaitForSession (Maybe SessionID) -- has id of prev session or Nothing if no previos session
  | SessionEstablished SessionID
  | UpgradeIsRequired

type UIState
  = EditSIPAccount {canCancel : Bool, accountConfig : AccountConfig}
  | EditDialParams {canCancel : Bool, dialConfig : DialConfig}
  | MainScreen

initUIState : UIState
initUIState = MainScreen

type alias MainScreenState =
  { typingDestID : String
  }
initMailScreenState : MainScreenState
initMailScreenState = MainScreenState ""

type alias Model =
  { uiState : UIState
  , sessionState : SessionState
  , mainState : MainScreenState
  , pendingRequests : Int
  , basicCallRS : BasicCallRS
  , lastMessage : String
  , userData : UserData
  }

type alias StoredData =
  { userData : UserData
  , ext : Maybe String
}

newModel : UserData -> Model
newModel userData = Model initUIState WaitingForServerReq initMailScreenState 0 NoCall "" userData

emptyModel : Model
emptyModel = newModel emptyUserData

storeDataFromModel : Model ->StoredData
storeDataFromModel model =
    StoredData model.userData Nothing

modelFromStoreData : StoredData -> Model
modelFromStoreData storedData= newModel storedData.userData

sessionEstablished : Model -> Bool
sessionEstablished model = case model.sessionState of
  SessionEstablished _ -> True
  _ -> False

setUIState : UIState -> Model -> Model
setUIState newUIState model = {model | uiState = newUIState}

emptySipAccount : AccountConfig
emptySipAccount =
                { sipUserName = ""
                , sipPassword = ""
                , sipDomain = ""
                , sipDirectMedia = False
                , sipEnableRPID = True
                , sipExtraConfig = Nothing
                }

emptyDialConfig : DialConfig
emptyDialConfig =
                { phoneNum = ""
                , callerID = Nothing
                , callerName = Nothing
                , extraDialConfig = Nothing
                }

setInitUIMode : Model -> Model
setInitUIMode model = model -- setUIState MainScreen model

isSipAccountConfigured : Model -> Bool
isSipAccountConfigured model =  not (List.isEmpty model.userData.sipAccounts)

isDialConfigured : Model -> Bool
isDialConfigured model =  not (model.userData.dialConfig==Nothing)

updateUserData : (UserData -> UserData) -> Model -> Model
updateUserData f model = {model | userData = f model.userData}

storeSipAccountsHistory : AccountConfig -> Model -> Model
storeSipAccountsHistory sipAccount model =
  model |> updateUserData (addSipDomain sipAccount.sipDomain) |> updateUserData (addUserName sipAccount.sipUserName)

storeDialConfigHistory : DialConfig -> Model -> Model
storeDialConfigHistory dialConfig model =
  model |> updateUserData (addUserPhone (PhoneEntry dialConfig.phoneNum Nothing Nothing)) |> updateUserData (addCallerID dialConfig.callerID dialConfig.callerName)

storeCallHistory : DestParty -> Date -> Model -> Model
storeCallHistory destParty callDate model = model |> updateUserData (addHistoryEntry destParty callDate)

clearSIPAccount : Model -> Model
clearSIPAccount = updateUserData (\ud -> {ud | sipAccounts = []})

clearDialConfig : Model -> Model
clearDialConfig  = updateUserData (\ud -> {ud | dialConfig = Nothing})

{-
  if List.isEmpty model.userData.sipAccounts
    then setUIState (EditSIPAccount {canCancel=False, accountConfig=emptySipAccount}) model
    else if (model.userData.dialConfig==Nothing)
      then setUIState (EditDialParams {canCancel=False, dialConfig=emptyDialConfig}) model
      else if model.uiState == JustStarted
        then setUIState MainScreen model
        else model
-}

setSessionEstablished : SessionID -> Model -> Model
setSessionEstablished sessionID model = {model | sessionState = SessionEstablished sessionID}

setWaitForSession : Model -> Model
setWaitForSession model = {model | sessionState = WaitForSession (getCurrentSessionID model.sessionState)}

setUpgradeRequired : Model -> Model
setUpgradeRequired model = {model | sessionState = UpgradeIsRequired}

resetPendingRequests : Model -> Model
resetPendingRequests model = {model | pendingRequests =0 }

updateWithConfReceived : Model -> Model
updateWithConfReceived model = {model | pendingRequests =  if (model.pendingRequests > 0) then (model.pendingRequests-1) else 0 }

updateBCState : BasicCallRS -> Model -> Model
updateBCState basicStateRS model = {model | basicCallRS = basicStateRS}

getCurrentSessionID : SessionState -> Maybe SessionID
getCurrentSessionID sessionState = case sessionState of
  SessionEstablished sessID -> Just sessID
  _  -> Nothing


updateEditSipAccount : (AccountConfig->AccountConfig) -> Model -> Model
updateEditSipAccount f model = case model.uiState of
  EditSIPAccount editParams -> {model | uiState= EditSIPAccount {editParams | accountConfig = f editParams.accountConfig}}
  _ -> model

setSipAccount : AccountConfig -> Model -> Model
setSipAccount accConfig model =
  let usrData = model.userData
  in {model | userData = {usrData | sipAccounts = [accConfig]}}

updateEditDialConfig : (DialConfig -> DialConfig) -> Model -> Model
updateEditDialConfig f model = case model.uiState of
  EditDialParams editParams -> {model | uiState= EditDialParams {editParams | dialConfig = f editParams.dialConfig}}
  _ -> model

updateEditMainScreen : (MainScreenState -> MainScreenState) -> Model -> Model
updateEditMainScreen f model = {model | mainState = f model.mainState}

setDialConfig : DialConfig -> Model -> Model
setDialConfig dialCfg model =
  let usrData = model.userData
  in {model | userData = {usrData | dialConfig = (Just dialCfg)}}

startSIPAccountEditting : Model -> Model
startSIPAccountEditting model =
  let (flCancel, sipAccount) =case model.userData.sipAccounts of
      [] -> (True, emptySipAccount)
      sipAcc :: _ -> (True, sipAcc)
  in setUIState (EditSIPAccount {canCancel = flCancel, accountConfig = sipAccount}) model

startDialConfigEditting : Model -> Model
startDialConfigEditting model =
  let (flCancel, dialCfg) =case model.userData.dialConfig of
      Nothing -> (True, emptyDialConfig)
      Just dialConf -> (True, dialConf)
  in setUIState (EditDialParams {canCancel = flCancel, dialConfig = dialCfg}) model
