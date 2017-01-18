module UserData exposing(..)

import FlowAppData exposing (..)
import BasicCallData exposing (..)
import Date exposing (..)
import Time exposing (..)

type alias HistoryEntry =
  {  number : String
  ,  name : Maybe String
  ,  callDateTime : Time
  , ext : Maybe (List (String, String))
  }

sameHistoryEntry : HistoryEntry -> HistoryEntry -> Bool
sameHistoryEntry he1 he2 = he1.number == he2.number && he1.name == he2.name

type alias PhoneEntry =
  { number : String
  , title : Maybe String
  , ext : Maybe (List (String, String))
  }

type alias UserData =
  { history : List HistoryEntry
  , phones : List PhoneEntry
  , callerIDs : List PhoneEntry
  , sipDomains : List String
  , userNames : List String
  , sipAccounts : List AccountConfig
  , dialConfig : Maybe DialConfig
  , ext : Maybe (List (String, String))
  }

emptyUserData : UserData
emptyUserData = UserData [] [] [] [] [] [] Nothing Nothing

addPhoneEntry : PhoneEntry -> UserData -> UserData
addPhoneEntry phoneEntry userData = {userData | phones=userData.phones ++ [phoneEntry]}

addHistoryEntry : DestParty -> Date -> UserData -> UserData
addHistoryEntry destParty callDate userData =
  let
    historyEntry = HistoryEntry destParty.destNum destParty.destName  (Date.toTime callDate) Nothing
    newHistory= historyEntry :: (List.take 99 (List.filter (\he -> (not (sameHistoryEntry historyEntry he))) userData.history))
  in
    {userData | history = newHistory }

removeHistoryEntry : HistoryEntry -> UserData -> UserData
removeHistoryEntry historyEntry userData = {userData | history=List.take 32 (List.filter (\he -> he /= historyEntry) userData.history)}

clearHistory : UserData -> UserData
clearHistory userData = {userData | history = []}

addSipDomain : String -> UserData -> UserData
addSipDomain sipDomain userData = {userData | sipDomains=sipDomain :: List.take 32 (List.filter (\sd-> sd/=sipDomain) userData.sipDomains)}

addUserName : String -> UserData -> UserData
addUserName userName userData = {userData | userNames =userName :: List.take 32 (List.filter (\un-> un/=userName) userData.userNames)}

addUserPhone : PhoneEntry -> UserData -> UserData
addUserPhone phoneEntry userData = {userData | phones =phoneEntry :: List.take 32 (List.filter (\pe-> pe.number/=phoneEntry.number) userData.phones)}

addCallerID : Maybe String -> Maybe String -> UserData -> UserData
addCallerID  callerIDMaybe callerNameMaybe userData =
  case callerIDMaybe of
    Nothing -> userData
    Just callerID -> addCallerID_ (PhoneEntry callerID callerNameMaybe Nothing) userData

addCallerID_ : PhoneEntry -> UserData -> UserData
addCallerID_  phoneEntry userData = {userData | callerIDs =phoneEntry :: List.take 32 (List.filter (\pe-> pe.number/=phoneEntry.number) userData.callerIDs)}
