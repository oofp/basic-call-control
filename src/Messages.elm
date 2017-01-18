module Messages exposing(..)

import SessionHandshake exposing (..)
import BasicCallData exposing (..)
import FlowAppData exposing (..)
import ModelData exposing (..)
import UserData exposing (..)
import Date exposing (..)

type EditScreenMsg a
     = StartEditting
     | Editting (a -> a)
     | Cancel
     | Save a

type alias ContactDetails =
    { contactNum  : String
    , contactName : String
    }

type Msg =  EditSipAccount (EditScreenMsg AccountConfig)
         |  EditConfig (EditScreenMsg DialConfig)
         |  EditMainScreen (MainScreenState -> MainScreenState)
         |  ClearSIPAccountMsg
         |  ClearDialConfigMsg
         |  ClearHistoryEntryMsg HistoryEntry
         |  ClearAllHistoryMsg
         |  MsgWithDate (Date->Msg)
         |  MakeCallMsg DestParty Date
         |  DialMsg DestParty Date
         |  HangupMsg
         |  DropCallMsg
         |  SessionMsgReceived SessionHandshake
         |  ServerEventReceived Event
         |  SystemError String
         |  PickContact
         |  ContactPicked ContactDetails
