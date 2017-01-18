module ViewConnecting exposing (..)

import ModelData exposing (..)
import Messages exposing (..)
-- import Html.Attributes exposing (..)
import Html exposing (..)
import ViewComps exposing (..)

viewConnectingToServer : Model -> List (Html Msg)
viewConnectingToServer model =
  let txt = case model.sessionState of
    SessionEstablished _ -> "Connection established"
    WaitingForServerReq -> "Awaiting for connectivity"
    WaitForSession _ -> "Connecting ..."
    UpgradeIsRequired -> "Sorry, but you need to upgrade the app to continue using it"
  in
    [rowSubTitle txt "label-default"]
