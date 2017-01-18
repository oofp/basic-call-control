module ViewMainScreen exposing (viewMainScreen)

import ModelData exposing (..)
import UserData exposing (..)
import BasicCallData exposing (..)
import Messages exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (..)
import Date exposing (..)
import MiscUtils exposing (..)
import ViewComps exposing (..)

viewMainScreen : Model -> List (Html Msg)
viewMainScreen model =
    case model.basicCallRS of
        NoCall ->
            viewNoCall model

        Initiated destParty ->
            viewCallInProgress model destParty "Dialing your phone" False

        RingingCaller destParty ->
            viewCallInProgress model destParty "Ringing your phone" False

        InitiatedCallee destParty ->
            viewCallInProgress model destParty "Dialing destination" True

        RingingCallee destParty ->
            viewCallInProgress model destParty "Destination phone is ringing" True

        CallerIdle ->
            viewOffhook model

        Connected destParty ->
            viewConnected model destParty


sipAccountStr : Model -> String
sipAccountStr model =
    case model.userData.sipAccounts of
        [] ->
            "No sip account configured. You need to configure it to start calling"

        sipAcc :: _ ->
            "SIP Account: " ++ sipAcc.sipUserName ++ "@" ++ sipAcc.sipDomain


dialConfigStr : Model -> String
dialConfigStr model =
    case model.userData.dialConfig of
        Nothing ->
            "No dialing parameters configured. You need to configure it to start calling"

        Just params ->
            "Your phone: " ++ params.phoneNum ++ (Maybe.withDefault "" (Maybe.map (\cID -> "; CallerID: " ++ cID) params.callerID))


editDestMsg : String -> Msg
editDestMsg str =
    let
        func mainScreen =
            { mainScreen | typingDestID = str }
    in
        EditMainScreen func

recentCallsView : Model -> (DestParty -> Date -> Msg) -> List (Html Msg)
recentCallsView model dialMsgFunc = case model.userData.history of
    [] -> []
    _  -> recentCallsViewNotEmpty model dialMsgFunc

recentCallsViewNotEmpty : Model -> (DestParty -> Date -> Msg) -> List (Html Msg)
recentCallsViewNotEmpty model dialMsgFunc =
    -- [ div [class "page-header"] [h3 [] [span [class "label label-info center-block"] [text "Recent Calls"]]]] ++
    [ div [class "row"] [h3 [] [] ]
    , div [ class "panel panel-info" ]
        [ div [ class "panel-heading" ]
            [ text "Recent Calls" ]
        , div [ class "panel-body" ]
            (
              (List.concatMap (rowForHistoryEntry dialMsgFunc) model.userData.history) ++
              [ rowButtonMd "Clear Recent Calls"  "btn-danger"  ClearAllHistoryMsg]
            )
        ]
    ]



historyDest : HistoryEntry -> DestParty
historyDest he = DestParty he.number he.name

destTitle : DestParty -> String
destTitle destParty = case destParty.destName of
    Just dstName -> dstName ++ " ["++ destParty.destNum++"]"
    Nothing -> destParty.destNum

historyDateStr : HistoryEntry -> String
historyDateStr he =
    let callDate = Date.fromTime he.callDateTime
    in String.join " "
                  [ toString (dayOfWeek callDate)++","
                  , toString (month callDate)
                  , String.padLeft 2 '0' (toString (day callDate))++","
                  , (String.padLeft 2 '0' (toString (hour callDate))) ++ ":"++ (String.padLeft 2 '0' (toString (minute callDate)))
                  ]

rowForHistoryEntry : (DestParty -> Date -> Msg) -> HistoryEntry -> List (Html Msg)
rowForHistoryEntry dialMsgFunc he =
    let dest = historyDest he
    in
        [ div [ class "panel panel-primary" ]
            [ --div [ class "panel-heading" ]
              --  [ text (destTitle dest)
              --  ]
              div [ class "panel-body" ]
                [ text ((destTitle dest) ++"; "++(historyDateStr he))
                , div [ class "btn-toolbar pull-right" ]
                    [ button [ onClick (MsgWithDate (dialMsgFunc dest)), class "btn btn-success btn-sm" ]
                        [ text "Dial"]
                    , button
                        [ onClick (ClearHistoryEntryMsg he)
                        , class "btn btn-warning btn-sm"
                        ]
                        [ text "Clear" ]
                    ]
                ]
            ]
        ]

dialingView : Model -> (DestParty -> Date -> Msg) -> List (Html Msg)
dialingView model dialMsgFunc =
    [ div [ class "panel panel-primary" ]
        [ div [ class "panel-heading" ]
            [ text "Destination:"]
        , div [ class "panel-body" ]
            [ input [ list "hist", onInput editDestMsg, value model.mainState.typingDestID, class "form-control", id "destNum", type_ "tel" ]
                []
            , datalist [id "hist"] (optionsListFromStrings (List.map (.number) model.userData.history))
        , div [ class "row"] [p [] []]
        , div [ class "row"]
            [ div [ class "text-center  col-md-4" ] []
            , div [ class "text-center  col-md-4" ]
                (if (isNothing (extractDigits model.mainState.typingDestID))
                    then
                        []
                    else
                        [ button [ onClick (MsgWithDate (dialMsgFunc (DestParty model.mainState.typingDestID Nothing))), class "btn btn-success btn-lg btn-block " ]
                            [ text "Dial" ]
                        ]
                )
            , div [ class "text-center  col-md-4" ] []
            ]
          ]
        ]
    , div [ class "row"]
          [ div [ class "text-center--  col-md-4" ] []
          , div [ class "text-center -- col-md-4" ]
              [ --button [ onClick PickContact, class "btn btn-info btn-lg btn-block " ]
                --  [ text "Contact..." ]
              ]
          , div [ class "text-center  col-md-4" ] []
          ]
    ]
    ++ recentCallsView model dialMsgFunc

canDial : Model -> Bool
canDial model =
    isDialConfigured model && isSipAccountConfigured model


viewNoCall : Model -> List (Html Msg)
viewNoCall model =
    [ div [ class "panel panel-primary" ]
        [ div [ class "panel-heading" ]
            [ text "SIP Account"]
        , div [ class "panel-body" ]
            [ text (sipAccountStr model)
            , div [ class "btn-toolbar pull-right" ]
                [ button [ onClick (EditSipAccount StartEditting), class "btn btn-success btn-sm" ]
                    [ text
                        (if isSipAccountConfigured model then
                            "Edit"
                            else
                            "Configure"
                        )
                    ]
                , button
                    [ onClick ClearSIPAccountMsg
                    , class
                        (if isSipAccountConfigured model then
                            "btn btn-warning btn-sm"
                            else
                            ""
                        )
                    , hidden (not (isSipAccountConfigured model))
                    ]
                    [ text "Clear" ]
                ]
            ]
        ]
    ,  div [ class "panel panel-primary" ]
         [ div [ class "panel-heading" ]
            [ text "Dial parameters" ]
        , div [ class "panel-body" ]
            [ text (dialConfigStr model)
            , div [ class "btn-toolbar pull-right" ]
                [ button [ onClick (EditConfig StartEditting), class "btn btn-success btn-sm" ]
                    [ text
                        (if isDialConfigured model then
                            "Edit"
                            else
                            "Configure"
                        )
                    ]
                , button
                    [ onClick ClearDialConfigMsg
                    , class
                        (if isDialConfigured model then
                            "btn btn-warning btn-sm"
                            else
                            ""
                        )
                    , hidden (not (isDialConfigured model))
                    ]
                    [ text "Clear" ]
                ]
            ]
        ]
    ] ++ (if canDial model then
                        dialingView model MakeCallMsg
                    else
                        []
        )


destPartyStr : DestParty -> String
destPartyStr destParty =
    case destParty.destName of
        Nothing ->
            destParty.destNum

        Just name ->
            name ++ "[" ++ destParty.destNum ++ "]"

viewCallInProgress : Model -> DestParty -> String -> Bool -> List (Html Msg)
viewCallInProgress model destParty labelStr phoneConnected =
  [ div [ class "panel panel-primary" ]
     [ div [ class "panel-heading" ]
        [ text "Dialing" ]
     , div [ class "panel-body" ] (
        start_l
          |> al ( div [ class "row" ]
                   [ -- rowSubTitle "Dialing" "label-primary"
                     div [ class "row" ]
                          [ div [ class "text-center" ]
                              [ h3 []
                                  [ p [ class "lead" ]
                                          [ text (destPartyStr destParty)
                                          , br []
                                              []
                                          , text labelStr
                                          ]
                                  ]
                              ]
                          ]
                  ]
              )
          |> al ( div [ class "row"]
                  [ div [ class "text-center  col-md-4" ] []
                  , div [ class "text-center  col-md-4" ]
                      [button
                          [ onClick
                              (if phoneConnected
                                  then
                                      DropCallMsg
                                  else
                                      HangupMsg
                              ) , class "btn btn-warning btn-lg btn-block"
                          ][ text "Cancel" ]
                      ]
                  , div [ class "text-center  col-md-4" ] []
                  ]
              )
           |> al ( div [ class "row"] [p [] []] )
           |> al_if phoneConnected
                  ( div [ class "row"]
                      [ div [ class "text-center  col-md-4" ] []
                      , div [ class "text-center  col-md-4" ]
                          [ button [ onClick HangupMsg , class "btn btn-danger btn-lg btn-block"]
                              [ text "Hangup"
                              ]
                          ]
                      , div [ class "text-center  col-md-4" ] []
                      ]
                  )
         )
      ]
    ]

viewOffhook : Model -> List (Html Msg)
viewOffhook model =
    [ div [ class "panel panel-primary" ]
        [ div [ class "panel-heading" ]
            [ text "Your phone is connected"
            ]
        , div [ class "panel-body" ]
            [rowButtonMd "Hangup" "btn-danger" HangupMsg]
        ]
    ] ++ (dialingView model DialMsg)

viewConnected : Model -> DestParty -> List (Html Msg)
viewConnected model destParty =
    [ div [ class "panel panel-success" ]
        [ div [ class "panel-heading" ]
            [ text "Call Connected"
            ]
        , div [ class "panel-body" ]
            [ div [ class "row" ]
              [ div [ class "text-center" ]
                  [ p [ class "lead" ]
                      [ text (destPartyStr destParty)
                      ]
                  ]
              ]
          , (rowButton "Drop Call"  "btn-warning" DropCallMsg)
          , div [ class "row"] [p [] []]
          , (rowButton "Hangup"  "btn-danger" HangupMsg)
          ]
       ]
    ]
