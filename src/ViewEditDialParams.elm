module ViewEditDialParams exposing (viewEditDialParams)

import FlowAppData exposing (..)
import UserData exposing (..)
import ViewComps exposing (..)
import Messages exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (..)

updatePhoneNum : String -> DialConfig -> DialConfig
updatePhoneNum newVal dialConfig =
    {dialConfig | phoneNum=String.trim newVal}

updateCallerID : String -> DialConfig -> DialConfig
updateCallerID newStr dialConfig =
    let newVal = String.trim newStr
    in {dialConfig | callerID= if (String.length newVal) > 0 then (Just newVal) else Nothing}

isDialConfigured : DialConfig -> Bool
isDialConfigured dialConfig =
    not (String.isEmpty dialConfig.phoneNum)

editMsg : (String -> DialConfig -> DialConfig) -> String -> Msg
editMsg f txt = EditConfig (Editting (f txt))

viewEditDialParams : DialConfig -> Bool -> UserData -> List (Html Msg)
viewEditDialParams dialConfig flCanCancel userData =
    [ h2 [] [span [class "label label-primary center-block"] [text "Dialing parameters"]]
    , div [class "row"] [p [] []]
    , table [class "table table-striped"] [tbody []
          [tr []
            [ td [] [ text "Phone number:"]
            , td [] [ input [list "phones", onInput (editMsg updatePhoneNum), value dialConfig.phoneNum, placeholder "Your Phone Number", type_ "tel"] []
                    , datalist [id "phones"] (optionsListFromStrings (List.map (.number) userData.phones))
                    ]
            ]
          , tr [] [td [colspan 2] [p [] []]]
          , tr []
            [ td [] [ text "Caller ID:"]
            , td [] [ input [list "callerIDs", onInput (editMsg updateCallerID), value (Maybe.withDefault "" dialConfig.callerID), placeholder "Your Caller ID (optional)", type_ "tel"] []
                    , datalist [id "callerIDs"] (optionsListFromStrings (List.map (.number) userData.callerIDs))
                    ]
            ]
          , tr [] [td [colspan 2] [p [] []]]
          , tr []
            [ td [colspan 2]
              [div [class "pull-right"] [div [class "btn-toolbar"]
                [button [onClick (EditConfig (Save dialConfig)), class (if (isDialConfigured dialConfig) then "btn btn-success" else ""),  hidden (not (isDialConfigured dialConfig))] [text "Save"]
                ,button [onClick (EditConfig Cancel), class (if flCanCancel then "btn btn-warning" else ""), hidden (not flCanCancel)]  [text "Cancel"]
                ]
              ]]
            ]
          ]
      ]
   ]
