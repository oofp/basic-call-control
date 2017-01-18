module ViewEditSIPAccount exposing (viewEditSIPAccount)

import FlowAppData exposing (..)
import UserData exposing (..)
import ViewComps exposing (..)
import Messages exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (..)

updateUserName : String -> AccountConfig -> AccountConfig
updateUserName newVal accountConfig =
    {accountConfig | sipUserName=String.trim newVal}

updatePassword : String -> AccountConfig -> AccountConfig
updatePassword newVal accountConfig =
    {accountConfig | sipPassword=String.trim newVal}

updateDomain : String -> AccountConfig -> AccountConfig
updateDomain newVal accountConfig =
    {accountConfig | sipDomain=String.trim newVal}

toggleDirectMedia : AccountConfig -> AccountConfig
toggleDirectMedia accountConfig =
    {accountConfig | sipDirectMedia= not accountConfig.sipDirectMedia}

toggleRPID : AccountConfig -> AccountConfig
toggleRPID accountConfig =
    {accountConfig | sipEnableRPID = not accountConfig.sipEnableRPID}

isAccountConfigured : AccountConfig -> Bool
isAccountConfigured accountConfig =
    not ((String.isEmpty accountConfig.sipUserName) || (String.isEmpty accountConfig.sipPassword) || (String.isEmpty accountConfig.sipDomain))


editMsg : (String -> AccountConfig -> AccountConfig) -> String -> Msg
editMsg f txt = EditSipAccount (Editting (f txt))

checkbox : Bool -> String -> (AccountConfig -> AccountConfig) -> Html Msg
checkbox checkedVal name toggleFunc =
  let checkedAttr = [checked checkedVal]
  in  div []
        [
         input ([ type_ "checkbox", onClick (EditSipAccount (Editting toggleFunc)) ]++ checkedAttr)  []
        , text ("    " ++ name)
        ]
  --label
  --  [ style [("padding", "20px")]
  --  ]

viewEditSIPAccount : AccountConfig -> Bool -> UserData -> List (Html Msg)
viewEditSIPAccount accountConfig flCanCancel userData =
    [ h2 [] [span [class "label label-primary center-block"] [text "SIP Account"]]
    , div [class "row"] [p [] []]
    , table [class "table table-striped"] [tbody []
          [tr []
            [ td [] [ text "Domain:"]
            , td [] [ input [list "domains", onInput (editMsg updateDomain), value accountConfig.sipDomain, placeholder "Your SIP domain"] []
                    , datalist [id "domains"] (optionsListFromStrings userData.sipDomains)
                    ]
            ]
          , tr [] [td [colspan 2] [p [] []]]
          , tr []
            [ td [] [text "User name:"]
            , td [] [ input [list "userNames", onInput (editMsg updateUserName), value accountConfig.sipUserName, placeholder "Your SIP user name"] []
                    , datalist [id "userNames"] (optionsListFromStrings userData.userNames)
                    ]
            ]
          , tr [] [td [colspan 2] [p [] []]]
          , tr []
            [ td [] [text "Password:"]
            , td [] [input [onInput (editMsg updatePassword), value accountConfig.sipPassword, placeholder "Your SIP password", type_ "password"] []]
            ]
          , tr [] [td [colspan 2] [p [] []]]
          , tr []
            [ td [] []
            , td [] [div []
                      [ checkbox accountConfig.sipDirectMedia "Direct media" toggleDirectMedia
                      , checkbox accountConfig.sipEnableRPID  "Enabled Remote-Party-ID" toggleRPID
                      ]
                    ]
            ]
          , tr [] [td [colspan 2] [p [] []]]
          , tr []
            [ td [colspan 2]
              [div [class "pull-right"] [div [class "btn-toolbar"]
                [button [onClick (EditSipAccount (Save accountConfig)), class (if (isAccountConfigured accountConfig) then "btn btn-success" else ""),  hidden (not (isAccountConfigured accountConfig))] [text "Save"]
                ,button [onClick (EditSipAccount Cancel), class (if flCanCancel then "btn btn-warning" else ""), hidden (not flCanCancel)]  [text "Cancel"]
                ]
              ]]
            ]
          ]
      ]
   ]
