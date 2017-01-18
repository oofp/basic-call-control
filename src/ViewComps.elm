module ViewComps exposing (..)

import Messages exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html exposing (..)

rowButton : String -> String -> Msg -> Html Msg
rowButton txt btnStyle cmd =
    div [ class "row"]
        [ div [ class "text-center  col-md-4" ] []
        , div [ class "text-center  col-md-4" ]
            [ button [ onClick cmd, class ("btn "++ btnStyle ++ " btn-lg btn-block ") ]
                [ text txt ]
            ]
        , div [ class "text-center  col-md-4" ] []
        ]
rowButtonMd : String -> String -> Msg -> Html Msg
rowButtonMd txt btnStyle cmd =
    div [ class "row"]
        [ div [ class "text-center  col-md-4" ] []
        , div [ class "text-center  col-md-4" ]
            [ button [ onClick cmd, class ("btn "++ btnStyle ++ " btn-md btn-block ") ]
                [ text txt ]
            ]
        , div [ class "text-center  col-md-4" ] []
        ]


rowSubTitle : String-> String -> Html Msg
rowSubTitle txt labelStyle =
    div [ class "row"]
        [ div [class "col-lg-2 col-md-2 col-sm-0"] []
        , div [class "col-lg-8 col-md-8 col-sm-12"]
            [div [class "page-header"] [span [class ("label "++ labelStyle ++ " center-block")] [text txt]]]

        , div [class "col-lg-2 col-md-2 col-sm-0"] []
        ]

optionsListFromStrings : List String -> List (Html Msg)
optionsListFromStrings strs = List.map (\str->option [] [ text str ]) strs
