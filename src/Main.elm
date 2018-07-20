module Main exposing (..)

import Debug exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import String exposing (length, contains, toFloat)
import Calc exposing (process)
import Operator exposing (..)
import Perform exposing (collapse)


type Input
    = BuildOperand String
    | AddOperator Operator
    | Decimal
    | Equals


inputToFloat : String -> Float
inputToFloat str =
    case String.toFloat str of
        Ok num ->
            num

        _ ->
            -- handle Error case!
            0


type alias Calculator =
    { operands : List Float
    , operators : List Operator
    , currentInput : String
    }


init : ( Calculator, Cmd Input )
init =
    ( { operands = []
      , operators = []
      , currentInput = "0"
      }
    , Cmd.none
    )


displayResult : String -> List Float -> String
displayResult currentInput memory =
    case currentInput of
        "" ->
            case memory of
                [] ->
                    "0"

                x :: ys ->
                    if isInfinite x then
                        "Undefined"
                    else if x == -99999999999.9 then
                        "Error"
                    else
                        toString x

        _ ->
            currentInput


view : Calculator -> Html Input
view calc =
    div [ container ]
        [ div [] [ text <| toString calc.operators ]
        , div [] [ text <| toString calc.operands ]
        , div [] [ text <| toString calc.currentInput ]
        , table [ style [ ( "border-collapse", "collapse" ) ] ]
            [ tr []
                [ td [ output, colspan 4 ] [ text <| displayResult calc.currentInput calc.operands ] ]
            , tr []
                [ td [ colspan 3, inputButton ] []
                , td [ operatorButton, onClick <| AddOperator DividedBy ] [ text "÷" ]
                ]
            , tr []
                [ td [ inputButton, onClick <| BuildOperand "7" ] [ text "7" ]
                , td [ inputButton, onClick <| BuildOperand "8" ] [ text "8" ]
                , td [ inputButton, onClick <| BuildOperand "9" ] [ text "9" ]
                , td [ operatorButton, onClick <| AddOperator Times ] [ text "x" ]
                ]
            , tr []
                [ td [ inputButton, onClick <| BuildOperand "4" ] [ text "4" ]
                , td [ inputButton, onClick <| BuildOperand "5" ] [ text "5" ]
                , td [ inputButton, onClick <| BuildOperand "6" ] [ text "6" ]
                , td [ operatorButton, onClick <| AddOperator Minus ] [ text "-" ]
                ]
            , tr []
                [ td [ inputButton, onClick <| BuildOperand "1" ] [ text "1" ]
                , td [ inputButton, onClick <| BuildOperand "2" ] [ text "2" ]
                , td [ inputButton, onClick <| BuildOperand "3" ] [ text "3" ]
                , td [ operatorButton, onClick <| AddOperator Plus ] [ text "+" ]
                ]
            , tr []
                [ td [ inputButton, colspan 2, onClick <| BuildOperand "0" ] [ text "0" ]
                , td [ inputButton, onClick <| Decimal ] [ text "." ]
                , td [ operatorButton, onClick <| Equals ] [ text "=" ]
                ]
            ]
        ]


update : Input -> Calculator -> ( Calculator, Cmd Input )
update input calc =
    case input of
        BuildOperand str ->
            ( { calc
                | currentInput =
                    case calc.currentInput of
                        "0" ->
                            str

                        other ->
                            calc.currentInput ++ str
              }
            , Cmd.none
            )

        Decimal ->
            ( { calc
                | currentInput =
                    if contains "." calc.currentInput then
                        calc.currentInput
                    else
                        calc.currentInput ++ "."
              }
            , Cmd.none
            )

        AddOperator op ->
            let
                operands =
                    (inputToFloat calc.currentInput) :: calc.operands

                ( operators, result ) =
                    process op calc.operators operands
            in
                ( { calc
                    | operands = result
                    , operators = operators
                    , currentInput = ""
                  }
                , Cmd.none
                )

        Equals ->
            let
                memoryStack =
                    collapse calc.operators <| (inputToFloat calc.currentInput) :: calc.operands
            in
                ( { calc
                    | operands = memoryStack
                    , operators = []
                    , currentInput = ""
                  }
                , Cmd.none
                )


main : Program Never Calculator Input
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Styles


container : Attribute a
container =
    style
        [ ( "margin-left", "100px" )
        , ( "font-family", "sans-serif" )
        ]


inputButton : Attribute a
inputButton =
    style
        [ ( "width", "50px" )
        , ( "text-indent", "20px" )
        , ( "border", "1px solid #242424" )
        , ( "background", "#DCDCDC" )
        , ( "height", "50px" )
        , ( "cursor", "default" )
        ]


operatorButton : Attribute a
operatorButton =
    style
        [ ( "background", "#F6872C" )
        , ( "width", "50px" )
        , ( "text-indent", "20px" )
        , ( "border", "1px solid #242424" )
        , ( "height", "50px" )
        , ( "cursor", "default" )
        ]


output : Attribute a
output =
    style
        [ ( "background", "#242424" )
        , ( "color", "white" )
        , ( "border", "1px solid #242424" )
        , ( "text-align", "right" )
        , ( "height", "50px" )
        , ( "padding-right", "20px" )
        , ( "cursor", "default" )
        ]
