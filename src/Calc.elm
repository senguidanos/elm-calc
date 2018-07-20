module Calc exposing (..)

import Perform exposing (..)
import Operator exposing (..)
import Debug exposing (log)


precedence : Operator -> Int
precedence op =
    case op of
        Plus ->
            0

        Minus ->
            0

        Times ->
            1

        DividedBy ->
            1


process : Operator -> List Operator -> List Float -> ( List Operator, List Float )
process op operators operands =
    case operators of
        [] ->
            ( op :: operators, operands )

        prevOp :: remainingOps ->
            case op of
                Plus ->
                    ( [ op ], collapse operators operands )

                Minus ->
                    ( [ op ], collapse operators operands )

                _ ->
                    if precedence op <= precedence prevOp then
                        ( [ op ], [] )
                    else
                        ( [ op ], [] )
