module Perform exposing (..)

import Operator exposing (..)
import Debug exposing (..)


collapse : List Operator -> List Float -> List Float
collapse ops operands =
    case ops of
        [] ->
            operands

        op :: prevOps ->
            case operands of
                [] ->
                    []

                [ lhs ] ->
                    if ((String.length <| toString lhs) <= 10) then
                        [ lhs ]
                    else
                        [ -99999999999.9 ]

                rhs :: lhs :: prevOperands ->
                    case op of
                        Plus ->
                            collapse prevOps <| (lhs + rhs) :: prevOperands

                        Minus ->
                            collapse prevOps <| (lhs - rhs) :: prevOperands

                        Times ->
                            collapse prevOps <| (lhs * rhs) :: prevOperands

                        DividedBy ->
                            collapse prevOps <| (lhs / rhs) :: prevOperands
