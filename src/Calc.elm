module Calc exposing (..)


type Operator
    = Plus
    | Minus
    | Times
    | DividedBy


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


calculate : Operator -> List Operator -> List Float -> ( List Operator, List Float )
calculate op operators operands =
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
                        calculate prevOp remainingOps <| collapse [ prevOp ] operands
                    else
                        ( op :: operators, operands )


collapse : List Operator -> List Float -> List Float
collapse ops operands =
    case operands of
        [] ->
            []

        [ lhs ] ->
            [ lhs ]

        rhs :: lhs :: prevOperands ->
            case ops of
                [] ->
                    operands

                op :: prevOps ->
                    case op of
                        Plus ->
                            collapse prevOps <| (lhs + rhs) :: prevOperands

                        Minus ->
                            collapse prevOps <| (lhs - rhs) :: prevOperands

                        Times ->
                            collapse prevOps <| (lhs * rhs) :: prevOperands

                        DividedBy ->
                            collapse prevOps <| (lhs / rhs) :: prevOperands
