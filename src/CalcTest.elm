module CalcTest exposing (..)

import Calc exposing (..)
import Perform exposing (Operator)
import Test exposing (..)
import Expect exposing (..)


tests : Test
tests =
    describe "Process"
        [ describe "process"
            [ test "returns operands without operator" <|
                \_ ->
                    process Plus [] [ 1 ]
                        |> Expect.equal ( [], [ 1 ] )
            , test "returns empty list without operands" <|
                \_ ->
                    process Minus [] []
                        |> Expect.equal ( [], [] )
            , test "applies operator to top two inputs" <|
                \_ ->
                    process Times [ Plus ] [ 1, 1 ]
                        |> Expect.equal ( [], [ 2 ] )
            , test "applies multiple operators" <|
                \_ ->
                    process DividedBy [ Plus, Times, Plus ] [ 2, 2, 4, 5 ]
                        |> Expect.equal ( [], [ 21 ] )
            , test "limits operations to number of operand pairs" <|
                \_ ->
                    process Times [ Plus, Times ] [ 2, 2 ]
                        |> Expect.equal ( [], [ 4 ] )
            ]
        ]
