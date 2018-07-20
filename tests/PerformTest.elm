module PerformTest exposing (..)

import Operator exposing (Operator(Plus, Minus, Times, DividedBy))
import Perform exposing (..)
import Test exposing (..)
import Expect exposing (..)


tests : Test
tests =
    describe "Perform"
        [ describe "collapse"
            [ test "returns operands without operator" <|
                \_ ->
                    collapse [] [ 1 ]
                        |> Expect.equal (Ok [ 1 ])
            , test "returns empty list without operands" <|
                \_ ->
                    collapse [] []
                        |> Expect.equal (Ok [])
            , test "applies operator to top two inputs" <|
                \_ ->
                    collapse [ Plus ] [ 1, 1 ]
                        |> Expect.equal (Ok [ 2 ])
            , test "applies multiple operators" <|
                \_ ->
                    collapse [ Plus, Times, Plus ] [ 2, 2, 4, 5 ]
                        |> Expect.equal (Ok [ 21 ])
            , test "limits operations to number of operand pairs" <|
                \_ ->
                    collapse [ Plus, Times ] [ 2, 2 ]
                        |> Expect.equal (Ok [ 4 ])
            , only <|
                test "returns an error if the result overflows 10 digits" <|
                    \_ ->
                        collapse [ Times ] [ 100000000000000.0, 10 ]
                            |> Expect.equal (Err "OVERFLOW")
            ]
        ]
