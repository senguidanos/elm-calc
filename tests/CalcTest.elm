module CalcTest exposing (..)

import Calc exposing (..)
import Test exposing (..)
import Expect exposing (..)


tests : Test
tests =
    describe "Perform"
        [ describe "collapse"
            [ test "returns operands without operator" <|
                \_ ->
                    collapse [] [ 1 ]
                        |> Expect.equal ([ 1 ])
            , test "returns empty list without operands" <|
                \_ ->
                    collapse [] []
                        |> Expect.equal ([])
            , test "applies operator to top two inputs" <|
                \_ ->
                    collapse [ Plus ] [ 1, 1 ]
                        |> Expect.equal ([ 2 ])
            , test "applies multiple operators" <|
                \_ ->
                    collapse [ Plus, Times, Plus ] [ 2, 2, 4, 5 ]
                        |> Expect.equal ([ 21 ])
            , test "limits operations to number of operand pairs" <|
                \_ ->
                    collapse [ Plus, Times ] [ 2, 2 ]
                        |> Expect.equal ([ 4 ])
            , only <|
                test "returns error code if the result overflows 10 digits" <|
                    \_ ->
                        collapse [ Times ] [ 100000000000000.0, 10 ]
                            |> Expect.equal ([ -99999999999 ])
            ]
        , describe "calculate"
            [ test "returns operands without operator" <|
                \_ ->
                    calculate Plus [] [ 1 ]
                        |> Expect.equal ( [], [ 1 ] )
            , test "returns empty list without operands" <|
                \_ ->
                    calculate Minus [] []
                        |> Expect.equal ( [], [] )
            , test "applies operator to top two inputs" <|
                \_ ->
                    calculate Times [ Plus ] [ 1, 1 ]
                        |> Expect.equal ( [], [ 2 ] )
            , test "applies multiple operators" <|
                \_ ->
                    calculate DividedBy [ Plus, Times, Plus ] [ 2, 2, 4, 5 ]
                        |> Expect.equal ( [], [ 21 ] )
            , test "limits operations to number of operand pairs" <|
                \_ ->
                    calculate Times [ Plus, Times ] [ 2, 2 ]
                        |> Expect.equal ( [], [ 4 ] )
            ]
        ]
