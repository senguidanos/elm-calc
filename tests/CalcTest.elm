module CalcTest exposing (..)

import Calc exposing (..)
import Test exposing (..)
import Expect exposing (..)


tests : Test
tests =
    describe "Perform"
        [ describe "collapse"
            [ test "returns operands without any operator" <|
                \_ ->
                    collapse [] [ 1 ]
                        |> Expect.equal ([ 1 ])
            , test "returns empty list without any operands" <|
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
            , test "returns error code if the result overflows 10 digits" <|
                \_ ->
                    collapse [ Times ] [ 100000000000000.0, 10 ]
                        |> Expect.equal ([ -9999999999.9 ])
            ]
        , describe "calculate"
            [ test "ignores operator without any operands" <|
                \_ ->
                    calculate Minus [] []
                        |> Expect.equal ( [], [] )
            , test "applies previous operator to top two inputs" <|
                \_ ->
                    calculate Plus [ Plus ] [ 1, 1 ]
                        |> Expect.equal ( [ Plus ], [ 2 ] )
            , test "applies multiple previous operators" <|
                \_ ->
                    calculate Plus [ Times, Times, Plus ] [ 2, 2, 4, 5 ]
                        |> Expect.equal ( [ Plus ], [ 21 ] )
            , test "does not collapse stack when current op has higher precedence" <|
                \_ ->
                    calculate Times [ Plus ] [ 1, 1 ]
                        |> Expect.equal ( [ Times, Plus ], [ 1, 1 ] )
            , test "stops collapsing operator-operand groupings when prevOp has lower precedence" <|
                \_ ->
                    calculate Times [ Times, Plus ] [ 2, 3, 2 ]
                        |> Expect.equal ( [ Times, Plus ], [ 6, 2 ] )
            , test "handles a single existing operand" <|
                \_ ->
                    calculate Plus [] [ 1 ]
                        |> Expect.equal ( [ Plus ], [ 1 ] )
            ]
        ]
