module CalculationsTests exposing (..)

import Calculations exposing (add, subtract)
import Test exposing (..)
import Expect exposing (..)

tests : Test
tests = 
  describe "Calculations"
    [ describe "add"
      [ test "adds two floats" <|
        \_ ->
          2.1
            |> add 3
            |> Expect.equal 5.1
      , test "adds negative floats" <|
        \_ ->
          -2.1
            |> add 3
            |> Expect.within (Absolute 0.00000001) 0.9
      ]
    , describe "subtract"
      [ test "subtracts two floats" <|
        \_ ->
          2.1
            |> subtract 3
            |> Expect.within (Absolute 0.00000001) 0.9
      , test "subtracts negative floats" <|
        \_ ->
          -2.1
            |> subtract 3
            |> Expect.within (Absolute 0.00000001) 5.1
      ]
    ]
