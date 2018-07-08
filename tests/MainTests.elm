module MainTests exposing (..)

import Main exposing (..)
import Calculations exposing (..)
import Test exposing (..)
import Expect exposing (..)

calculator : Calculator
calculator =
  { runningTotal = 3
  , currentInput = 5
  , currentOperation = noOp
  , previousOperation = noOp
  }

calculatorPreviousAdd : Calculator
calculatorPreviousAdd =
  { calculator | previousOperation = add }

tests : Test
tests = 
  describe "Main"
    [ describe "update" 
      [ describe "Plus"
        [ test "returns Calculator with add currentOperation" <|
          \_ -> 
            calculator
              |> update Plus
              |> .currentOperation
              |> Expect.equal add
        , test "calculates runningTotal from previousOperation" <|
          \_ ->
            calculatorPreviousAdd
              |> update Plus
              |> .runningTotal
              |> Expect.equal 8
        , test "sets currentInput to runningTotal" <|
          \_ ->
            calculatorPreviousAdd
              |> update Plus
              |> .currentInput
              |> Expect.equal 8
        ]
      , describe "Minus"
        [ test "returns Calculator with subtract currentOperation" <|
          \_ ->
            calculator
              |> update Minus
              |> .currentOperation
              |> Expect.equal subtract
        , test "calculates runningTotal from previousOperation" <|
          \_ ->
            calculatorPreviousAdd
              |> update Minus
              |> .runningTotal
              |> Expect.equal 8
        , test "sets currentInput to runningTotal" <|
          \_ ->
            calculatorPreviousAdd
              |> update Minus
              |> .currentInput
              |> Expect.equal 8

        ]
      , describe "Times"
        [ test "returns Calculator with multiply currentOperation" <|
          \_ ->
            calculator
              |> update Times
              |> .currentOperation
              |> Expect.equal multiply
        -- , test "returns Calculator with runningTotal" <|
        --   \_ ->
        --     let
        --     calculator =
        --       { runningTotal = 12
        --       , currentInput = 5
        --       , currentOperation = Nothing
        --       , previousOperation = add
        --       }
        --       |> . 
        --       |> update Times
        --       |> 
        ]
      ]
    ]