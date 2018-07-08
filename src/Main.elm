module Main exposing (..)

import Html exposing (..)

import Html.Attributes exposing (..)
import Calculations exposing (add, subtract, multiply, divide)

type alias Calculator = 
  { runningTotal : Float
  , currentInput : Float
  , currentOperation : Float -> Float -> Float
  , previousOperation : Float -> Float -> Float
  }

type Input = 
  Plus
  | Minus
  | Times
  | DividedBy

noOp : Float -> Float -> Float
noOp x y =
  x


calculator : Calculator
calculator = 
  { runningTotal = 0
  , currentInput = 0
  , currentOperation = noOp
  , previousOperation = noOp
  }

view : Calculator -> Html a
view calculator =
  div []
    [ h2 [] [ text "Calculator " ]
    , div []
      [text (toString Calculator)]
    , input [ type_ "number", placeholder "Hello World" ] []
    ]

update : Input -> Calculator -> Calculator
update input calc = 
  case input of
    Plus ->
      { calc | 
          currentOperation = add,
          runningTotal = calc.previousOperation calc.runningTotal calc.currentInput,
          currentInput = calc.previousOperation calc.runningTotal calc.currentInput,
          previousOperation = noOp
      }
    Minus ->
      { calc | 
        currentOperation = subtract,
        runningTotal = calc.previousOperation calc.runningTotal calc.currentInput,
        currentInput = calc.previousOperation calc.runningTotal calc.currentInput,
        previousOperation = noOp
      }
    Times ->
      { calc | 
        currentOperation = multiply,
        runningTotal = calc.previousOperation calc.runningTotal calc.currentInput,
        currentInput = calc.previousOperation calc.runningTotal calc.currentInput
      }
    DividedBy ->
      { calc | currentOperation = divide }


main : Html a
main = 
  view calculator