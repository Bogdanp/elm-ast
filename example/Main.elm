module Main where

import Html exposing (..)
import Html.Events exposing (..)

import Ast
import Ast.Expression exposing (..)
import Ast.Statement exposing (..)

type Action
  = NoOp
  | Update String

init : String
init = """module Main where

f : Int -> Int
f x = x + 1

g : Int -> Int
g x = x * 2

h = f << g
"""

update : Action -> String -> String
update action model =
  case action of
    NoOp ->
      model

    Update m ->
      m

withChild : a -> List Html -> Html
withChild title children =
  li [] [ pre [] [ text <| toString title ]
        , ul [] children
        ]

expression : Expression -> Html
expression e =
  case e of
    Range e1 e2 ->
      withChild e [ expression e1
                  , expression e2
                  ]

    List es ->
      withChild e (List.map expression es)

    Application e1 e2 ->
      withChild e [ expression e1
                  , expression e2
                  ]

    e ->
      li [] [ pre [] [ text <| toString e ] ]

statement : Statement -> Html
statement s =
  case s of
    FunctionDeclaration _ _ e ->
      withChild s [ expression e ]

    s ->
      li [] [ pre [] [ text <| toString s ] ]

tree : String -> Html
tree m =
  case Ast.parse m of
    (Ok statements, _) ->
      ul [] (List.map statement statements)

    err ->
      div [] [ text <| toString err ]

view : Signal.Address Action -> String -> Html
view address model =
  div [] [ textarea [ on "input" targetValue (Signal.message address << Update) ] [ text model ]
         , tree model
         ]

mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoOp

main : Signal Html
main =
  Signal.foldp update init mailbox.signal
    |> Signal.map (view mailbox.address)
