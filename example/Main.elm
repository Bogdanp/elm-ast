module Main where

import Html exposing (..)
import Html.Events exposing (..)

import Ast

type Action
  = NoOp
  | Update String

update : Action -> String -> String
update action model =
  case action of
    NoOp ->
      model

    Update m ->
      m

view : Signal.Address Action -> String -> Html
view address model =
  div [] [ textarea [ on "input" targetValue (Signal.message address << Update) ] [ text model ]
         , pre [] [ text <| toString <| Ast.parse model ]
         ]

mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoOp

main : Signal Html
main =
  Signal.foldp update "" mailbox.signal
    |> Signal.map (view mailbox.address)
