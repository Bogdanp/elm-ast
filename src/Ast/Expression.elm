module Ast.Expression exposing
    ( Expression(..)
    , expression )

{-| This module exposes parsers for Elm expressions.

# Types
@docs Expression

# Parsers
@docs expression

-}

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num
import Dict exposing (Dict)
import List.Extra exposing (break, singleton)
import String

import Ast.BinOp exposing (..)
import Ast.Helpers exposing (..)

type Collect a
  = Cont a
  | Stop a

{-| Representations for Elm's expressions. -}
type Expression
  = Character Char
  | String String
  | Integer Int
  | Float Float
  | Variable (List Name)
  | Range Expression Expression
  | List (List Expression)
  | Access Expression (List Name)
  | Record (List (Name, Expression))
  | RecordUpdate Name (List (Name, Expression))
  | If Expression Expression Expression
  | Let (List (Name, Expression)) Expression
  | Case Expression (List (Expression, Expression))
  | Lambda (List Name) Expression
  | Application Expression Expression
  | BinOp Expression Expression Expression

character : Parser Expression
character =
  Character <$> between' (char '\'') anyChar

string : Parser Expression
string =
  let
    singleString =
      String
        <$> (Combine.string "\"" *> regex "(\\\\\"|[^\"\n])*" <* Combine.string "\"")

    multiString  =
      (String << String.concat)
        <$> (Combine.string "\"\"\"" *> many (regex "[^\"]*") <* Combine.string "\"\"\"")
  in
    multiString <|> singleString

integer : Parser Expression
integer =
  Integer <$> Combine.Num.int

float : Parser Expression
float =
  Float <$> Combine.Num.float

access : Parser Expression
access =
  Access <$> variable <*> many1 (Combine.string "." *> loName)

variable : Parser Expression
variable =
  Variable <$> choice [ singleton <$> loName
                      , sepBy1 (Combine.string "." ) upName
                      ]

range : OpTable -> Parser Expression
range ops =
  rec <| \() ->
    brackets
      <| Range
           <$> (expression ops)
           <*> (symbol ".." *> expression ops)

list : OpTable -> Parser Expression
list ops =
  rec <| \() ->
    List <$> brackets (commaSeparated' (expression ops))

record : OpTable -> Parser Expression
record ops =
  rec <| \() ->
    Record <$> braces (commaSeparated' ((,) <$> loName <*> (symbol "=" *> expression ops)))

letExpression : OpTable -> Parser Expression
letExpression ops =
  let
    binding =
      rec <| \() ->
        (,)
          <$> (between' whitespace loName)
          <*> (symbol "=" *> expression ops)
  in
    rec <| \() ->
      Let
        <$> (symbol "let" *> many1 binding)
        <*> (symbol "in" *> expression ops)

ifExpression : OpTable -> Parser Expression
ifExpression ops =
  rec <| \() ->
    If
      <$> (symbol "if" *> expression ops)
      <*> (symbol "then" *> expression ops)
      <*> (symbol "else" *> expression ops)

caseExpression : OpTable -> Parser Expression
caseExpression ops =
  let
    binding =
      rec <| \() ->
        (,)
          <$> (whitespace *> expression ops)
          <*> (symbol "->" *> expression ops)
  in
    rec <| \() ->
      Case
        <$> (symbol "case" *> expression ops)
        <*> (symbol "of" *> many1 binding)

lambda : OpTable -> Parser Expression
lambda ops =
  rec <| \() ->
    Lambda
      <$> (symbol "\\" *> many (between' spaces loName))
      <*> (symbol "->" *> expression ops)

application : OpTable -> Parser Expression
application ops =
  rec <| \() ->
    term ops `chainl` (Application <$ spaces')

binary : OpTable -> Parser Expression
binary ops =
  rec <| \() ->
    let
      next =
        between' whitespace operator `andThen` \op ->
          choice [ Cont <$> application ops, Stop <$> expression ops ] `andThen` \e ->
            case e of
              Cont t -> ((::) (op, t)) <$> collect
              Stop e -> succeed [(op, e)]

      collect = next <|> succeed []
    in
      application ops `andThen` \e ->
        collect `andThen` \eops ->
          split ops 0 e eops

term : OpTable -> Parser Expression
term ops =
  rec <| \() ->
    choice [ character, string, float, integer, access, variable
           , range ops, list ops, record ops
           , parens (expression ops)
           ]

{-| A parser for Elm expressions. -}
expression : OpTable -> Parser Expression
expression ops =
  rec <| \() ->
    choice [ letExpression ops
           , caseExpression ops
           , ifExpression ops
           , lambda ops
           , binary ops
           ]

op : OpTable -> String -> (Assoc, Int)
op ops n =
  Dict.get n ops
    |> Maybe.withDefault (L, 9)

assoc : OpTable -> String -> Assoc
assoc ops n = fst <| op ops n

level : OpTable -> String -> Int
level ops n = snd <| op ops n

hasLevel : OpTable -> Int -> (String, Expression) -> Bool
hasLevel ops l (n, _) = level ops n == l

split : OpTable -> Int -> Expression -> List (String, Expression) -> Parser Expression
split ops l e eops =
  case eops of
    [] ->
      succeed e

    _ ->
      findAssoc ops l eops `andThen` \assoc ->
        Ast.Helpers.sequence (splitLevel ops l e eops) `andThen` \es ->
          let ops' = List.filterMap (\x -> if hasLevel ops l x
                                           then Just (fst x)
                                           else Nothing) eops in
          case assoc of
            R -> joinR es ops'
            _ -> joinL es ops'

splitLevel : OpTable -> Int -> Expression -> List (String, Expression) -> List (Parser Expression)
splitLevel ops l e eops =
  case break (hasLevel ops l) eops of
    (lops, (_, e')::rops) ->
      split ops (l + 1) e lops :: splitLevel ops l e' rops

    (lops, []) ->
      [ split ops (l + 1) e lops ]

joinL : List Expression -> List String -> Parser Expression
joinL es ops =
  case (es, ops) of
    ([e], []) ->
      succeed e

    (a::b::remE, op::remO) ->
      joinL ((BinOp (Variable [op]) a b) :: remE) remO

    _ ->
      fail []

joinR : List Expression -> List String -> Parser Expression
joinR es ops =
  case (es, ops) of
    ([e], []) ->
      succeed e

    (a::b::remE, op::remO) ->
      joinR (b::remE) remO `andThen` \e ->
        succeed (BinOp (Variable [op]) a e)

    _ ->
      fail []

findAssoc : OpTable -> Int -> List (String, Expression) -> Parser Assoc
findAssoc ops l eops =
  let
    lops = List.filter (hasLevel ops l) eops
    assocs = List.map (assoc ops << fst) lops
    error issue =
      let operators = List.map fst lops |> String.join " and " in
      "conflicting " ++ issue ++ " for operators " ++ operators
  in
    if List.all ((==) L) assocs then
      succeed L
    else if List.all ((==) R) assocs then
      succeed R
    else if List.all ((==) N) assocs then
      case assocs of
        [_] -> succeed N
        _   -> fail [ error "precedence" ]
    else
      fail [ error "associativity" ]
