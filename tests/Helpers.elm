module Helpers exposing (..)

import Ast exposing (parse, parseExpression, parseStatement)
import Ast.BinOp exposing (Assoc, operators)
import Ast.Expression exposing (Expression(..), MExp, WithMeta, dropMeta)
import Ast.Helpers exposing (Alias, ModuleName, Name)
import Ast.Statement exposing (ExportSet(..), Statement(..), Type(..))
import Expect exposing (..)



-- Structures


type ExpressionSansMeta
    = CharacterSM Char
    | StringSM String
    | IntegerSM Int
    | FloatSM Float
    | VariableSM (List Name)
    | ListSM (List ExpressionSansMeta)
    | TupleSM (List ExpressionSansMeta)
    | AccessSM ExpressionSansMeta (List Name)
    | AccessFunctionSM Name
    | RecordSM (List ( Name, ExpressionSansMeta ))
    | RecordUpdateSM Name (List ( Name, ExpressionSansMeta ))
    | IfSM ExpressionSansMeta ExpressionSansMeta ExpressionSansMeta
    | LetSM (List ( ExpressionSansMeta, ExpressionSansMeta )) ExpressionSansMeta
    | CaseSM ExpressionSansMeta (List ( ExpressionSansMeta, ExpressionSansMeta ))
    | LambdaSM (List ExpressionSansMeta) ExpressionSansMeta
    | ApplicationSM ExpressionSansMeta ExpressionSansMeta
    | BinOpSM ExpressionSansMeta ExpressionSansMeta ExpressionSansMeta


type StatementSansMeta
    = ModuleDeclarationSM ModuleName ExportSet
    | PortModuleDeclarationSM ModuleName ExportSet
    | EffectModuleDeclarationSM ModuleName (List ( Name, Name )) ExportSet
    | ImportStatementSM ModuleName (Maybe Alias) (Maybe ExportSet)
    | TypeAliasDeclarationSM Type Type
    | TypeDeclarationSM Type (List Type)
    | PortTypeDeclarationSM Name Type
    | PortDeclarationSM Name (List Name) ExpressionSansMeta
    | FunctionTypeDeclarationSM Name Type
    | FunctionDeclarationSM Name (List ExpressionSansMeta) ExpressionSansMeta
    | InfixDeclarationSM Assoc Int Name
    | CommentSM String


dropStatementMeta : Statement -> StatementSansMeta
dropStatementMeta s =
    case s of
        ModuleDeclaration mn es ->
            ModuleDeclarationSM mn es

        PortModuleDeclaration mn es ->
            PortModuleDeclarationSM mn es

        EffectModuleDeclaration mn l es ->
            EffectModuleDeclarationSM mn l es

        ImportStatement mn a es ->
            ImportStatementSM mn a es

        TypeAliasDeclaration t1 t2 ->
            TypeAliasDeclarationSM t1 t2

        TypeDeclaration t l ->
            TypeDeclarationSM t l

        PortTypeDeclaration n t ->
            PortTypeDeclarationSM n t

        PortDeclaration n l e ->
            PortDeclarationSM n l (dropMExpMeta e)

        FunctionTypeDeclaration n t ->
            FunctionTypeDeclarationSM n t

        FunctionDeclaration n l e ->
            FunctionDeclarationSM n (List.map dropMExpMeta l) (dropMExpMeta e)

        InfixDeclaration a i n ->
            InfixDeclarationSM a i n

        Comment s ->
            CommentSM s


dropMExpMeta : MExp -> ExpressionSansMeta
dropMExpMeta ( e, _ ) =
    dropExpressionMeta e


dropWithMetaMExp : ( WithMeta a {}, MExp ) -> ( a, ExpressionSansMeta )
dropWithMetaMExp ( a, b ) =
    ( dropMeta a, dropMExpMeta b )


dropDoubleMExp : ( MExp, MExp ) -> ( ExpressionSansMeta, ExpressionSansMeta )
dropDoubleMExp ( a, b ) =
    ( dropMExpMeta a, dropMExpMeta b )


dropExpressionMeta : Expression -> ExpressionSansMeta
dropExpressionMeta e =
    case e of
        Character c ->
            CharacterSM c

        String s ->
            StringSM s

        Integer i ->
            IntegerSM i

        Float f ->
            FloatSM f

        Variable l ->
            VariableSM l

        List l ->
            ListSM (List.map dropMExpMeta l)

        Tuple l ->
            TupleSM (List.map dropMExpMeta l)

        Access ex l ->
            AccessSM (dropMExpMeta ex) (List.map dropMeta l)

        AccessFunction n ->
            AccessFunctionSM n

        Record l ->
            RecordSM (List.map dropWithMetaMExp l)

        RecordUpdate n l ->
            RecordUpdateSM (dropMeta n) (List.map dropWithMetaMExp l)

        If e1 e2 e3 ->
            IfSM (dropMExpMeta e1) (dropMExpMeta e2) (dropMExpMeta e3)

        Let l e ->
            LetSM (List.map dropDoubleMExp l) (dropMExpMeta e)

        Case e l ->
            CaseSM (dropMExpMeta e) (List.map dropDoubleMExp l)

        Lambda l e ->
            LambdaSM (List.map dropMExpMeta l) (dropMExpMeta e)

        Application e1 e2 ->
            ApplicationSM (dropMExpMeta e1) (dropMExpMeta e2)

        BinOp e1 e2 e3 ->
            BinOpSM (dropMExpMeta e1) (dropMExpMeta e2) (dropMExpMeta e3)


access : ExpressionSansMeta -> List Name -> ExpressionSansMeta
access =
    AccessSM


accessFun : Name -> ExpressionSansMeta
accessFun =
    AccessFunctionSM


app : ExpressionSansMeta -> ExpressionSansMeta -> ExpressionSansMeta
app left right =
    ApplicationSM left right


lambda : List ExpressionSansMeta -> ExpressionSansMeta -> ExpressionSansMeta
lambda =
    LambdaSM


record : List ( Name, ExpressionSansMeta ) -> ExpressionSansMeta
record =
    RecordSM


recordUpdate : String -> List ( String, ExpressionSansMeta ) -> ExpressionSansMeta
recordUpdate name =
    RecordUpdateSM name


var : String -> ExpressionSansMeta
var name =
    VariableSM [ name ]


integer : Int -> ExpressionSansMeta
integer =
    IntegerSM


float : Float -> ExpressionSansMeta
float =
    FloatSM


character : Char -> ExpressionSansMeta
character =
    CharacterSM


string : String -> ExpressionSansMeta
string =
    StringSM


binOp :
    ExpressionSansMeta
    -> ExpressionSansMeta
    -> ExpressionSansMeta
    -> ExpressionSansMeta
binOp name l r =
    BinOpSM name l r


let_ : List ( ExpressionSansMeta, ExpressionSansMeta ) -> ExpressionSansMeta -> ExpressionSansMeta
let_ =
    LetSM


tuple : List ExpressionSansMeta -> ExpressionSansMeta
tuple =
    TupleSM


case_ :
    ExpressionSansMeta
    -> List ( ExpressionSansMeta, ExpressionSansMeta )
    -> ExpressionSansMeta
case_ =
    CaseSM


list : List ExpressionSansMeta -> ExpressionSansMeta
list =
    ListSM


moduleDeclaration : ModuleName -> ExportSet -> StatementSansMeta
moduleDeclaration =
    ModuleDeclarationSM


portModuleDeclaration : ModuleName -> ExportSet -> StatementSansMeta
portModuleDeclaration =
    PortModuleDeclarationSM


effectModuleDeclaration :
    ModuleName
    -> List ( Name, Name )
    -> ExportSet
    -> StatementSansMeta
effectModuleDeclaration =
    EffectModuleDeclarationSM


importStatement :
    ModuleName
    -> Maybe Alias
    -> Maybe ExportSet
    -> StatementSansMeta
importStatement =
    ImportStatementSM


typeAliasDeclaration : Type -> Type -> StatementSansMeta
typeAliasDeclaration =
    TypeAliasDeclarationSM


typeDeclaration : Type -> List Type -> StatementSansMeta
typeDeclaration =
    TypeDeclarationSM


portTypeDeclaration : Name -> Type -> StatementSansMeta
portTypeDeclaration =
    PortTypeDeclarationSM


portDeclaration : Name -> List Name -> ExpressionSansMeta -> StatementSansMeta
portDeclaration =
    PortDeclarationSM


functionTypeDeclaration : Name -> Type -> StatementSansMeta
functionTypeDeclaration =
    FunctionTypeDeclarationSM


functionDeclaration :
    Name
    -> List ExpressionSansMeta
    -> ExpressionSansMeta
    -> StatementSansMeta
functionDeclaration =
    FunctionDeclarationSM


infixDeclaration : Assoc -> Int -> Name -> StatementSansMeta
infixDeclaration =
    InfixDeclarationSM


comment : String -> StatementSansMeta
comment =
    CommentSM



-- Helpers


fails : String -> Expectation
fails s =
    case parseExpression operators s of
        Err _ ->
            Expect.pass

        _ ->
            Expect.fail (s ++ " expected to fail")


simpleParse : String -> Result String MExp
simpleParse i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, e ) ->
            Ok e

        Err ( _, { position }, es ) ->
            Err ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isExpression : MExp -> String -> Expectation
isExpression e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal e r

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isExpressionSansMeta : ExpressionSansMeta -> String -> Expectation
isExpressionSansMeta e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal e (dropMExpMeta r)

        Err ( _, a, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString a.position ++ " rest: |" ++ a.input ++ "| with errors: " ++ toString es)


isApplicationSansMeta : ExpressionSansMeta -> List ExpressionSansMeta -> String -> Expectation
isApplicationSansMeta fn args i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, app ) ->
            let
                l =
                    List.foldl (flip ApplicationSM) fn args

                r =
                    dropMExpMeta app
            in
            Expect.equal l r

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isStatement : Statement -> String -> Expectation
isStatement s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


isStatementSansMeta : StatementSansMeta -> String -> Expectation
isStatementSansMeta s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal (dropStatementMeta r) s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


areStatements : List Statement -> String -> Expectation
areStatements s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)


areStatementsSansMeta : List StatementSansMeta -> String -> Expectation
areStatementsSansMeta s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal (List.map dropStatementMeta r) s

        Err ( _, { position }, es ) ->
            Expect.fail ("failed to parse: " ++ i ++ " at position " ++ toString position ++ " with errors: " ++ toString es)
