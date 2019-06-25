module Helpers exposing (ExpressionSansMeta(..), PatternSansMeta(..), StatementSansMeta(..), access, accessFun, app, applicationFromListSM, applicationPattern, areStatements, areStatementsSansMeta, asPattern, binOp, case_, character, characterPattern, comment, consPattern, constructorPattern, dropDoubleMExp, dropExpressionMeta, dropMExpMeta, dropPatternMatches, dropPatternMeta, dropStatementMeta, dropWithMetaMExp, effectModuleDeclaration, fails, failsPattern, failure, float, floatPattern, functionDeclaration, functionTypeDeclaration, importStatement, infixDeclaration, integer, integerPattern, isApplicationSansMeta, isExpression, isExpressionSansMeta, isPattern, isPatternSansMeta, isStatement, isStatementSansMeta, lambda, let_, list, listPattern, moduleDeclaration, portDeclaration, portModuleDeclaration, portTypeDeclaration, record, recordUpdate, simpleParse, string, stringPattern, tuple, tuplePattern, typeAliasDeclaration, typeDeclaration, var, variablePattern, wildPattern)

import Ast exposing (parse, parseExpression, parsePattern, parseStatement)
import Ast.BinOp exposing (Assoc, operators)
import Ast.Common exposing (..)
import Ast.Expression exposing (Expression(..), MExp)
import Ast.Statement exposing (ExportSet(..), Statement, StatementBase(..), Type(..))
import Expect exposing (..)



-- Structures


type ExpressionSansMeta
    = LiteralSM Literal
    | VariableSM Name
    | ConstructorSM Name
    | ExternalSM (List Name) ExpressionSansMeta
    | ListSM (List ExpressionSansMeta)
    | TupleSM (List ExpressionSansMeta)
    | AccessSM ExpressionSansMeta (List Name)
    | AccessFunctionSM Name
    | RecordSM (List ( Name, ExpressionSansMeta ))
    | RecordUpdateSM Name (List ( Name, ExpressionSansMeta ))
    | IfSM ExpressionSansMeta ExpressionSansMeta ExpressionSansMeta
    | LetSM (List ( PatternSansMeta, ExpressionSansMeta )) ExpressionSansMeta
    | CaseSM ExpressionSansMeta (List ( PatternSansMeta, ExpressionSansMeta ))
    | LambdaSM (List PatternSansMeta) ExpressionSansMeta
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
    | FunctionDeclarationSM PatternSansMeta ExpressionSansMeta
    | InfixDeclarationSM Assoc Int Name
    | CommentSM String


type PatternSansMeta
    = PWildSM
    | PVariableSM Name
    | PConstructorSM Name
    | PLiteralSM Literal
    | PTupleSM (List PatternSansMeta)
    | PConsSM PatternSansMeta PatternSansMeta
    | PListSM (List PatternSansMeta)
    | PRecordSM (List Name)
    | PAsSM PatternSansMeta Name
    | PApplicationSM PatternSansMeta PatternSansMeta


dropStatementMeta : Statement -> StatementSansMeta
dropStatementMeta ( s, _ ) =
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

        FunctionDeclaration p e ->
            FunctionDeclarationSM (dropPatternMeta p) (dropMExpMeta e)

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


dropPatternMatches : List ( MPattern, MExp ) -> List ( PatternSansMeta, ExpressionSansMeta )
dropPatternMatches l =
    let
        ( patterns, exps ) =
            List.unzip l

        expsSM =
            List.map dropMExpMeta exps

        patternsSM =
            List.map dropPatternMeta patterns
    in
    List.map2 (,) patternsSM expsSM


dropExpressionMeta : Expression -> ExpressionSansMeta
dropExpressionMeta e =
    case e of
        Literal l ->
            LiteralSM l

        Variable l ->
            VariableSM l

        Constructor n ->
            ConstructorSM n

        External l e ->
            ExternalSM l (dropMExpMeta e)

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
            LetSM (dropPatternMatches l) (dropMExpMeta e)

        Case e l ->
            CaseSM (dropMExpMeta e) (dropPatternMatches l)

        Lambda l e ->
            LambdaSM (List.map dropPatternMeta l) (dropMExpMeta e)

        Application e1 e2 ->
            ApplicationSM (dropMExpMeta e1) (dropMExpMeta e2)

        BinOp e1 e2 e3 ->
            BinOpSM (dropMExpMeta e1) (dropMExpMeta e2) (dropMExpMeta e3)


dropPatternMeta : MPattern -> PatternSansMeta
dropPatternMeta ( p, _ ) =
    case p of
        PWild ->
            PWildSM

        PVariable n ->
            PVariableSM n

        PConstructor n ->
            PConstructorSM n

        PLiteral l ->
            PLiteralSM l

        PTuple li ->
            PTupleSM <| List.map dropPatternMeta li

        PCons head tail ->
            PConsSM (dropPatternMeta head) (dropPatternMeta tail)

        PList li ->
            PListSM <| List.map dropPatternMeta li

        PRecord li ->
            PRecordSM <| List.map Tuple.first li

        PAs pat name ->
            PAsSM (dropPatternMeta pat) name

        PApplication p1 p2 ->
            PApplicationSM (dropPatternMeta p1) (dropPatternMeta p2)


access : ExpressionSansMeta -> List Name -> ExpressionSansMeta
access =
    AccessSM


accessFun : Name -> ExpressionSansMeta
accessFun =
    AccessFunctionSM


app : ExpressionSansMeta -> ExpressionSansMeta -> ExpressionSansMeta
app left right =
    ApplicationSM left right


lambda : List PatternSansMeta -> ExpressionSansMeta -> ExpressionSansMeta
lambda =
    LambdaSM


record : List ( Name, ExpressionSansMeta ) -> ExpressionSansMeta
record =
    RecordSM


recordUpdate : String -> List ( String, ExpressionSansMeta ) -> ExpressionSansMeta
recordUpdate name =
    RecordUpdateSM name


var : String -> ExpressionSansMeta
var =
    VariableSM


variablePattern : String -> PatternSansMeta
variablePattern =
    PVariableSM


constructorPattern : String -> PatternSansMeta
constructorPattern =
    PConstructorSM


wildPattern : PatternSansMeta
wildPattern =
    PWildSM


consPattern : PatternSansMeta -> PatternSansMeta -> PatternSansMeta
consPattern =
    PConsSM


tuplePattern : List PatternSansMeta -> PatternSansMeta
tuplePattern =
    PTupleSM


listPattern : List PatternSansMeta -> PatternSansMeta
listPattern =
    PListSM


asPattern : PatternSansMeta -> Name -> PatternSansMeta
asPattern =
    PAsSM


integer : Int -> ExpressionSansMeta
integer =
    LiteralSM << Integer


integerPattern : Int -> PatternSansMeta
integerPattern =
    PLiteralSM << Integer


float : Float -> ExpressionSansMeta
float =
    LiteralSM << Float


floatPattern : Float -> PatternSansMeta
floatPattern =
    PLiteralSM << Float


character : Char -> ExpressionSansMeta
character =
    LiteralSM << Character


characterPattern : Char -> PatternSansMeta
characterPattern =
    PLiteralSM << Character


string : String -> ExpressionSansMeta
string =
    LiteralSM << String


stringPattern : String -> PatternSansMeta
stringPattern =
    PLiteralSM << String


binOp :
    ExpressionSansMeta
    -> ExpressionSansMeta
    -> ExpressionSansMeta
    -> ExpressionSansMeta
binOp name l r =
    BinOpSM name l r


applicationPattern : PatternSansMeta -> PatternSansMeta -> PatternSansMeta
applicationPattern =
    PApplicationSM


applicationFromListSM : PatternSansMeta -> List PatternSansMeta -> PatternSansMeta
applicationFromListSM acc li =
    case li of
        [] ->
            acc

        right :: rest ->
            applicationFromListSM (PApplicationSM acc right) rest


let_ : List ( PatternSansMeta, ExpressionSansMeta ) -> ExpressionSansMeta -> ExpressionSansMeta
let_ =
    LetSM


tuple : List ExpressionSansMeta -> ExpressionSansMeta
tuple =
    TupleSM


case_ :
    ExpressionSansMeta
    -> List ( PatternSansMeta, ExpressionSansMeta )
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
    PatternSansMeta
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


failsPattern : String -> Expectation
failsPattern s =
    case parsePattern s of
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
            Err <| failure i position es


isExpression : MExp -> String -> Expectation
isExpression e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal e r

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


isExpressionSansMeta : ExpressionSansMeta -> String -> Expectation
isExpressionSansMeta e i =
    case parseExpression operators (String.trim i) of
        Ok ( _, _, r ) ->
            Expect.equal e (dropMExpMeta r)

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


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
            Expect.fail <| failure i position es


isStatement : Statement -> String -> Expectation
isStatement s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


isStatementSansMeta : StatementSansMeta -> String -> Expectation
isStatementSansMeta s i =
    case parseStatement operators i of
        Ok ( _, _, r ) ->
            Expect.equal (dropStatementMeta r) s

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


areStatements : List Statement -> String -> Expectation
areStatements s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal r s

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


areStatementsSansMeta : List StatementSansMeta -> String -> Expectation
areStatementsSansMeta s i =
    case parse i of
        Ok ( _, _, r ) ->
            Expect.equal (List.map dropStatementMeta r) s

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


isPattern : MPattern -> String -> Expectation
isPattern p i =
    case parsePattern i of
        Ok ( _, _, r ) ->
            Expect.equal r p

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


isPatternSansMeta : PatternSansMeta -> String -> Expectation
isPatternSansMeta p i =
    case parsePattern i of
        Ok ( _, _, r ) ->
            Expect.equal (dropPatternMeta r) p

        Err ( _, { position }, es ) ->
            Expect.fail <| failure i position es


failure : String -> Int -> List String -> String
failure i position es =
    "failed to parse: " ++ i ++ " at position " ++ toString position ++ " (" ++ String.slice position (position + 1) i ++ ") with errors: " ++ toString es
