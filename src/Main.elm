port module Main exposing (main)

import Generator
import Parser exposing ((|.), (|=), Parser, Problem(..), Step(..), Trailing(..))
import Types exposing (AggregateType(..), EnumType, EnumValue(..), Field(..), Model, NonEnumType(..), PrimitiveType(..), Type(..), UnionMember(..), UserType(..))


port print : String -> Cmd msg


port exit : Int -> Cmd msg


port writeFile : String -> Cmd msg


port printDone : ({} -> msg) -> Sub msg


port writeFileDone : ({} -> msg) -> Sub msg


type alias Flags =
    Model


type Msg
    = PrintDone
    | WriteFileDone


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ printDone (always PrintDone)
        , writeFileDone (always WriteFileDone)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PrintDone ->
            let
                content =
                    case Parser.run mainParser model.schema of
                        Ok userTypes ->
                            Generator.generate model userTypes

                        Err deadEnds ->
                            String.join "\n" <|
                                List.map deadEndToString deadEnds
            in
            ( model
            , writeFile content
            )

        WriteFileDone ->
            ( model, exit 0 )


deadEndToString : Parser.DeadEnd -> String
deadEndToString { col, problem, row } =
    " - "
        ++ String.fromInt (row - 2)
        ++ ","
        ++ String.fromInt col
        ++ " "
        ++ problemToString problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Expecting s ->
            "Expected \"" ++ s ++ "\""

        ExpectingInt ->
            "branch 'ExpectingInt' not implemented"

        ExpectingHex ->
            "branch 'ExpectingHex' not implemented"

        ExpectingOctal ->
            "branch 'ExpectingOctal' not implemented"

        ExpectingBinary ->
            "branch 'ExpectingBinary' not implemented"

        ExpectingFloat ->
            "branch 'ExpectingFloat' not implemented"

        ExpectingNumber ->
            "branch 'ExpectingNumber' not implemented"

        ExpectingVariable ->
            "branch 'ExpectingVariable' not implemented"

        ExpectingSymbol s ->
            "Expected \"" ++ s ++ "\""

        ExpectingKeyword _ ->
            "branch 'ExpectingKeyword _' not implemented"

        ExpectingEnd ->
            "branch 'ExpectingEnd' not implemented"

        UnexpectedChar ->
            "branch 'UnexpectedChar' not implemented"

        Problem p ->
            p

        BadRepeat ->
            "branch 'BadRepeat' not implemented"


mainParser : Parser (List UserType)
mainParser =
    Parser.succeed identity
        |. whitespace
        |= Parser.loop [] userType


userType :
    List UserType
    -> Parser (Parser.Step (List UserType) (List UserType))
userType acc =
    Parser.oneOf
        [ Parser.map (\t -> Loop <| t :: acc)
            (Parser.succeed Type
                |. Parser.keyword "type"
                |. whitespace
                |= userTypeName
                |. whitespace
                |= nonEnumType
            )
        , Parser.map (\t -> Loop <| t :: acc)
            (Parser.succeed Enum
                |. Parser.keyword "enum"
                |. whitespace
                |= userTypeName
                |. whitespace
                |= enumType
            )
        , Parser.succeed (Done <| List.reverse acc)
            |. Parser.end
        ]
        |. whitespace


enumType : Parser EnumType
enumType =
    Parser.sequence
        { start = "{"
        , end = "}"
        , separator = ""
        , item = enumValue
        , spaces = whitespace
        , trailing = Optional
        }


enumValue : Parser EnumValue
enumValue =
    Parser.succeed EnumValue
        |= enumValueName
        |. whitespace
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "="
                |. whitespace
                |= Parser.int
            , Parser.succeed Nothing
            ]


enumValueName : Parser String
enumValueName =
    Parser.getChompedString
        (Parser.chompIf Char.isUpper
            |. Parser.chompWhile (\c -> Char.isUpper c || Char.isDigit c || c == '_')
        )


nonEnumType : Parser NonEnumType
nonEnumType =
    Parser.oneOf
        [ Parser.map PrimitiveType primitiveType
        , Parser.map AggregateType aggregateType
        , Parser.map UserTypeName userTypeName
        ]


primitiveType : Parser PrimitiveType
primitiveType =
    Parser.oneOf
        [ Parser.succeed Int |. Parser.symbol "int"
        , Parser.succeed (I 8) |. Parser.symbol "i8"
        , Parser.succeed (I 16) |. Parser.symbol "i16"
        , Parser.succeed (I 32) |. Parser.symbol "i32"
        , Parser.succeed (I 64) |. Parser.symbol "i64"
        , Parser.succeed UInt |. Parser.symbol "uint"
        , Parser.succeed (U 8) |. Parser.symbol "u8"
        , Parser.succeed (U 16) |. Parser.symbol "u16"
        , Parser.succeed (U 32) |. Parser.symbol "u32"
        , Parser.succeed (U 64) |. Parser.symbol "u64"
        , Parser.succeed (Float 32) |. Parser.symbol "f32"
        , Parser.succeed (Float 64) |. Parser.symbol "f64"
        , Parser.succeed Bool |. Parser.symbol "bool"
        , Parser.succeed String |. Parser.symbol "string"
        , Parser.succeed (Data << Just)
            |. Parser.symbol "data<"
            |= Parser.int
            |. Parser.symbol ">"
        , Parser.succeed (Data Nothing) |. Parser.symbol "data"
        , Parser.succeed Void |. Parser.symbol "void"
        ]
        |. whitespace


aggregateType : Parser AggregateType
aggregateType =
    Parser.oneOf
        [ optionalType
        , arrayType
        , mapType
        , unionType
        , structType
        ]


structType : Parser AggregateType
structType =
    let
        field =
            Parser.succeed Field
                |= Parser.getChompedString
                    (Parser.chompIf Char.isAlpha
                        |. Parser.chompWhile Char.isAlpha
                    )
                |. whitespace
                |. Parser.symbol ":"
                |. whitespace
                |= Parser.lazy (\_ -> type_)
    in
    Parser.succeed StructType
        |= Parser.sequence
            { start = "{"
            , end = "}"
            , separator = ""
            , trailing = Optional
            , item = field
            , spaces = whitespace
            }



{- struct-type     = "{" [WS] fields [WS] "}"
   fields          = field / (fields WS field)
   field           = 1*ALPHA [WS] ":" [WS] type
-}


unionType : Parser AggregateType
unionType =
    Parser.succeed UnionType
        |= Parser.sequence
            { start = "("
            , end = ")"
            , spaces = whitespace
            , separator = "|"
            , trailing = Forbidden
            , item = unionMember
            }


unionMember : Parser UnionMember
unionMember =
    Parser.succeed UnionMember
        |= Parser.lazy (\_ -> type_)
        |. whitespace
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "="
                |. whitespace
                |= Parser.int
            , Parser.succeed Nothing
            ]


mapType : Parser AggregateType
mapType =
    Parser.succeed MapType
        |. Parser.symbol "map["
        |= Parser.lazy (\_ -> type_)
        |. Parser.symbol "]"
        |= Parser.lazy (\_ -> type_)


arrayType : Parser AggregateType
arrayType =
    Parser.succeed ArrayType
        |. Parser.symbol "["
        |= Parser.oneOf
            [ Parser.succeed Just |= Parser.int
            , Parser.succeed Nothing
            ]
        |. Parser.symbol "]"
        |= Parser.lazy (\_ -> type_)


optionalType : Parser AggregateType
optionalType =
    Parser.succeed OptionalType
        |. Parser.symbol "optional<"
        |= Parser.lazy (\_ -> type_)
        |. Parser.symbol ">"


type_ : Parser Type
type_ =
    Parser.oneOf
        [ Parser.succeed NonEnumType |= nonEnumType
        , Parser.succeed EnumType |= enumType
        ]


userTypeName : Parser String
userTypeName =
    Parser.getChompedString <|
        Parser.succeed identity
            |. Parser.chompIf Char.isUpper
            |. Parser.chompWhile Char.isAlphaNum


whitespace : Parser ()
whitespace =
    let
        step () =
            Parser.succeed
                (\f t ->
                    if f == t then
                        Done ()

                    else
                        Loop ()
                )
                |= Parser.getOffset
                |. Parser.oneOf
                    [ Parser.lineComment "#"
                    , Parser.spaces
                    ]
                |= Parser.getOffset
    in
    Parser.loop () step


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( flags
    , print <| "Writing module " ++ flags.moduleName ++ " to " ++ flags.outputFile
    )
