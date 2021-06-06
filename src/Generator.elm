module Generator exposing (generate)

import Elm.CodeGen as Gen exposing (Declaration, Expression, File, Import, Module, TopLevelExpose, TypeAnnotation)
import Elm.Pretty
import String.Extra as String
import Types exposing (AggregateType(..), EnumType, EnumValue(..), Field(..), Model, NonEnumType(..), PrimitiveType(..), Type(..), UnionMember(..), UserType(..))


generate : Model -> List UserType -> String
generate model userTypes =
    file model userTypes
        |> Elm.Pretty.pretty 83


file : Model -> List UserType -> File
file model userTypes =
    let
        input =
            userTypes
                |> List.map (\ut -> "\n\n" ++ userTypeToString 0 ut)
                |> String.concat
                |> String.split "\n"
                |> List.map
                    (\s ->
                        if String.isEmpty s then
                            s

                        else
                            "    " ++ s
                    )
                |> String.join "\n"

        comment =
            Gen.emptyFileComment
                |> Gen.markdown "Input:"
                |> Gen.code input
    in
    Gen.file (module_ model userTypes) imports (declarations userTypes) (Just comment)


declarations : List UserType -> List Declaration
declarations =
    List.concatMap toDeclarations


toDeclarations : UserType -> List Declaration
toDeclarations userType =
    let
        comment =
            Gen.emptyDocComment
                |> Gen.markdown ("Codec for the `" ++ typeName ++ "` type.")
                |> Just

        typeAnnotation =
            Just (Gen.typed "Codec" [ Gen.typed typeName [] ])

        typeName =
            getUserTypeName userType
    in
    [ toTypeDeclaration userType
    , Gen.funDecl comment typeAnnotation (toCodecName userType) [] (toCodecUserType userType)
    ]


codec : String -> Expression
codec =
    Gen.fqVal [ "Codec" ]


toCodecUserType : UserType -> Expression
toCodecUserType userType =
    case userType of
        Type name (AggregateType (UnionType members)) ->
            toCodecUnion name members

        Type name def ->
            codecNameNonEnumType def

        Enum _ defs ->
            toCodecEnum defs


toCodecEnum : EnumType -> Expression
toCodecEnum values =
    let
        isSequential =
            List.all (\(EnumValue _ e) -> e == Nothing) values

        toCouple (EnumValue variantName variantValue) ( i, acc ) =
            let
                j =
                    Maybe.withDefault i variantValue
            in
            ( j + 1, Gen.tuple [ Gen.val (enumVariantNameToElm variantName), Gen.int j ] :: acc )
    in
    if isSequential then
        Gen.apply [ codec "enum", Gen.list <| List.map (\(EnumValue variantName _) -> Gen.val <| enumVariantNameToElm variantName) values ]

    else
        Gen.apply [ codec "enumWithValues", Gen.list <| List.reverse <| Tuple.second <| List.foldl toCouple ( 0, [] ) values ]


toCodecUnion : String -> List UnionMember -> Expression
toCodecUnion arg1 arg2 =
    todoExpression "toCodecUnion"


codecNameNonEnumType : NonEnumType -> Expression
codecNameNonEnumType net =
    case net of
        PrimitiveType pt ->
            codecNamePrimitiveType pt

        AggregateType at ->
            codecNameAggregateType at

        UserTypeName name ->
            Gen.val <| String.decapitalize name ++ "Codec"


codecNameAggregateType : AggregateType -> Expression
codecNameAggregateType at =
    case at of
        OptionalType ot ->
            Gen.apply [ codec "optional", Gen.parens <| codecNameType ot ]

        ArrayType size ct ->
            arrayCodec size ct

        MapType _ _ ->
            todoExpression "branch 'MapType _ _' not implemented"

        UnionType _ ->
            todoExpression "branch 'UnionType _' not implemented"

        StructType fields ->
            structCodec fields


arrayCodec : Maybe Int -> Type -> Expression
arrayCodec size at =
    case size of
        Nothing ->
            Gen.apply
                [ codec "array"
                , Gen.parens <| codecNameType at
                ]

        Just s ->
            Gen.apply
                [ codec "arrayWithLength"
                , Gen.int s
                , Gen.parens <| codecNameType at
                ]


structCodec : List Field -> Expression
structCodec fields =
    let
        toPattern (Field name _) =
            Gen.varPattern name

        patterns =
            List.map toPattern fields

        toAssignment (Field name _) =
            ( name, Gen.val name )

        ctor =
            Gen.record (List.map toAssignment fields)

        ctorLambda =
            Gen.parens (Gen.lambda patterns ctor)

        step (Field name type_) =
            Gen.apply
                [ codec "field"
                , Gen.accessFun <| "." ++ name
                , Gen.parens <| codecNameType type_
                ]
    in
    Gen.pipe
        (Gen.apply [ codec "struct", ctorLambda ])
        (List.map step fields ++ [ codec "buildStruct" ])


codecNameType : Type -> Expression
codecNameType t =
    case t of
        NonEnumType net ->
            codecNameNonEnumType net

        EnumType _ ->
            todoExpression "branch 'EnumType _' not implemented"


codecNamePrimitiveType : PrimitiveType -> Expression
codecNamePrimitiveType pt =
    case pt of
        Int ->
            codec "int"

        I 64 ->
            todoExpression "i64 Codec is not supported yet"

        I s ->
            codec ("i" ++ String.fromInt s)

        UInt ->
            codec "uint"

        U 64 ->
            todoExpression "u64 Codec is not supported yet"

        U _ ->
            todoExpression "branch 'U _' not implemented"

        Float _ ->
            todoExpression "branch 'Float _' not implemented"

        Bool ->
            codec "bool"

        String ->
            codec "string"

        Data Nothing ->
            codec "data"

        Data (Just length) ->
            Gen.apply [ codec "dataWithLength", Gen.int length ]

        Void ->
            codec "void"


todoExpression : String -> Expression
todoExpression msg =
    Gen.apply [ Gen.fqFun [ "Debug" ] "todo", Gen.string msg ]


toTypeDeclaration : UserType -> Declaration
toTypeDeclaration userType =
    case userType of
        Type name (AggregateType (UnionType members)) ->
            toDeclarationUnion name members

        Type name def ->
            Gen.aliasDecl Nothing name [] (nonEnumTypeToAnnotation def)

        Enum name defs ->
            toDeclarationEnum name defs


isAlias : UserType -> Bool
isAlias userType =
    case userType of
        Type _ (AggregateType (UnionType _)) ->
            False

        Type _ _ ->
            True

        Enum _ _ ->
            False


nonEnumTypeToAnnotation : NonEnumType -> TypeAnnotation
nonEnumTypeToAnnotation net =
    case net of
        PrimitiveType pt ->
            primitiveTypeToAnnotation pt

        AggregateType at ->
            aggregateTypeToAnnotation at

        UserTypeName s ->
            Gen.typed s []


aggregateTypeToAnnotation : AggregateType -> TypeAnnotation
aggregateTypeToAnnotation atype =
    case atype of
        OptionalType t ->
            Gen.maybeAnn (typeToAnnotation t)

        ArrayType _ t ->
            Gen.typed "Array" [ typeToAnnotation t ]

        MapType f t ->
            Gen.dictAnn (typeToAnnotation f) (typeToAnnotation t)

        UnionType _ ->
            Debug.todo "branch 'UnionType _' not implemented"

        StructType fields ->
            structToAnnotation fields


structToAnnotation : List Field -> TypeAnnotation
structToAnnotation fields =
    let
        fieldToAnnotation (Field name ft) =
            ( name, typeToAnnotation ft )
    in
    Gen.recordAnn (List.map fieldToAnnotation fields)


typeToAnnotation : Type -> TypeAnnotation
typeToAnnotation type_ =
    case type_ of
        NonEnumType net ->
            nonEnumTypeToAnnotation net

        EnumType _ ->
            Debug.todo "branch 'EnumType _' not implemented"


primitiveTypeToAnnotation : PrimitiveType -> TypeAnnotation
primitiveTypeToAnnotation ptype =
    case ptype of
        Int ->
            Gen.intAnn

        I _ ->
            Gen.intAnn

        UInt ->
            Gen.intAnn

        U _ ->
            Gen.intAnn

        Float _ ->
            Gen.floatAnn

        Bool ->
            Gen.boolAnn

        String ->
            Gen.stringAnn

        Data _ ->
            Gen.typed "Bytes" []

        Void ->
            Gen.unitAnn


toDeclarationEnum : String -> EnumType -> Declaration
toDeclarationEnum name defs =
    let
        toVariant (EnumValue variantName _) =
            ( enumVariantNameToElm variantName, [] )
    in
    Gen.customTypeDecl Nothing name [] (List.map toVariant defs)


enumVariantNameToElm : String -> String
enumVariantNameToElm variantName =
    variantName
        |> String.split "_"
        |> List.map (String.toLower >> String.toTitleCase)
        |> String.concat


toDeclarationUnion : String -> List UnionMember -> Declaration
toDeclarationUnion name defs =
    let
        toVariant (UnionMember t _) =
            case t of
                NonEnumType (UserTypeName n) ->
                    ( name ++ n, [ Gen.typed n [] ] )

                _ ->
                    Debug.todo "branch '!_!' not implemented"
    in
    Gen.customTypeDecl Nothing name [] (List.map toVariant defs)


imports : List Import
imports =
    [ Gen.importStmt [ "Codec", "Bare" ]
        (Just [ "Codec" ])
        (Just (Gen.exposeExplicit [ Gen.closedTypeExpose "Codec", Gen.closedTypeExpose "Bytes" ]))
    , Gen.importStmt [ "Dict" ]
        Nothing
        (Just (Gen.exposeExplicit [ Gen.closedTypeExpose "Dict" ]))
    , Gen.importStmt [ "Array" ]
        Nothing
        (Just (Gen.exposeExplicit [ Gen.closedTypeExpose "Array" ]))
    ]


module_ : Model -> List UserType -> Module
module_ model userTypes =
    Gen.normalModule (String.split "." model.moduleName) (List.concatMap toExpose userTypes)


toExpose : UserType -> List TopLevelExpose
toExpose userType =
    [ Gen.funExpose <| toCodecName userType
    , let
        name =
            getUserTypeName userType
      in
      if isAlias userType then
        Gen.closedTypeExpose name

      else
        Gen.openTypeExpose name
    ]


toCodecName : UserType -> String
toCodecName userType =
    String.decapitalize (getUserTypeName userType) ++ "Codec"


getUserTypeName : UserType -> String
getUserTypeName userType =
    case userType of
        Type name _ ->
            name

        Enum name _ ->
            name


userTypeToString : Int -> UserType -> String
userTypeToString i ut =
    case ut of
        Type name value ->
            "type " ++ name ++ " " ++ nonEnumTypeToString i value

        Enum name options ->
            "enum " ++ name ++ " " ++ enumTypeToString options


enumTypeToString : EnumType -> String
enumTypeToString et =
    "{\n" ++ String.join "\n" (List.map optionToString et) ++ "\n}"


nonEnumTypeToString : Int -> NonEnumType -> String
nonEnumTypeToString i net =
    case net of
        PrimitiveType p ->
            primitiveTypeToString p

        AggregateType a ->
            aggregateTypeToString i a

        UserTypeName n ->
            n


aggregateTypeToString : Int -> AggregateType -> String
aggregateTypeToString i at =
    case at of
        OptionalType t ->
            "optional<" ++ typeToString i t ++ ">"

        ArrayType Nothing t ->
            "[]" ++ typeToString i t

        ArrayType (Just l) t ->
            "[" ++ String.fromInt l ++ "]" ++ typeToString i t

        MapType f t ->
            "map[" ++ typeToString i f ++ "]" ++ typeToString i t

        UnionType members ->
            "(" ++ String.join " | " (List.map (unionMemberToString i) members) ++ ")"

        StructType fields ->
            let
                step f =
                    indent (i + 1) ++ fieldToString (i + 1) f ++ "\n"
            in
            "{\n" ++ String.concat (List.map step fields) ++ indent i ++ "}"


unionMemberToString : Int -> UnionMember -> String
unionMemberToString i (UnionMember t k) =
    case k of
        Nothing ->
            typeToString i t

        Just l ->
            typeToString i t ++ " = " ++ String.fromInt l


fieldToString : Int -> Field -> String
fieldToString i (Field name value) =
    name ++ " : " ++ typeToString i value


indent : Int -> String
indent n =
    String.repeat n "    "


typeToString : Int -> Type -> String
typeToString i t =
    case t of
        NonEnumType net ->
            nonEnumTypeToString i net

        EnumType et ->
            enumTypeToString et


primitiveTypeToString : PrimitiveType -> String
primitiveTypeToString p =
    case p of
        Int ->
            "int"

        I s ->
            "i" ++ String.fromInt s

        UInt ->
            "uint"

        U s ->
            "u" ++ String.fromInt s

        Float s ->
            "f" ++ String.fromInt s

        Bool ->
            "bool"

        String ->
            "string"

        Data Nothing ->
            "data"

        Data (Just l) ->
            "data<" ++ String.fromInt l ++ ">"

        Void ->
            "void"


optionToString : EnumValue -> String
optionToString (EnumValue name value) =
    case value of
        Nothing ->
            "    " ++ name

        Just v ->
            "    " ++ name ++ " = " ++ String.fromInt v
