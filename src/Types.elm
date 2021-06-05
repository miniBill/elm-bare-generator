module Types exposing
    ( AggregateType(..)
    , EnumType
    , EnumValue(..)
    , Field(..)
    , Model
    , NonEnumType(..)
    , PrimitiveType(..)
    , Type(..)
    , UnionMember(..)
    , UserType(..)
    )


type UserType
    = Type String NonEnumType
    | Enum String EnumType


type NonEnumType
    = PrimitiveType PrimitiveType
    | AggregateType AggregateType
    | UserTypeName String


type PrimitiveType
    = Int
    | I Int
    | UInt
    | U Int
    | Float Int
    | Bool
    | String
    | Data (Maybe Int)
    | Void


type AggregateType
    = OptionalType Type
    | ArrayType (Maybe Int) Type
    | MapType Type Type
    | UnionType (List UnionMember)
    | StructType (List Field)


type Type
    = NonEnumType NonEnumType
    | EnumType EnumType


type alias EnumType =
    List EnumValue


type EnumValue
    = EnumValue String (Maybe Int)


type UnionMember
    = UnionMember Type (Maybe Int)


type Field
    = Field String Type


type alias Model =
    { schema : String
    , moduleName : String
    , outputFile : String
    }
