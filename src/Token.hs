module Token where

data Token =
    -- Literals
    TInt Int
    | TFloat Double
    | TString String
    | TBool Bool
    | TNewLine

    -- Identifiers and Keywords
    | TIdentifier String
    | TEqual
    | TIf
    | TPrint
    -- Comparisons
    | TEqualComp     -- Equal comparison (==)
    | TGt
    | TGtEq
    | TLt
    | TLtEq

    -- Arithmetic Operators
    | TPlus
    | TSub
    | TMul
    | TDiv
    | TFac

    -- Punctuation
    | TLParen
    | TRParen
    | TComma
    deriving (Show, Eq)