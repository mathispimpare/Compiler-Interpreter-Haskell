module Token where

data Token =
    -- Literals
    TInt Int
    | TFloat Double
    | TChar Char
    | TString String
    | TBool Bool
    | TNewLine

    -- Identifiers and Keywords
    | TIdentifier String
    | TEqual
    | TEqualComp     -- Equal comparison (==)
    | TPrint

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
    | TComment String
    deriving (Show, Eq)