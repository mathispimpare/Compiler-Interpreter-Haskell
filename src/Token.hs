module Token where

data Token =
    -- Literals
    TInt Int
    | TFloat Float
    | TChar Char
    | TString String
    | TNewLine
    | TIdentifier String
    | TPrint
    
    -- Arithmetic Operators
    | TPlus
    | TMinus
    | TTimes
    | TDiv
    | TFact

    -- Punctuation
    | TLParen
    | TRParen
    | TComma
    | TPoint
    | TSemicolon
    deriving Show