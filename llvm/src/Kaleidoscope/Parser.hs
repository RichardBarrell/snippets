module Kaleidoscope.Parser where

import Text.Parsec
import Control.Applicative ((<$>), (<*>))

data Kt
    = KtDef
    | KtExtern
    | KtId String
    | KtNum Double
    deriving (Show, Eq)

ktokens :: Parsec String () [Kt]
ktokens = tokens where
    tokens = spaces >> (end <|> more)
    more = (:) <$> (numToken <|> wordToken) <*> tokens
    end = eof >> return []

wordToken = do word <- many1 (noneOf " \t\r\n\f\v")
               case word of "def"    -> return KtDef
                            "extern" -> return KtExtern
                            _ -> return (KtId word)

numToken = readNumToken <$> many1 digit
                        <*> numAfterPt
                        <*> numExponent
           where
           readNumToken a b c = KtNum $ read (a ++ b ++ c)
           numAfterPt = do char '.'
                           digits <- many1 digit
                           return $ '.' : digits
                        <|> return ""
           numExponent = do oneOf "eE"
                            plusMinus <- fmap (:"") (oneOf "+-") <|> return ""
                            digits <- many1 digit
                            return $ 'e' : plusMinus ++ digits
                         <|> return ""

data Kast
    = KaExp Kexp

data Kexp
    = KeNum Double
    | KeVar String
    | KeBin Kop Kexp Kexp
    | KeCall String 
    | KaVar String
    | KaBinExp Kop Kexp

data Kop
    = KoAdd
    | KoSub
    | KoMul
    | KoDiv

op_to_kop '+' = Just KoAdd
op_to_kop '-' = Just KoSub
op_to_kop '*' = Just KoMul
op_to_kop '/' = Just KoDiv
op_to_kop _ = Nothing
