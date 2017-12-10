module MatcherParsers (regexP) where

import Control.Applicative
import qualified Data.Set as Set

import Parser
import Types

-- | Parses a regular expression. Precedence from high to low is parentheses,
--   kleene-star, sequence, then alternation
--   Note that sequence without the (.) for strings is highest precedence. For
--   example, ab* will be parsed as (ab)*
regexP :: Parser (Regex Char)
regexP = altE

regexReserved :: Char -> Bool
regexReserved c = c `elem` ['|', '.', '(', ')', '*']

-- | Parses a raw string (lacking any operators) as a regular expression.
regexStringP :: Parser (Regex Char)
regexStringP = seqString <$> some (satisfy (not . regexReserved)) where
  seqString :: String -> Regex Char
  seqString [] = Empty
  seqString [x] = (Single . Set.singleton) x
  seqString (x:xs) = Seq (seqString [x]) (seqString xs)

-- | Parses the kleene-star when '*' is placed directly after a literal string
starLiteralP :: Parser (Regex Char)
starLiteralP = Star <$> regexStringP <* char '*'

-- | Parses the kleene-star when '*' is placed following a regular expression
--   enclosed in parentheses
starRegexP :: Parser (Regex Char)
starRegexP = Star <$> (parenP '(' altE ')') <* char '*'

-- | Parses the alternation character into an operator
altOp :: Parser (Regex Char -> Regex Char -> Regex Char)
altOp = char '|' *> pure Alt

-- | Parses the sequence character into an operator
seqOp :: Parser (Regex Char -> Regex Char -> Regex Char)
seqOp = char '.' *> pure Seq

-- | A left-associative alternation of sequences
altE :: Parser (Regex Char)
altE = chainl seqE altOp

-- | A left-associative sequence of basic regular expressions
seqE :: Parser (Regex Char)
seqE = chainl textE seqOp

-- | A basic regular expression: either an entire other regular expression
--   wrapped in parentheses, the kleene-star of a regular expression, or just
--   a basic string
textE :: Parser (Regex Char)
textE = starRegexP <|> parenP '(' altE ')' <|> starLiteralP <|> regexStringP

-- | chainl p pop parses one or more occurrences of p, separated by op.
--   Returns a value produced by a left associative application of all
--   functions returned by op.
chainl :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl p pop = foldl comb <$> p <*> rest where
  comb = \x (op, y) -> op x y
  rest = many ((,) <$> pop <*> p)

parenP :: Char -> Parser b -> Char -> Parser b
parenP open p close = char open *> p <* char close