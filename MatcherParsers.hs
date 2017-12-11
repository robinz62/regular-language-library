module MatcherParsers (dfaP,
                       nfaP,
                       regexP,
                       regexReservedChars) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Parser
import Types

-----------------------------
-- Regex-related functions --
-----------------------------

-- | Parses a regular expression. Precedence from high to low is parentheses,
--   kleene-star, sequence, then alternation
--   Note that sequence without the (.) for strings is highest precedence. For
--   example, ab* will be parsed as (ab)*
regexP :: Parser (Regex Char)
regexP = altE

regexReservedChars :: [Char]
regexReservedChars = ['|', '.', '(', ')', '*']

regexReserved :: Char -> Bool
regexReserved c = c `elem` regexReservedChars

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

-----------------------------------
-- DFA and NFA-related functions --
-----------------------------------

-- | Parses a DFA from a string. Details on required format can be found in
--   the README document
dfaP :: Parser (DFA Char)
dfaP = D <$> (mkDFA <$> (dfaTitleP *> statesP)
                    <*> alphaP
                    <*> transitionsP
                    <*> startP
                    <*> finalP)

-- | Parses an NFA from a string. Details on required format can be found in
--   the README document
nfaP :: Parser (NFA Char)
nfaP = N <$> (mkNFA <$> (nfaTitleP *> statesP)
                    <*> alphaP
                    <*> nfaTransitionsP
                    <*> epTransitionsP
                    <*> startP
                    <*> finalP)

mkDFA :: a -> b -> c -> d -> e -> (a, b, c, d, e)
mkDFA a b c d e = (a, b, c, d, e)

mkNFA :: a -> b -> c -> d -> e -> f -> (a, b, (c, d), e, f)
mkNFA a b c d e f = (a, b, (c, d), e, f)

dfaTitleP :: Parser String
dfaTitleP = string "DFA\n"

nfaTitleP :: Parser String
nfaTitleP = string "NFA\n"

-- | Parses the number of states and converts to a Set of Nodes
--   For example, "N 5\n" becomes { 0, 1, 2, 3, 4 }
statesP :: Parser (Set Node)
statesP = Set.fromList <$> (
  (\n -> [0..(n-1)]) <$> (string "N " *> oneNat <* char '\n')
  )

-- | Parses the alphabet and converts to a Set of Chars
--   For example, "A [abcde]\n" becomes { a, b, c, d, e }
--   This does mean that the alphabet may not contain the character ']'
alphaP :: Parser (Set Char)
alphaP = Set.fromList <$> (
  string "A [" *> some (satisfy (/=']')) <* string "]\n"
  )

-- | Parses lines of DFA transition functions, including a header line
--   For example, "TRANSITION\n0 a 1\n0 b 1\n" becomes the transitions
--   ((0, a), 1) and ((0, b), 1)
transitionsP :: Parser (Map (Node, Char) Node)
transitionsP = Map.fromList <$> (string "TRANSITION\n" *> some trans) where
  trans = mkTrans <$> (oneNat <* char ' ')
                  <*> (get <* char ' ')
                  <*> oneNat
                  <*  char '\n'
  mkTrans a b c = ((a, b), c)

-- | Parsers lines of NFA transition functions, including a header line
--   For example, "TRANSITION\n0 a 1 2 3\n" becomes the transition
--   ((0, a), { 1, 2, 3})
nfaTransitionsP :: Parser (Map (Node, Char) (Set Node))
nfaTransitionsP = Map.fromList <$> (string "TRANSITION\n" *> some trans) where
  trans = mkTrans <$> (oneNat <* char ' ')
                  <*> get
                  <*> some (char ' ' *> oneNat)
                  <*  char '\n'
  mkTrans a b c = ((a, b), Set.fromList c)

-- | Parses lines of NFA epsilon transitions, including a header line
--   For example, "EP-TRANSITION\n0 1 2 3\n1 1 2 3" becomes the transitions
--   (0, {1, 2, 3}) and (1, {1, 2, 3})
epTransitionsP :: Parser (Map Node (Set Node))
epTransitionsP = Map.fromList <$> (string "EP-TRANSITION\n" *> some trans) where
  trans = mkTrans <$> oneNat
                  <*> some (char ' ' *> oneNat)
                  <*  char '\n'
  mkTrans a b = (a, Set.fromList b)

-- | Parses the start state
--   For example, "START 0\n" returns 0
startP :: Parser Node
startP = string "START " *> oneNat <* char '\n'

-- | Parses a list of final states
--   For example, "F 0 1 2" becomes the set { 0, 1, 2 }
finalP :: Parser (Set Node)
finalP = Set.fromList <$> (char 'F' *> many (char ' ' *> oneNat))