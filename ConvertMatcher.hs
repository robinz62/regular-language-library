module ConvertMatcher where

import DFA
import NFA
import Regex

dfaToNFA :: DFA -> NFA
dfaToNFA = undefined

nfaToDFA :: NFA -> DFA
nfaToDFA = undefined

regexToNFA :: RegexA -> NFA
regexToNFA = undefined

nfaToRegex :: NFA -> RegexA
nfaToRegex = undefined

-- equality???