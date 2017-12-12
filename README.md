Robin Zhang (rzha)

Roger Zhang (rogerz)

## Order to read source files
- Types.hs: defines the datatypes (such as DFA, NFA, etc.) that are used
  in almost every other source file
- Matcher.hs: the class interface that defines what functions any instance
  of Matcher must support (DFA, NFA, and RegexA all implement this interface)
- DFA.hs: the DFA implementation for Matcher
- NFA.hs: the NFA implementation for Matcher
- Regex.hs: the Regex implementation for Matcher
- ConvertMatcher.hs: defines functions for converting between the three
  matchers (DFA, NFA, Regex)

## Other source files
- Operations.hs: defines some functions used by other source files; reading
  this file is optional for understanding the project
- MatcherParsers.hs: defines parsers to specifically parse our DFAs, NFAs, and
  Regexes from strings
- Parser.hs: contains definition of Parser. Similar to file used in class
- SampleMatchers.hs: some prebuilt DFAs, NFAs, and Regexs for testing
- Tests.hs: contains all tests, separated into sections

## Compiling
- Our project is a library that provides useful operations, so it doesn't
  really have a main executable. Compiling with "ghc --make Main.hs" will
  create an executable that simply runs all the test cases. Loading Tests.hs
  in ghci will let you play around with the available functions.

## Input Formats for the fromString function (in Matcher.hs)

DFA input format (each line is terminated by a newline character, last line
should not have a newline character)

```
DFA
N {number of states}
A [{characters in alphabet}]
TRANSITION
{curr state} {character} {next state}
{curr state} {character} {next state}
{...}
{curr state} {character} {next state}
START {start state}
F {space separated final states}
```

Example:

```
DFA
N 2
A [ab]
TRANSITION
0 a 1
0 b 1
1 a 1
1 b 1
START 0
F 1
```

NFA input format (each line is terminated by a newline character, last line
should not have a newline character)

```
NFA
N {number of states}
A [{characters in alphabet}]
TRANSITION
{curr state} {character} {next state}
{curr state} {character} {next state}
{...}
{curr state} {character} {next state}
EP-TRANSITION
{curr state} {space separated next states}
{...}
{curr state} {space separated next states}
START {start state}
F {space separated final states}
```

Example:

```
NFA
N 3
A [abc]
TRANSITION
0 a 0 1 2
1 a 1 2
EP-TRANSITION
0 0 1 2
1 0 1 2
START 0
F 1 2
```

Regex input format

Any characters typed will be added to the alphabet. Available regex commands
include alternation (`|`), sequence (`.`), kleene-star (`*`), and parentheses
grouping. Sequencing with (`.`) is always required except when sequencing
individual characters (e.g. `abc`). In this case, note that this sequence will
have the highest precedence (i.e. `abc*` is equivalent to `(abc)*`).

Examples:

```
a
a|b
(a|b)*
(ab|cd)*.(xyz)*
a|b|c|d
```