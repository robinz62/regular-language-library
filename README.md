Roger Zhang (rogerz)
Robin Zhang (rzha)

Order to read source files
- Types.hs: defines the datatypes (such as DFA, NFA, etc.) that are used
  in almost every other source file
- Matcher.hs: the class interface that defines what functions any instance
  of Matcher must support (DFA, NFA, and RegexA all implement this interface)
- DFA.hs: the DFA implementation for Matcher
- NFA.hs: the NFA implementation for Matcher
- Regex.hs: the Regex implementation for Matcher
- ConvertMatcher.hs: defines functions for converting between the three
  matchers (DFA, NFA, Regex)

Other source files
- Operations.hs: defines some functions used by other source files; reading
  this file is optional for understanding the project
- MatcherParsers.hs: // TODO
- Parser.hs: // TODO
- SampleMatchers.hs: some prebuilt DFAs, NFAs, and Regexs for testing
- Tests.hs: contains all tests, separated into sections

Compiling
- Compiling with "ghc --make Main.hs" should work, though it isn't the preferred
  method. Since the project is a library (no meaningful executables), it's
  better to simply ":load Test.hs" for demonstration purposes. This will also
  make most of the functions available to use in ghci.

Input Formats for the fromString function (in Matcher.hs)

DFA input format

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

NFA input format

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

Regex input format