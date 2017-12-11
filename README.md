cabal install split

dfas or something

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