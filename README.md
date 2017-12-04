cabal install split

dfas or something

DFA input format

--begin string--
n (e.g. 10)
alphabet (e.g. abcdefghijklmnopqrstuvwxyz)
0 a 1
0 b 2
0 c 1
1 a 1
...
9 z 9
q0
space-separated final states

NFA input format
--begin string--
n (e.g. 10)
alphabet
m (number of specified transitions)
0 a 0 1 2
0 b 0 2 4
0 c 3 4 5
...
9 z 9 8 6
0 ep 0 1 2
0 
q0
space-separated final states