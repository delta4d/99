import Prob
import Test.Hspec
import Data.Ord (comparing)
import Data.List (sort, sortBy, nub, group, transpose)
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "myLast" $ do
        it "returns last element of a list" $ do
            myLast [1, 2, 3, 4] `shouldBe` 4
            myLast "xyz" `shouldBe` 'z'

        it "throws an exception if used with an empty list" $ do
            evaluate (myLast []) `shouldThrow` anyException


    describe "myButLast" $ do
        it "returns last but one element of a list" $ do
            myButLast [1, 2, 3, 4] `shouldBe` 3
            myButLast ['a'..'z'] `shouldBe` 'y'

        it "throws an exception if list length < 2" $ do
            evaluate (myButLast []) `shouldThrow` anyException
            evaluate (myButLast [0]) `shouldThrow` anyException


    describe "elementAt" $ do
        it "finds kth element of a list" $ do
            elementAt [1, 2, 3] 2 `shouldBe` 2
            elementAt "haskell" 5 `shouldBe` 'e'

        it "throws an exception if index out of range" $ do
            evaluate (elementAt [] 0) `shouldThrow` anyException
            evaluate (elementAt [] 1) `shouldThrow` anyException


    describe "myLength" $ do
        it "returns the length of a list" $ do
            myLength [] `shouldBe` 0
            myLength "Hello, world!" `shouldBe` 13


    describe "myReverse" $ do
        it "reverses a list" $ do
            myReverse "A man, a plan, a canal, panama!" 
                `shouldBe` "!amanap ,lanac a ,nalp a ,nam A"
            myReverse [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]


    describe "isPalindrome" $ do
        it "finds out whether a list is a palindrome" $ do
            isPalindrome [1, 2, 3] `shouldBe` False
            isPalindrome "madamimadam" `shouldBe` True
            isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1] `shouldBe` True


    describe "flatten" $ do
        it "flatten a nested list structure" $ do
            flatten (Elem 5) `shouldBe` [5]
            flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
                `shouldBe` [1,2,3,4,5]
            flatten (List [] :: NestedList Int) `shouldBe` []


    describe "compress" $ do
        it "eliminate consecutive duplicates of list elements" $ do
            compress "aaaabccaadeeee" `shouldBe` "abcade"


    describe "pack" $ do
        it "pack consecutive duplicates of list elements" $ do
        pack "aaaabccaadeeee" `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"]


    describe "encode" $ do
        it "run-length encoding of a list" $ do
            encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

------------------------------------------------------------------------------

    describe "encodeModified" $ do
        it "modify result of p10" $ do
            encodeModified "aaaabccaadeeee" `shouldBe`
                [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']


    describe "decodeModified" $ do
        it "decode run-length encoded list" $ do
            decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',
                Multiple 2 'a',Single 'd',Multiple 4 'e'] `shouldBe` "aaaabccaadeeee"


    describe "encodeDirect" $ do
        it "run-length encoding of a list" $ do
            encodeDirect  "aaaabccaadeeee" `shouldBe`
                [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']


    describe "dupli" $ do
        it "duplicate the elemtnts of a list" $ do
            dupli [1, 2, 3] `shouldBe` [1, 1, 2, 2, 3, 3]


    describe "repli" $ do
        it "replicate the elements of a list with given times" $ do
            repli "abc" 3 `shouldBe` "aaabbbccc"


    describe "dropEvery" $ do
        it "drops every nth element of a list" $ do
            dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"


    describe "split" $ do
        it "split a list into two parts" $ do
            split "abcdefghik" 3 `shouldBe` ("abc", "defghik")


    describe "slice" $ do
        it "extracts a slice from a list" $ do
            slice "abcdefghik" 3 7 `shouldBe` "cdefg"


    describe "rotate" $ do
        it "rotates a list n places to the left" $ do
            rotate "abcdefgh" 3 `shouldBe` "defghabc"


    describe "removeAt" $ do
        it "removes the kth element from a list" $ do
            removeAt 2 "abcd" `shouldBe` ('b', "acd")

------------------------------------------------------------------------------

    describe "insertAt" $ do
        it "insert an element at a given position into a list" $ do
            insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"


    describe "range" $ do
        it "create a range list" $ do
            range 4 9 `shouldBe` [4, 5, 6, 7, 8, 9]


    describe "combinations" $ do
        it "generates the k combinations of n distinct objects" $ do
            combinations 3 "abcd" `shouldBe` ["abc", "abd", "acd", "bcd"]


    describe "group3" $ do
        it "divide 9 to 2,3,4" $ do
            length (group3 "abcdefghi") `shouldBe` 1260


    describe "group'" $ do
        it "generate all the dividings" $ do
            length (group' [2,3,4] "abcdefghi") `shouldBe` 1260
            length (group' [2,2,5] "abcdefghi") `shouldBe` 756


    describe "lsort" $ do
        it "sorts by length" $ do
            lsort ["abc","de","fgh","de","ijkl","mn","o"] `shouldBe`
                ["o","de","de","mn","abc","fgh","ijkl"]


    describe "lfsort" $ do
        it "sorts length frequency" $ do
            lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] `shouldBe`
                ["ijkl","o","abc","fgh","de","de","mn"]

------------------------------------------------------------------------------

    describe "isPrime" $ do
        it "determine whethera given integer is prime" $ do
            isPrime 1 `shouldBe` False
            isPrime 7 `shouldBe` True
            isPrime 8 `shouldBe` False


    describe "myGCD" $ do
        it "calc gcd of two integers" $ do
            [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9,3,3]


    describe "coprime" $ do
        it "determine whether two positive integers are coprime" $ do
            coprime 35 64 `shouldBe` True
            coprime 36 64 `shouldBe` False


    describe "totient" $ do
        it "calc Euler's totient func" $ do
            totient 10 `shouldBe` 4


    describe "primeFactors" $ do
        it "determine the prime factors of integer" $ do
            primeFactors 315 `shouldBe` [3, 3, 5, 7]


    describe "prime_factors_mult" $ do
        it "determine prime factors" $ do
            prime_factors_mult 315 `shouldBe` [(3,2), (5,1), (7,1)]


    describe "phi" $ do
        it "calc Euler phi" $ do
            phi 10 `shouldBe` 4


    describe "primesR" $ do
        it "list prime in range" $ do
            primesR 10 20 `shouldBe` [11, 13, 17, 19]


    describe "goldbach" $ do
        it "split into 2 primes sum" $ do
            let (a, b) = goldbach 28
            a + b `shouldBe` 28
            isPrime a `shouldBe` True
            isPrime b `shouldBe` True

------------------------------------------------------------------------------

    describe "table" $ do
        it "generates truth table" $ do
            table (\a b -> (and' a (or' a b))) `shouldBe`
                [[True, True, True],
                 [True, False, True],
                 [False, True, False],
                 [False, False, False]]


    describe "table2" $ do
        it "generates truth table" $ do
            table2 (\a b -> a `and'` (a `or'` not b)) `shouldBe`
                [[True, True, True],
                 [True, False, True],
                 [False, True, False],
                 [False, False, False]]


    describe "tablen" $ do
        it "generates truth table" $ do
            tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
                `shouldBe`[[True, True, True, True   ],
                           [True, True, False, True  ],
                           [True, False, True, True  ],
                           [True, False, False, True ],
                           [False, True, True, True  ],
                           [False, True, False, True ],
                           [False, False, True, True ],
                           [False, False, False, True]]


    describe "gray" $ do
        it "generates gray code" $ do
            sort (gray 3) `shouldBe` sort ["000","001","011","010","110","111","101","100"]


    describe "huffman" $ do
        it "generates huffman code" $ do
            huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] `shouldBe`
                [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

------------------------------------------------------------------------------

    describe "cbalTree" $ do
        it "construct completely blanced binary tree" $ do
            let t1 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))
            let t2 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty)
            let t3 = Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)
            let t4 = Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
            cbalTree 4 `shouldBe` [t1, t2, t3, t4]


    describe "symmetric" $ do
        it "judge if symmetric tree" $ do
            symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
            symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` True


    describe "construct" $ do
        it "constructs binary search tree" $ do
            construct [3, 2, 5, 7, 1] `shouldBe`
                Branch 3
                       (Branch 2 (Branch 1 Empty Empty) Empty)
                       (Branch 5 Empty (Branch 7 Empty Empty))


    describe "symCbalTrees" $ do
        it "generate symmetric, completely balanced tree" $ do
            symCbalTrees 5 `shouldBe`
                [Branch 'x'
                        (Branch 'x' Empty (Branch 'x' Empty Empty))
                        (Branch 'x' (Branch 'x' Empty Empty) Empty),
                 Branch 'x'
                        (Branch 'x' (Branch 'x' Empty Empty) Empty)
                        (Branch 'x' Empty (Branch 'x' Empty Empty))]


    -- describe "hbalTree" $ do
    --     it "construct height-balanced binary trees" $ do
    --         (take 4 $ hbalTree 'x' 3) `shouldBe`
    --             [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
    --              Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
    --              Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
    --              Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]


    -- describe "hbalTreeNodes" $ do
    --     it "construct height-balanced binary trees with a given number of nodes" $ do
    --         (length $ hbalTreeNodes 'x' 15) `shouldBe` 1553
    --         map (hbalTreeNodes 'x') [0..3] `shouldBe`
    --             [[Empty],
    --              [Branch 'x' Empty Empty],
    --              [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
    --              [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]

------------------------------------------------------------------------------

    describe "countLeaves" $ do
        it "counts leaves" $ do
            countLeaves tree4 `shouldBe` 2


    describe "leaves" $ do
        it "collects leaves" $ do
            sort (leaves tree4) `shouldBe` [2, 4]


    describe "internals" $ do
        it "collect internal nodes" $ do
            sort (internals tree4) `shouldBe` [1, 2]


    describe "atLevel" $ do
        it "collect nodes at given level" $ do
            sort (atLevel tree4 2) `shouldBe` [2, 2]


    describe "completeBinaryTree" $ do
        it "construct complete binary tree" $ do
            isCompleteBinaryTree (completeBinaryTree 4) `shouldBe` True

    describe "layout64" $ do
        it "set layout to (inorder, depth)" $ do
            let tree64 = Branch 'n'
                                (Branch 'k'
                                        (Branch 'c'
                                                (Branch 'a' Empty Empty)
                                                (Branch 'h'
                                                        (Branch 'g'
                                                                (Branch 'e' Empty Empty)
                                                                Empty
                                                        )
                                                        Empty
                                                )
                                        )
                                        (Branch 'm' Empty Empty)
                                )
                                (Branch 'u'
                                        (Branch 'p'
                                                Empty
                                                (Branch 's'
                                                        (Branch 'q' Empty Empty)
                                                        Empty
                                                )
                                        )
                                        Empty
                                )
            layout64 tree64 `shouldBe`
                Branch ('n',(8,1))
                       (Branch ('k',(6,2))
                               (Branch ('c',(2,3))
                                       (Branch ('a',(1,4)) Empty Empty)
                                       (Branch ('h',(3,4))
                                               (Branch ('g',(2,5))
                                                       (Branch ('e',(1,6)) Empty Empty)
                                                       Empty)
                                               Empty
                                       )
                               )
                               (Branch ('m',(1,3)) Empty Empty)
                       )
                       (Branch ('u',(4,2))
                               (Branch ('p',(1,3))
                                       Empty
                                       (Branch ('s',(2,4))
                                               (Branch ('q',(1,5)) Empty Empty)
                                               Empty
                                       )
                               )
                               Empty
                       )


    describe "layout65" $ do
        it "sets layout to (equal distance, depth)" $ do
            let tree65 = Branch 'n'
                                (Branch 'k'
                                        (Branch 'c'
                                                (Branch 'a' Empty Empty)
                                                (Branch 'e'
                                                        (Branch 'd' Empty Empty)
                                                        (Branch 'g' Empty Empty)
                                                )
                                        )
                                        (Branch 'm' Empty Empty)
                                )
                                (Branch 'u'
                                        (Branch 'p'
                                                Empty
                                                (Branch 'q' Empty Empty)
                                        )
                                        Empty
                                )
            layout65 tree65 `shouldBe`
                         Branch ('n', (15, 1))
                                (Branch ('k', (7, 2))
                                        (Branch ('c', (3, 3))
                                                (Branch ('a', (1, 4)) Empty Empty)
                                                (Branch ('e', (5, 4))
                                                        (Branch ('d', (4, 5)) Empty Empty)
                                                        (Branch ('g', (6, 5)) Empty Empty)
                                                )
                                        )
                                        (Branch ('m', (11, 3)) Empty Empty)
                                )
                                (Branch ('u', (23, 2))
                                        (Branch ('p', (19, 3))
                                                Empty
                                                (Branch ('q', (21, 4)) Empty Empty)
                                        )
                                        Empty
                                )


    describe "layout66" $ do
        it "sets layout to (h, d)" $ do
            let tree66 = Branch 'n'
                                (Branch 'k'
                                        (Branch 'c'
                                                (Branch 'a' Empty Empty)
                                                (Branch 'e'
                                                        (Branch 'd' Empty Empty)
                                                        (Branch 'g' Empty Empty)
                                                )
                                        )
                                        (Branch 'm' Empty Empty)
                                )
                                (Branch 'u'
                                        (Branch 'p'
                                                Empty
                                                (Branch 'q' Empty Empty)
                                        )
                                        Empty
                                )
            layout66 tree66 `shouldBe`
                        Branch ('n', (5, 1))
                                (Branch ('k', (3, 2))
                                        (Branch ('c', (2, 3))
                                                (Branch ('a', (1, 4)) Empty Empty)
                                                (Branch ('e', (3, 4))
                                                        (Branch ('d', (2, 5)) Empty Empty)
                                                        (Branch ('g', (4, 5)) Empty Empty)
                                                )
                                        )
                                        (Branch ('m', (4, 3)) Empty Empty)
                                )
                                (Branch ('u', (7, 2))
                                        (Branch ('p', (6, 3))
                                                Empty
                                                (Branch ('q', (7, 4)) Empty Empty)
                                        )
                                        Empty
                                )


    describe "stringToTree,treeToString" $ do
        it "converts between string and tree" $ do
            treeToString (stringToTree "a(b(d,e),c(,f(g,)))") `shouldBe` "a(b(d,e),c(,f(g,)))"


    describe "preInTree" $ do
        it "reconstruct tree with preorder and inorder" $ do
            let tree = stringToTree "a(b(d,e),c(,f(g,)))"
            preInTree (treeToPreorder tree) (treeToInorder tree) `shouldBe` tree


    describe "ds2tree,tree2ds" $ do
        it "converts between dotstring and tree" $ do
            let tree = ds2tree "abd..e..c.fg..."
            tree2ds tree `shouldBe` "abd..e..c.fg..."

------------------------------------------------------------------------------

    describe "nnodes" $ do
        it "counts nodes" $ do
            nnodes mtree2 `shouldBe` 2


    describe "stringToMtree,mtreeToString" $ do
        it "converts between string and mtree" $ do
            let s = "afg^^c^bd^e^^^"
            mtreeToString (stringToMtree s) `shouldBe` s


    describe "ipl" $ do
        it "determine the internal path length" $ do
            ipl mtree4 `shouldBe` 2
            ipl mtree5 `shouldBe` 9


    describe "bottom_up" $ do
        it "construct the bottom-up order sequence of mtree" $ do
            bottom_up mtree5 `shouldBe` "gfcdeba"


    describe "display_lisp" $ do
        it "display tree lispy repr" $ do
            display_lisp mtree1 `shouldBe` "a"
            display_lisp mtree2 `shouldBe` "(a b)"
            display_lisp mtree3 `shouldBe` "(a (b c))"
            display_lisp mtree4 `shouldBe` "(b d e)"
            display_lisp mtree5 `shouldBe` "(a (f g) c (b d e))"

------------------------------------------------------------------------------

    describe "paths" $ do
        it "finds path in graph" $ do
            paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe` []
            paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe` [[1,2,3,4],[1,3,4]]


    describe "cycles" $ do
        it "find cycles" $ do
            cycles 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe` [[2,3,4,2]]
            cycles 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)] `shouldBe` []


    describe "spantree" $ do
        it "find all spantrees of graph" $ do
            let k4 = Graph "abcd"
                     [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]
            length (spantree k4) `shouldBe` 16


    describe "prim" $ do
        it "constructs minimum spanning tree" $ do
            let sumDis = sum . map (\(_, _, x) -> x)
            sumDis (prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)])
                `shouldBe` sumDis [(1,2,12),(1,3,34),(2,4,55),(2,5,32)]


    describe "iso" $ do
        it "judge graph isomorphism" $ do
            let g1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
            let g2 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
            iso g1 g2 `shouldBe` True

            let g3 = Graph [1,2,3,4] [(1,2), (2,3), (3,1)]
            let g4 = Graph [1,2,3,4] [(1,2), (2,3), (3,4)]
            iso g3 g4 `shouldBe` False


    describe "kcolor" $ do
        it "colors graph" $ do
            let g = Graph ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
            kcolor g `shouldBe` [('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]


    describe "depthfirst" $ do
        it "dfs graph" $ do
            let g = ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
            depthfirst g 1 `shouldBe` [1, 2, 3, 4, 5]


    describe "connectedcomponents" $ do
        it "finds all components" $ do
            let g = ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
            connectedcomponents g `shouldBe` [[1,2,3,4,5], [6,7]]


    describe "bipartite" $ do
        it "judge if bipartite" $ do
            let g1 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]
            let g2 = Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)]
            bipartite g1 `shouldBe` True 
            bipartite g2 `shouldBe` False

------------------------------------------------------------------------------

    describe "queens" $ do
        it "solves queens problem" $ do
            length (queens 8) `shouldBe` 92


    describe "knights" $ do
        let valid_move = \(a, b) (c, d) -> abs (a-c) + abs (b-d) == 3 && a /= c
        let check = \x n -> length x == n * n && _c n x where
                        _c n [] = True
                        _c n [_] = True
                        _c n (x@(a,b):xs@(y@(c,d):_))
                            | 1 <= a && a <= n
                              && 1 <= b && b <= n
                              && valid_move x y = _c n xs
                            | otherwise = False

        it "knightsTo" $ do
            check (head $ knightsTo 8 (1, 1)) 8 `shouldBe` True

        it "closedKnights" $ do
            let g = head $ closedKnights 8
            valid_move (head g) (last g) && check g 8 `shouldBe` True


    describe "vonKoch" $ do
        it "solves vonKoch problem" $ do
            let a = [(1, 7), (7, 2), (7, 3), (5, 3), (5, 4), (3, 6)]
            let b = head $ vonKoch a
            (length . nub . sort . map (\(x, y) -> abs (b!!(x-1) - b!!(y-1))) $ a)
                `shouldBe` length a


    -- describe "regular" $ do
    --     it "generate k-regular simple graphs with n nodes" $ do
    --         length (regular 6 3) `shouldBe` 2

------------------------------------------------------------------------------

    describe "fullWords" $ do
        it "convert number to full words repr" $ do
            fullWords 175 `shouldBe` "one-seven-five"


    describe "identifier" $ do
        it "checks ada identifier syntax" $ do
            identifier "this-is-a-long-identifier" `shouldBe` True
            identifier "this-ends-in-" `shouldBe` False
            identifier "two--hyphens" `shouldBe` False


    describe "nanogram" $ do
        it "solves nanogram" $ do
            let row  = [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]
                col  = [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
                res  = lines . head $ nonogram row col
                nano = map (filter (> 0) .  map (\g -> if head g == 'X' then length g else 0) . group)
            row == nano res && col == nano (transpose res) `shouldBe` True


    describe "crossword" $ do
        it "solves crossword" $ do
            let expected = map (sortBy (comparing fst)) [[((3,1),'A'),((3,2),'L'),
                            ((3,3),'P'),((3,4),'H'),((3,5),'A'),((1,3),'P'),((2,3),'O'),
                            ((4,3),'P'),((5,3),'Y'),((4,5),'R'),((5,5),'E'),((6,5),'S')]]
            let result = solve99 "ALPHA\nARES\nPOPPY\n\n  .  \n  .  \n.....\n  . .\n  . .\n    .\n"
            map (sortBy (comparing fst)) result `shouldBe` expected

------------------------------------------------------------------------------

-- vim:ts=4:et
