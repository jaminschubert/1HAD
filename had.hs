import Data.AminoAcid                 -- http://lpaste.net/107071
import Data.List

{--

Sorting lists by lengths.

This is the P28 problem from the P-99 problem set

Say we have the universe of amino acids from before, and a
pool of the following sequences: --}

pool :: [[AcidName]]
pool = [[A, C, D], [E, F], [G, H, I], [E, F], [K, L, M, N], [P, Q], [R]]

-- a) Write a function

cmpLen :: (Ord a) => [a] -> [a] -> Ordering
cmpLen a b
  | length a == length b = cmpEq a b
  | length a < length b = LT
  | length a > length b = GT

cmpEq :: (Ord a) => [a] -> [a] -> Ordering
cmpEq (a:as) (b:bs)
  | as == bs = EQ
  | a == b = cmpEq as bs
  | a < b = LT
  | a > b = GT            

lenSort :: Ord a => [[a]] -> [[a]]
lenSort xxs = sortBy cmpLen xxs

{--

such that child lists are arranged by length:
lenSort pool ~> [[R], [E,F], [E,F], [P,Q], [A,C,D], [G,H,I], [K,L,M,N]]

--}

-- b) write a function

freqSort :: (Ord a) => [[a]] -> [[a]]
freqSort xs = concat $ deepSort $ concatSubs $ groupByLen $ sortBy cmpLen $ groupByLen $ lenSort xs
  where
    groupByLen xs = groupBy (\a b -> length a == length b) xs
    concatSubs xxxs = map (\xxs -> concat xxs ) xxxs
    deepSort xxs = map (\xs -> sortBy cmpEq xs) xxs

{--

such that child lists are arranged by the frequency of their length:

freqSort pool ~> [[K,L,M,N], [R], [A,C,D], [G,H,I], [E,F], [E,F], [P,Q]]

Explanatory comment from the original problem:

"Note that in the above example, the first two lists in the result have 
length 4 and 1, both lengths appear just once. The third and forth list have 
length 3 which appears, there are two list of this length. And finally, the 
last three lists have length 2. This is the most frequent length."

That's it: the ultimate list problem from the P-99 problem set.

Question: what data type developed recently here would allow more-
efficient definitions of the above functions?

 --}
