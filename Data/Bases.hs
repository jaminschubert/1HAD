module Data.Bases where

{--

The below solution for the exercise, http://lpaste.net/107023,
that was (more than) a bit of a challenge is expressed in two 
separate modules: Data.Bases and Data.AminoAcid. We present both 
in two lpaste files, the first one has a link to the second at 
the bottom of the file as a comment.

--}

import Data.Char

data Base = A | T | C | G
   deriving (Eq, Ord, Show, Read)

data NucleotideTriple = Triple (Base, Base, Base)
   deriving (Eq, Ord)

{-- 

For the read instance of a triple, we will specialize on the formatting
we are give which is, e.g.: "tgg"

 --}

instance Show NucleotideTriple where
   show (Triple (a, b, c)) = map (head . show) [a, b, c]

mkTrip :: [Base] -> NucleotideTriple
mkTrip [a,b,c] = Triple (a,b,c)

instance Read NucleotideTriple where
   readsPrec prec str = readParen False 
     (\r -> [(mkTrip bases, rest) |
                (trechars, rest) <- lex r,
                let bases = map (read . return . toUpper) trechars]) str

-- so, let triple = Triple (T, G, G) in read (show triple) == triple is True

-- Data.AminoAcid, the continuation to the solution, is located at 
-- http://lpaste.net/107071
