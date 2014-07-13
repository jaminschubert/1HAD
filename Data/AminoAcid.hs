module Data.AminoAcid where

{--

Continuation of the solution for sequencing amino acids
started with Data.Bases at http://lpaste.net/107069

This module contains quite a bit of commented-out false-starts.
I kept the comments in to show where and how I stumbled, feet
of clay, and all that, please let me know if you find these
failed attempts distracting, in which case I will remove them.

--}

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable

import Data.Char
import Data.Bases

data AcidName = A | C | D | E | F | G | H | I | K | L
              | M | N | P | Q | R | S | T | V | W | Y
   deriving (Eq, Ord, Show, Read)

{--
instance Read AcidName where
   readsPrec prec str = readParen False (\r -> [(name, rest) |
         (str, rest) <- lex r,
         let name = read $ map toUpper str]) str -- infinite loop, read-read
 --}

data AminoAcid = Acid AcidName (Set NucleotideTriple )
   deriving (Eq, Ord)

instance Show AminoAcid where
   show (Acid name triples) =
        (show name) ++ " : " ++ (show $ Set.toList triples)

instance Read AminoAcid where
   readsPrec prec str = readParen False (\r -> [(acid, rest) |
      (acidName, colonRemainder) <- lex r,
      (":", triplesEtc) <- lex colonRemainder,
      (triples, rest) <- readsPrec prec triplesEtc,
      let acid = Acid (read acidName) (Set.fromList triples)]) str

{-- So!

let triples = read "[GCT, GCC, GCA, GCG]" :: [NucleotideTriple ]
let acid = Acid H (Set.fromList triples)

acid ~> H : [GCA,GCT,GCC,GCG]

read (show acid) == acid is True

so now that we're pretty close, we have to write a parser function for the 
given file format.

 --}

parseAcidDefs :: FilePath -> IO [AminoAcid]
parseAcidDefs file = readFile file >>= return . map parseAcid . lines

parseAcid :: String -> AminoAcid
parseAcid line = let (name, colonrest) = head $ lex line
                     (colon, rest) = head $ lex colonrest
                     triples = parseTriples rest []
                 in  Acid (read $ map toUpper name) (Set.fromList triples)

parseTriples :: String -> [NucleotideTriple] -> [NucleotideTriple]
parseTriples str accum | str == "" = accum
                       | otherwise = let (trip, rest) = head $ lex str
                                     in  parseTriples rest (read trip : accum)

{--
-- so now that we have them parsed, we can write the file out as a list
-- of rules and then embed that list into this very file!

encodeAcidRule :: AminoAcid -> String
encodeAcidRule (Acid name triples) = "Acid " ++ (show name)
      ++ " (Set.fromList " 
      ++ showStringList (map encodeTriple $ Set.toList triples) ++ ")"

encodeTriple :: NucleotideTriple -> String
encodeTriple (Triple (a, b, c)) = "Triple (" ++ show a ++ ", "
         ++ show b ++ ", " ++ show c ++ ")"

showStringList :: [String] -> String
showStringList [] = "[]"
showStringList (h : t) = "[" ++ h ++ s' t
   where s' [] = "]"
         s' (h : t) = ", " ++ h ++ s' t

-- so, with the above functions we can encode the rules in the DSL as
-- a pure-Haskell list:

acidRules :: [AminoAcid]
acidRules = [
          Acid A (Set.fromList [Triple (G, C, A), Triple (G, C, T), Triple (G, C, C), Triple (G, C, G)]),
          Acid C (Set.fromList [Triple (T, G, T), Triple (T, G, C)]),
          Acid D (Set.fromList [Triple (G, A, T), Triple (G, A, C)]),
          Acid E (Set.fromList [Triple (G, A, A), Triple (G, A, G)]),
          Acid F (Set.fromList [Triple (T, T, T), Triple (T, T, C)]),
          Acid G (Set.fromList [Triple (G, G, A), Triple (G, G, T), Triple (G, G, C), Triple (G, G, G)]),
          Acid H (Set.fromList [Triple (C, A, T), Triple (C, A, C)]),
          Acid I (Set.fromList [Triple (A, T, A), Triple (A, T, T), Triple (A, T, C)]),
          Acid K (Set.fromList [Triple (A, A, A), Triple (A, A, G)]),
          Acid L (Set.fromList [Triple (T, T, A), Triple (T, T, G), Triple (C, T, A), Triple (C, T, T), Triple (C, T, C), Triple 
(C, T, G)]),
          Acid M (Set.fromList [Triple (A, T, G)]),
          Acid N (Set.fromList [Triple (A, A, T), Triple (A, A, C)]),
          Acid P (Set.fromList [Triple (C, C, A), Triple (C, C, T), Triple (C, C, C), Triple (C, C, G)]),
          Acid Q (Set.fromList [Triple (C, A, A), Triple (C, A, G)]),
          Acid R (Set.fromList [Triple (A, G, A), Triple (A, G, G), Triple (C, G, A), Triple (C, G, T), Triple (C, G, C), Triple 
(C, G, G)]),
          Acid S (Set.fromList [Triple (A, G, T), Triple (A, G, C), Triple (T, C, A), Triple (T, C, T), Triple (T, C, C), Triple 
(T, C, G)]),
          Acid T (Set.fromList [Triple (A, C, T), Triple (A, C, C), Triple (A, C, G)]),
          Acid V (Set.fromList [Triple (G, T, A), Triple (G, T, T), Triple (G, T, C), Triple (G, T, G)]),
          Acid W (Set.fromList [Triple (T, G, G)]),
          Acid Y (Set.fromList [Triple (T, A, T), Triple (T, A, C)])]

I did the above with: 

   parseAcidDefs "acids.defs" >>= 
                 mapM_ (putStrLn . (flip (++) ",") . ((++) "       ") .
                        encodeAcidRule)

 --}

{-- Okay, so all the above was a great idea until the Amino acids A, G, C, T
    name-clashed with the bases of the same name, so, sigh: punt for now
    and just take the 'parsing in the file'-route. :(
 --}

data AminoAcidDefinitions = Defs (Map AcidName AminoAcid)
                                 (Map NucleotideTriple AminoAcid)

aminoAcidDefinitions :: [AminoAcid] -> AminoAcidDefinitions
aminoAcidDefinitions rules = a' rules Map.empty Map.empty
   where a' [] names trips = Defs names trips
         a' (acid@(Acid name triples) : acids) names trips = -- HAH!
            a' acids (Map.insert name acid names) 
                     (addTrips (Set.toList triples) acid trips)
         addTrips [] _ t = t
         addTrips (trip : ts) acid@(Acid name _) t =
            case (Map.lookup trip t) of
               Nothing -> addTrips ts acid (Map.insert trip acid t)
               Just (Acid nm _) -> error ("duplicate key " ++ show trip
                    ++ " for " ++ show name ++ " and " ++ show nm)

-- Now with the above we can do the exerices:

acid :: NucleotideTriple -> AminoAcidDefinitions -> Maybe AminoAcid
acid trip (Defs _ map) = Map.lookup trip map

{--

As we already proved that there is at most one amino acid defined by
a nucleotide triple, we improve the type-signature of the acid function
to reflect this semideterminacy

 --}

{-- BLEH!
target :: String -> AminoAcidDefinitions -> [[NucleotideTriple]]
target aminoAcid (Defs names _) =
   let acidNames = map (read . return . toUpper) aminoAcid
   in  a' acidNames []
      where a' [] ans = ans
            a' (acid : acids) accum = a' acids (deck |< 

... instead of trying to eat an elephant whole, I'll break out
targeting functionally piece-wise

--}

targeted :: [AminoAcid] -> [[NucleotideTriple]]
targeted acid = map (\(Acid _ trips) -> Set.toList trips) acid

{--

but the function targeted is the wrong orientation we want M x N not N x M
so we have to flip/redistribute the triples.

 --}

retargeted :: [[NucleotideTriple]] -> [[NucleotideTriple]]
retargeted [] = [[]]
retargeted (trips : rest) =
   concat [ trips >>= \trip -> map ((:) trip) (retargeted rest) ]

-- resignaturized target to accept strings for acid-sequences:

target :: String -> AminoAcidDefinitions -> Maybe [[NucleotideTriple]]
target acid (Defs names _) = 
   let aminoAcidNames = map (read . return . toUpper) acid
       aminoAcids = for aminoAcidNames (flip Map.lookup names)
   in  aminoAcids >>= return . retargeted . targeted

go :: String -> IO (Maybe [[NucleotideTriple]])
go acid = 
   parseAcidDefs "acids.defs" >>= return . target acid . aminoAcidDefinitions

{--

go "kmspdw" ~> IO (Just list)

where list is 96 NucleotideTriple sequences, including the sequence:

[AAG,ATG,TCT,CCG,GAC,TGG]

which is the sought sequence in the original article

Cool beans!

Other samples:

go "abdd" ~> error [b is not an acid]

go "acddtci" ~> IO (Just [576 sequences])

 --}

{--

Notes while solving this exercise:

1 and 2. Given Data.Bases and Data.AminoAcid:

As we already proved that there is at most one amino acid defined by
a nucleotide triple, we improve the type-signature of the acid function
to reflect this semideterminacy

3. 4 * 4 * 4 == 64 

or 

let bases [A, G, T, C] 
in  length [Triple (a, b, c) | a <- bases, b <- bases, c <- bases]

4. solution provided by the go function or target function

 --}
