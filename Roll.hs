{--

roll rotates list n steps to the left.

roll [1,2,3] 1 ~> [2,3,1]

The argument n can be negative, indicating that the rotation is to occur
to the right, then, instead.

roll [1,2,3] (-1) ~> [3,1,2]

(Recall that we write '(-1)' and not just '-1' because '-1' is interpreted
as the binary operator (-) applied to the Int 1)

The list can be rotated multiple times, so:

roll [1,2,3] 9 ~> [1,2,3]

Have at it!

Explanation: the term ROLL comes from the FORTH programming language,
placing the n-th element of the stack on TOS ('top of stack').

--}

import Data.List
import Debug.Trace
--import Test.QuickCheck

roll:: [a] -> Int -> [a]
roll [] _ = []
roll ys@(x:xs) i
  | i == 0 = ys
  | abs i > length ys && i > 0 = roll ys $ i `rem` (length ys)
  | i > 0 = roll xs (i - 1) ++ [x]
  | abs i > length ys && i < 0 = roll ys $ (-i) `rem` (length ys)
  | i < 0 = roll ys (i + length ys)

