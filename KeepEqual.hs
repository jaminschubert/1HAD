-- HAD http://lpaste.net/194934289094148096

import Data.List

keepEqual :: Eq a => [a] -> [a] -> [a]
keepEqual a b = map fst $ filter snd $ zip a $ zipWith (==) a b

