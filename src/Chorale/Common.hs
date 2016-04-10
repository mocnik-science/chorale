-----------------------------------------------------------------------------
-- |
-- Module      :  Chorale.Common
-- Copyright   :  2013-2016 Franz-Benjamin Mocnik
-- License     :  MIT
--
-- Maintainer  :  mail@mocnik-science.net
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module Chorale.Common (
    -- * Applicative
    (.*),
    (.**),
    (.***),
    curryM2,
    curryMM2,
    curry3,
    uncurry3,
    curryM3,
    curryMM3,
    curry4,
    uncurry4,
    curryM4,
    curryMM4,
    curry5,
    uncurry5,
    curryM5,
    curryMM5,
    -- * Tuples
    -- ** Generating Tuples
    appendFst,
    appendSnd,
    appendFst3,
    appendSnd3,
    appendThd3,
    removeFst3,
    removeSnd3,
    removeThd3,
    appendFst4,
    appendSnd4,
    appendThd4,
    appendFth4,
    removeFst4,
    removeSnd4,
    removeThd4,
    removeFth4,
    appendFst5,
    appendSnd5,
    appendThd5,
    appendFourth5,
    appendFifth5,
    removeFst5,
    removeSnd5,
    removeThd5,
    removeFourth5,
    removeFifth5,
    make2,
    make3,
    make4,
    make5,
    -- ** Retrieving the Tuples' Components
    fst3,
    snd3,
    thd3,
    fst4,
    snd4,
    thd4,
    fth4,
    fst5,
    snd5,
    thd5,
    fourth5,
    fifth5,
    -- ** Modifying Tuples
    tupleToList2,
    listToTuple2,
    tupleToList3,
    listToTuple3,
    tupleToList4,
    listToTuple4,
    tupleToList5,
    listToTuple5,
    -- ** Applying Functions to Tuples
    map12,
    map21,
    map22,
    map13,
    map31,
    map33,
    map14,
    map41,
    map44,
    map15,
    map51,
    map55,
    mapFst,
    mapSnd,
    -- * Monads and Tuples
    sequence2,
    sequence3,
    sequence4,
    sequence5,
    (<<),
    -- * Ordering
    compareUsing,
    -- * Comparing and Sorting
    vanishes,
    equaling,
    sortAndGroup,
    sortAndGroupBy,
    sortAndGroupLookupBy,
    -- * List Operations
    notNull,
    takeWhileList,
    takeUntilList,
    takeToFirst,
    splitOnFirst,
    nubOrd,
    nubOrdBy',
    zipWithDefault,
    subset,
    subsets,
    findIndicesTuples,
    replaceInList,
    replaceElementInList,
    removeFromList,
    stripPostfix,
    applyToList,
    mapFoldl,
    reverseMap,
    count,
    deleteAll,
    deleteAlls,
    cartesian,
    -- * Boolean Operations
    xor,
    xnor,
    -- * Number Operations
    average,
    -- * String Operations
    justifyLeft,
    justifyRight,
    -- * Maybe Operations
    mapJust,
    onJustUse,
    -- * Either Operations
    mapLeft,
    mapRight,
    fromLeft,
    fromRight,
    Either3(..)) where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set
import Safe

-- --== APPLICATIVE

-- | @f . g a $ b = (f .* g) a b@
infixr 8 .*
(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) = (.) . (.)

-- | @f . g a b $ c = (f .** g) a b c@
infixr 8 .**
(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) = (.) . (.*)

-- | @f . g a b c $ d = (f .** g) a b c d@
infixr 8 .***
(.***) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.***) = (.) . (.**)

-- | 'curry' for two monadic arguments
curryM2 :: (Functor m, Monad m) => (a -> b -> x) -> (m a, m b) -> m x
curryM2 f t = uncurry f <$> sequence2 t

-- | 'curry' for two monadic arguments with monadic function
curryMM2 :: (Functor m, Monad m) => (a -> b -> m x) -> (m a, m b) -> m x
curryMM2 = join .* curryM2

-- | 'curry' for three arguments
{-# INLINE curry3 #-}
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

-- | 'uncurry' for three arguments
{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | 'curry' for three monadic arguments
curryM3 :: (Functor m, Monad m) => (a -> b -> c -> x) -> (m a, m b, m c) -> m x
curryM3 f t = uncurry3 f <$> sequence3 t

-- | 'curry' for three monadic arguments with monadic function
curryMM3 :: (Functor m, Monad m) => (a -> b -> c -> m x) -> (m a, m b, m c) -> m x
curryMM3 = join .* curryM3

-- | 'curry' for four arguments
{-# INLINE curry4 #-}
curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a, b, c, d)

-- | 'uncurry' for four arguments
{-# INLINE uncurry4 #-}
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- | 'curry' for four monadic arguments
curryM4 :: (Functor m, Monad m) => (a -> b -> c -> d -> x) -> (m a, m b, m c, m d) -> m x
curryM4 f t = uncurry4 f <$> sequence4 t

-- | 'curry' for four monadic arguments with monadic function
curryMM4 :: (Functor m, Monad m) => (a -> b -> c -> d -> m x) -> (m a, m b, m c, m d) -> m x
curryMM4 = join .* curryM4

-- | 'curry' for five arguments
{-# INLINE curry5 #-}
curry5 :: ((a, b, c, d, e) -> f) -> a -> b -> c -> d -> e -> f
curry5 f a b c d e = f (a, b, c, d, e)

-- | 'uncurry' for five arguments
{-# INLINE uncurry5 #-}
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

-- | 'curry' for five monadic arguments
curryM5 :: (Functor m, Monad m) => (a -> b -> c -> d -> e -> x) -> (m a, m b, m c, m d, m e) -> m x
curryM5 f t = uncurry5 f <$> sequence5 t

-- | 'curry' for five monadic arguments with monadic function
curryMM5 :: (Functor m, Monad m) => (a -> b -> c -> d -> e -> m x) -> (m a, m b, m c, m d, m e) -> m x
curryMM5 = join .* curryM5

-- --== TUPLES
-- --== GENERATING TUPLES

-- | append a first argument for making a 2-tuple
appendFst :: a -> b -> (a, b)
appendFst a b = (a, b)

-- | append a second argument for making a 2-tuple
appendSnd :: b -> a -> (a, b)
appendSnd b a = (a, b)

-- | append a first argument for making a 3-tuple
appendFst3 :: a -> (b, c) -> (a, b, c)
appendFst3 a (b, c) = (a, b, c)

-- | append a second argument for making a 3-tuple
appendSnd3 :: b -> (a, c) -> (a, b, c)
appendSnd3 b (a, c) = (a, b, c)

-- | append a third argument for making a 3-tuple
appendThd3 :: c -> (a, b) -> (a, b, c)
appendThd3 c (a, b) = (a, b, c)

-- | remove first argument of a 3-tuple
removeFst3 :: (a, b, c) -> (b, c)
removeFst3 (_, b, c) = (b, c)

-- | remove second argument of a 3-tuple
removeSnd3 :: (a, b, c) -> (a, c)
removeSnd3 (a, _, c) = (a, c)

-- | remove third argument of a 3-tuple
removeThd3 :: (a, b, c) -> (a, b)
removeThd3 (a, b, _) = (a, b)

-- | append a first argument for making a 4-tuple
appendFst4 :: a -> (b, c, d) -> (a, b, c, d)
appendFst4 a (b, c, d) = (a, b, c, d)

-- | append a second argument for making a 4-tuple
appendSnd4 :: b -> (a, c, d) -> (a, b, c, d)
appendSnd4 b (a, c, d) = (a, b, c, d)

-- | append a third argument for making a 4-tuple
appendThd4 :: c -> (a, b, d) -> (a, b, c, d)
appendThd4 c (a, b, d) = (a, b, c, d)

-- | append a fourth argument for making a 4-tuple
appendFth4 :: d -> (a, b, c) -> (a, b, c, d)
appendFth4 d (a, b, c) = (a, b, c, d)

-- | remove first argument of a 4-tuple
removeFst4 :: (a, b, c, d) -> (b, c, d)
removeFst4 (_, b, c, d) = (b, c, d)

-- | remove second argument of a 4-tuple
removeSnd4 :: (a, b, c, d) -> (a, c, d)
removeSnd4 (a, _, c, d) = (a, c, d)

-- | remove third argument of a 4-tuple
removeThd4 :: (a, b, c, d) -> (a, b, d)
removeThd4 (a, b, _, d) = (a, b, d)

-- | remove fourth argument of a 4-tuple
removeFth4 :: (a, b, c, d) -> (a, b, c)
removeFth4 (a, b, c, _) = (a, b, c)

-- | append a first argument for making a 5-tuple
appendFst5 :: a -> (b, c, d, e) -> (a, b, c, d, e)
appendFst5 a (b, c, d, e) = (a, b, c, d, e)

-- | append a second argument for making a 5-tuple
appendSnd5 :: b -> (a, c, d, e) -> (a, b, c, d, e)
appendSnd5 b (a, c, d, e) = (a, b, c, d, e)

-- | append a third argument for making a 5-tuple
appendThd5 :: c -> (a, b, d, e) -> (a, b, c, d, e)
appendThd5 c (a, b, d, e) = (a, b, c, d, e)

-- | append a fourth argument for making a 5-tuple
appendFourth5 :: d -> (a, b, c, e) -> (a, b, c, d, e)
appendFourth5 d (a, b, c, e) = (a, b, c, d, e)

-- | append a fifth argument for making a 5-tuple
appendFifth5 :: e -> (a, b, c, d) -> (a, b, c, d, e)
appendFifth5 e (a, b, c, d) = (a, b, c, d, e)

-- | remove first argument of a 5-tuple
removeFst5 :: (a, b, c, d, e) -> (b, c, d, e)
removeFst5 (_, b, c, d, e) = (b, c, d, e)

-- | remove second argument of a 5-tuple
removeSnd5 :: (a, b, c, d, e) -> (a, c, d, e)
removeSnd5 (a, _, c, d, e) = (a, c, d, e)

-- | remove third argument of a 5-tuple
removeThd5 :: (a, b, c, d, e) -> (a, b, d, e)
removeThd5 (a, b, _, d, e) = (a, b, d, e)

-- | remove fourth argument of a 5-tuple
removeFourth5 :: (a, b, c, d, e) -> (a, b, c, e)
removeFourth5 (a, b, c, _, e) = (a, b, c, e)

-- | remove fifth argument of a 5-tuple
removeFifth5 :: (a, b, c, d, e) -> (a, b, c, d)
removeFifth5 (a, b, c, d, _) = (a, b, c, d)

-- | make a 2-tuple containing the given value in each component
make2 :: a -> (a, a)
make2 a = (a, a)

-- | make a 3-tuple containing the given value in each component
make3 :: a -> (a, a, a)
make3 a = (a, a, a)

-- | make a 4-tuple containing the given value in each component
make4 :: a -> (a, a, a, a)
make4 a = (a, a, a, a)

-- | make a 5-tuple containing the given value in each component
make5 :: a -> (a, a, a, a, a)
make5 a = (a, a, a, a, a)

-- --== RETRIEVE THE TUPLES' COMPONENTS

-- | get the first argument of a 3-tuple
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | get the second argument of a 3-tuple
snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

-- | get the third argument of a 3-tuple
thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- | get the first argument of a 4-tuple
fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

-- | get the second argument of a 4-tuple
snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

-- | get the third argument of a 4-tuple
thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c

-- | get the fourth argument of a 4-tuple
fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

-- | get the first argument of a 5-tuple
fst5 :: (a, b, c, d, e) -> a
fst5 (a, _, _, _, _) = a

-- | get the second argument of a 5-tuple
snd5 :: (a, b, c, d, e) -> b
snd5 (_, b, _, _, _) = b

-- | get the third argument of a 5-tuple
thd5 :: (a, b, c, d, e) -> c
thd5 (_, _, c, _, _) = c

-- | get the fourth argument of a 5-tuple
fourth5 :: (a, b, c, d, e) -> d
fourth5 (_, _, _, d, _) = d

-- | get the fourth argument of a 5-tuple
fifth5 :: (a, b, c, d, e) -> e
fifth5 (_, _, _, _, e) = e

-- --== MODIFY TUPLES

-- | convert a 2-tuple to a list
tupleToList2 :: (a, a) -> [a]
tupleToList2 (x, y) = [x, y]

-- | convert a 2-tuple to a list
listToTuple2 :: [a] -> Maybe (a, a)
listToTuple2 [x, y] = Just (x, y)
listToTuple2 _ = Nothing

-- | convert a 3-tuple to a list
tupleToList3 :: (a, a, a) -> [a]
tupleToList3 (x, y, z) = [x, y, z]

-- | convert a 3-tuple to a list
listToTuple3 :: [a] -> Maybe (a, a, a)
listToTuple3 [x, y, z] = Just (x, y, z)
listToTuple3 _ = Nothing

-- | convert a 4-tuple to a list
tupleToList4 :: (a, a, a, a) -> [a]
tupleToList4 (x1, x2, x3, x4) = [x1, x2, x3, x4]

-- | convert a 4-tuple to a list
listToTuple4 :: [a] -> Maybe (a, a, a, a)
listToTuple4 [x1, x2, x3, x4] = Just (x1, x2, x3, x4)
listToTuple4 _ = Nothing

-- | convert a 5-tuple to a list
tupleToList5 :: (a, a, a, a, a) -> [a]
tupleToList5 (x1, x2, x3, x4, x5) = [x1, x2, x3, x4, x5]

-- | convert a 5-tuple to a list
listToTuple5 :: [a] -> Maybe (a, a, a, a, a)
listToTuple5 [x1, x2, x3, x4, x5] = Just (x1, x2, x3, x4, x5)
listToTuple5 _ = Nothing

-- --== APPLYING FUNCTIONS TO TUPLES

-- | apply one function to a 2-tuple
map12 :: (a -> a') -> (a, a) -> (a', a')
map12 f (a0, a1) = (f a0, f a1)

-- | apply a 2-tuple of functions to a value
map21 :: (a -> a', a -> a'') -> a -> (a', a'')
map21 (f, g) a = (f a, g a)

-- | apply a 2-tuple of functions to a 2-tuple
map22 :: (a -> a', b -> b') -> (a, b) -> (a', b')
map22 (f, g) (a, b) = (f a, g b)

-- | apply one function to a 3-tuple
map13 :: (a -> a') -> (a, a, a) -> (a', a', a')
map13 f (a0, a1, a2) = (f a0, f a1, f a2)

-- | apply a 3-tuple of functions to a value
map31 :: (a -> a', a -> a'', a -> a''') -> a -> (a', a'', a''')
map31 (f, g, h) a = (f a, g a, h a)

-- | apply a 3-tuple of functions to a 3-tuple
map33 :: (a -> a', b -> b', c -> c') -> (a, b, c) -> (a', b', c')
map33 (f, g, h) (a, b, c) = (f a, g b, h c)

-- | apply one function to a 4-tuple
map14 :: (a -> a') -> (a, a, a, a) -> (a', a', a', a')
map14 f (a0, a1, a2, a3) = (f a0, f a1, f a2, f a3)

-- | apply a 4-tuple of functions to a value
map41 :: (a -> a', a -> a'', a -> a''', a -> a'''') -> a -> (a', a'', a''', a'''')
map41 (f, g, h, i) a = (f a, g a, h a, i a)

-- | apply a 4-tuple of functions to a 4-tuple
map44 :: (a -> a', b -> b', c -> c', d -> d') -> (a, b, c, d) -> (a', b', c', d')
map44 (f, g, h, i) (a, b, c, d) = (f a, g b, h c, i d)

-- | apply one function to a 5-tuple
map15 :: (a -> a') -> (a, a, a, a, a) -> (a', a', a', a', a')
map15 f (a0, a1, a2, a3, a4) = (f a0, f a1, f a2, f a3, f a4)

-- | apply a 5-tuple of functions to a value
map51 :: (a -> a', a -> a'', a -> a''', a -> a'''', a -> a''''') -> a -> (a', a'', a''', a'''', a''''')
map51 (f, g, h, i, j) a = (f a, g a, h a, i a, j a)

-- | apply a 5-tuple of functions to a 5-tuple
map55 :: (a -> a', b -> b', c -> c', d -> d', e -> e') -> (a, b, c, d,e ) -> (a', b', c', d', e')
map55 (f, g, h, i, j) (a, b, c, d, e) = (f a, g b, h c, i d, j e)

-- | apply a function to the first argument of a 2-tuple
mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f = map22 (f, id)

-- | apply a function to the second argument of a 2-tuple
mapSnd :: (b -> b') -> (a, b) -> (a, b')
mapSnd g = map22 (id, g)

-- --== MONADS AND TUPLES

-- | like 'sequence' but for a 2-tuple
sequence2 :: (Functor m, Monad m) => (m a, m b) -> m (a, b)
sequence2 (a, b) = do
    a' <- a
    b' <- b
    return (a', b')

-- | like 'sequence' but for a 3-tuple
sequence3 :: (Functor m, Monad m) => (m a, m b, m c) -> m (a, b, c)
sequence3 (a, b, c) = do
    a' <- a
    b' <- b
    c' <- c
    return (a', b', c')

-- | like 'sequence' but for a 4-tuple
sequence4 :: (Functor m, Monad m) => (m a, m b, m c, m d) -> m (a, b, c, d)
sequence4 (a, b, c, d) = do
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    return (a', b', c', d')

-- | like 'sequence' but for a 5-tuple
sequence5 :: (Functor m, Monad m) => (m a, m b, m c, m d, m e) -> m (a, b, c, d, e)
sequence5 (a, b, c, d, e) = do
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    return (a', b', c', d', e')

-- | like '>>' but with reversed argument
(<<) :: (Monad m) => m b -> m a -> m b
m1 << m2 = m2 >> m1

-- --== ORDERING

-- | compare function that uses the order in a given list
--
-- e.g. @compareUsing [1,3,2]@ will state @1 < 2@ and @3 < 2@
compareUsing :: Eq a => [a] -> a -> a -> Ordering
compareUsing as = uncurry compare .* curry (map12 (`elemIndex` as))

-- --== COMPARING AND SORTING

-- | tests whether a given number vanishes
vanishes :: (Num a, Eq a) => a -> Bool
vanishes = (==) 0

-- | similar to comparing but for equalities
equaling :: Eq b => (a -> b) -> a -> a -> Bool
equaling f x y = f x == f y

-- | sort and group
sortAndGroup :: Ord a => [a] -> [[a]]
sortAndGroup = sortAndGroupBy id

-- | sort and than group, non-overloaded version
sortAndGroupBy :: Ord b => (a -> b) -> [a] -> [[a]]
sortAndGroupBy f = groupBy (equaling f) . sortBy (comparing f)

-- | sort and than group to a lookup table
sortAndGroupLookupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
sortAndGroupLookupBy f = map (map21 (f . head, id)) . sortAndGroupBy f

-- --== LIST OPERATIONS

-- | returns 'False' if a list is empty, otherwise 'True'
notNull :: [a] -> Bool
notNull = not . null

-- | like 'takeWhile' but the function @p@ takes the whole list as argument
takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList p = takeWhileList' [] where
    takeWhileList' ys [] = ys
    takeWhileList' ys (x:xs)
        | p (ys ++ [x]) = takeWhileList' (ys ++ [x]) xs
        | otherwise = ys

-- | similar 'takeWhileList' but returns the the sublist such that @p@ is met the first time
takeUntilList :: ([a] -> Bool) -> [a] -> [a]
takeUntilList p = takeUntilList' [] where
    takeUntilList' ys [] = ys
    takeUntilList' ys (x:xs)
        | (not . p) (ys ++ [x]) = takeUntilList' (ys ++ [x]) xs
        | otherwise = ys ++ [x]

-- | 'takeToFirst' @p xs@ returns the suffix until (and inclusive) the first occurance where @p xs@
takeToFirst :: (a -> Bool) -> [a] -> [a]
takeToFirst _ [] = []
takeToFirst p (x:xs)
    | p x = [x]
    | otherwise = x : takeToFirst p xs

-- | like 'splitOn' but splits only at the first occurance
splitOnFirst :: Eq a => a -> [a] -> ([a], Maybe [a])
splitOnFirst x xs
    | isJust j = mapSnd (Just . tail) . splitAt (fromJust j) $ xs
    | otherwise = (xs, Nothing)
    where
        j = elemIndex x xs

-- | like 'nub' but requires 'a' to be an instance of 'Ord'
--
-- The original 'nub' is O(n^2) on lists of length n. 'nubOrd' is O(n log(n)).
nubOrd :: Ord a => [a] -> [a]
nubOrd = s Set.empty where
    s _ [] = []
    s m (x:xs)
        | Set.member x m = s m xs
        | otherwise = x : s (Set.insert x m) xs

-- | like 'nubBy' but requires 'b' to be an instance of 'Ord'
--
-- @nubOrdBy' f = nubBy (equaling f)@
-- The original 'nubBy' is O(n^2) on lists of length n. 'nubOrdBy'' is O(n log(n)).
nubOrdBy' :: Ord b => (a -> b) -> [a] -> [a]
nubOrdBy' f = s Map.empty where
    s _ [] = []
    s m (x:xs)
        | Map.member (f x) m = s m xs
        | otherwise = x : s (Map.insert (f x) x m) xs

-- | like 'zipWith' but with a default value such that the resulting list is as long as the longest input list
{-# NOINLINE [1] zipWithDefault #-}
zipWithDefault :: a -> (a -> a -> c) -> [a] -> [a] -> [c]
zipWithDefault a0 f (a:as) (b:bs) = f a b : zipWithDefault a0 f as bs
zipWithDefault a0 f [] (b:bs) = f a0 b : zipWithDefault a0 f [] bs
zipWithDefault a0 f (a:as) [] = f a a0 : zipWithDefault a0 f as []
zipWithDefault _ _ _ _ = []

-- | test whether the first list is a subset of the second one
subset :: Eq a => [a] -> [a] -> Bool
subset as bs = all (`elem` bs) as

-- | all subsets for a list
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (a:as) =  uncurry (++) . map21 (id, map (a:)) . subsets $ as

-- | like 'findIndices' but results a list of tuples (x, i) where x is the list and i the index
findIndicesTuples :: (a -> Bool) -> [a] -> [([a], Int)]
findIndicesTuples f as = map (appendFst as) . findIndices f $ as

-- | replace the element at the given position by a given list of elements
--
-- for just removing the j-th element of a list, use the following function
--
-- > replaceInList j []
--
-- for replacing the j-th element of a list by an element @a@, use the following function
--
-- > replaceInList j [a]
replaceInList :: Int -> [a] -> [a] -> [a]
replaceInList n as = uncurry (++) . mapSnd ((++) as. tail) . splitAt n

-- | replace all appearances of an element in a list by a given list of elements
replaceElementInList :: Eq a => a -> [a] -> [a] -> [a]
replaceElementInList a bs as = case elemIndex a as of
    Just j -> take j as ++ bs ++ replaceElementInList a bs (drop (j + 1) as)
    Nothing -> as

-- | remove the j-th element from a list
removeFromList :: Int -> [a] ->[a]
removeFromList j = replaceInList j []

-- | like 'stripPrefix' but for postfixes
stripPostfix :: Eq a => [a] -> [a] -> Maybe [a]
stripPostfix = curry $ mapJust reverse . uncurry stripPrefix . map12 reverse

-- | apply a function to the element at the given position in a given list of elements
applyToList :: Int -> (a -> a) -> [a] -> [a]
applyToList n f as = replaceInList n [f $ as `at` n] as

-- | map a function @f@ to a list; the function results a result value @b@ as well as a value @c@ which can be used for the computation of the next element (i.e. the next @f a@)
mapFoldl :: (Maybe c -> a -> (c, b)) -> [a] -> [b]
mapFoldl f = mapFoldl' f Nothing where
    mapFoldl' _ _ [] = []
    mapFoldl' f' lastC (a:as) = b : mapFoldl' f' (Just c) as where
        (c, b) = f' lastC a

-- | map an array of functions to a value
reverseMap :: [a -> b] -> a -> [b]
reverseMap fs a = map (\f -> f a) fs

-- | count for how many entries the predicate is met
count :: (a -> Bool) -> [a] -> Int
count = length .* filter

-- | delete all occurances of an element in a list
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll = filter . (/=)

-- | delete all occurances of a sublist in a list
deleteAlls :: Eq a => [a] -> [a] -> [a]
deleteAlls = flip $ foldr deleteAll

-- | cartesian product
cartesian :: [a] -> [b] -> [(a, b)]
cartesian as bs = [(a, b) | a <- as, b <- bs]

-- --== BOOLEAN OPERATORS

-- | xor function
xor :: Bool -> Bool -> Bool
xor a = xnor (not a)

-- | xnor function
xnor :: Bool -> Bool -> Bool
xnor a b = (a && b) || (not a && not b)

-- --== NUMBER OPERATIONS

-- | compute the average of a list of numbers
average :: [Double] -> Double
average xs
    | null xs = error "Average cannot be computed on empty list"
    | otherwise = uncurry (/) . map21 (sum, fromIntegral . length) $ xs

-- --== STRING OPERATIONS

-- | append a char as often as needed in order to return a string of given length where the given string ist justified left
justifyLeft :: Int -> Char -> String -> String
justifyLeft n c s = s ++ replicate (max 0 $ n - length s) c

-- | append a char as often as needed in order to return a string of given length where the given string ist justified right
justifyRight :: Int -> Char -> String -> String
justifyRight n c s = replicate (max 0 $ n - length s) c ++ s

-- --== MAYBE OPERATIONS

-- | maps a 'Just' value
mapJust :: (a -> b) -> Maybe a -> Maybe b
mapJust = fmap

-- | uses an endomorphism parametrised by a 'Just' value
onJustUse :: (a -> b -> b) -> Maybe a -> b -> b
onJustUse f = \case
    (Just b) -> f b
    _ -> id

-- --== EITHER OPERATIONS

-- | maps a 'Left' value
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

-- | maps a 'Right' value
{-# ANN mapRight "HLint: ignore Use fmap" #-}
mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f = either Left (Right . f)

-- | returns 'Left' value
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "Error: fromLeft on Right"

-- | returns 'Right' value
fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Error: fromRight on Left"

-- | 'Either'-like type for 3 values
data Either3 a b c = E1 a | E2 b | E3 c
