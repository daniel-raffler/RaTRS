{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Core.Data.Matrix (
  module X,
  (*^),
  (^+^),
  (^-^),
  negative,
  dot,
  (##),
  (<#),
  (#>),
  kronecker,
  kronecker',
  offset,
  indices,
  toSV,
  toSM,
  fromRowsL,
  echelon,
  echelon',
  inverse,
  pinverse,
  slice
  ) where

import Protolude hiding (transpose)
import Pre

import qualified Data.Text as Text
import qualified Data.Map  as Map
import qualified Data.List as List

import Numeric.LinearAlgebra.Sparse as X (dim)

import Data.Sparse.SpMatrix as X (
  SpMatrix,
  ncols,
  nrows,
  lookupSM,
  toListSM,
  fromListSM,
  transposeSM,
  eye
  )

import Data.Sparse.SpVector as X (
  SpVector,
  fromListSV,
  fromListDenseSV,
  toListSV,
  toDenseListSV,
  lookupSV,
  lookupDefaultSV,
  filterSV
  )

import Data.Sparse.Common as X (toColsL, toRowsL, fromColsL)

import Text.PrettyPrint.Leijen.Text (
  Pretty,
  Doc,
  (<+>),
  (<$$>),
  textStrict,
  tupled,
  vsep,
  hsep,
  pretty,
  tupled,
  fill,
  integer,
  int
  )

import Data.List ((!!))

instance Pretty Rational where
  pretty v | denominator v == 1 = integer $ numerator v
           | otherwise = (integer $ numerator   v) <> textStrict "/" <>
                         (integer $ denominator v)

instance (Num a, Pretty a) => Pretty (SpMatrix a) where
  pretty m = grid [(pshow . tupled . map int . indices (dim m)) n :
                     (map (pshow . pretty) . toDenseListSV) c
                        | (n,c) <- zip [0..] $ toRowsL m]
    where grid c = vsep $ map (hsep . map pad . zip len) c
            where len = map (maximum . map Text.length) $ List.transpose c
                  pad (len,v) = textStrict $ toS $ take len (toS v <> repeat ' ')

instance (Num a, Pretty a) => Pretty (SpVector a) where
  pretty = tupled . map pretty . toDenseListSV

type Dim = (Int,Int)

-- | Print an error message if matrix dimensions are not compatible
checked :: (Num a, Pretty a)
        => Text -> (Dim -> Dim -> Bool) -> SpMatrix a -> SpMatrix a -> e -> e
checked m fx u v id | on fx dim u v = id
                    | otherwise     = error $ pshow (
                        textStrict "Could not" <+> textStrict m <+>
                          textStrict "matrices, dimensions are incompatible" <$$>
                          on parallel pretty u v)
  where parallel a b = vsep  $ on (zipWith spaces) (Text.lines . pshow) a b
          where spaces u v = textStrict (u <> "    " <> v)

checked' :: (Num a, Pretty a)
         => Text -> (Int -> Int -> Bool) -> SpVector a -> SpVector a -> e -> e
checked' m fx u v id = checked m fx' (toSM u) (toSM v) id
  where fx' (a,b) (u,v) = fx b v

-- | Multiply vector by a scalar
(*^) :: Num a => a -> SpVector a -> SpVector a
a *^ v = fromListSV (dim v) [(i, a*c) | (i,c) <- toListSV v]

-- | Add two vectors
(^+^) :: (Pretty a, Num a) => SpVector a -> SpVector a -> SpVector a
v ^+^ w = checked' "add" (==) v w $ fromListSV (dim v) $ on add toListSV v w
  where add a b = Map.toList $ on (Map.unionWith (+)) Map.fromList a b

-- | Substract two vectors
(^-^) :: (Pretty a, Num a) => SpVector a -> SpVector a -> SpVector a
v ^-^ w = checked' "substract" (==) v w (v ^+^ (negative w))

-- | Negate vector
negative :: Num a => SpVector a -> SpVector a
negative v = fromListSV (dim v) [(i, negate c) | (i,c) <- toListSV v]

-- | Inner product of two vectors
dot :: (Pretty a, Num a) => SpVector a -> SpVector a -> a
u `dot` v = checked' "multiply'd" (==) u v (sum $ Map.elems $ on (Map.intersectionWith (*)) (Map.fromList . toListSV) u v)

-- | Matrix multiplication
(##) :: (Pretty a, Num a) => SpMatrix a -> SpMatrix a -> SpMatrix a
mt ## ms = checked "multiply'#" (\(_,t) (s,_) -> t == s) mt ms $
  fromListSM (nrows mt, ncols ms)
    [(n, m, dot a b) | (n,a) <- zip [0..] $ toRowsL mt, (m,b) <- zip [0..] $ toColsL ms]

-- | Left multiplication with a vector 
(<#) :: (Pretty a, Num a) => SpVector a -> SpMatrix a -> SpVector a
v <# mt = checked "multiply#<" (\(_,t) (s,_) -> t == s) (toSM v) mt $
  fromListDenseSV (ncols mt) (map (v `dot`) $ toColsL mt)

-- | Right multiplication with a vector 
(#>) :: (Pretty a, Num a) => SpMatrix a -> SpVector a -> SpVector a
mt #> v = checked "multiply#>" (\(_,t) (s,_) -> t == s) mt (toSM' v) $
  fromListDenseSV (nrows mt) (map (`dot` v) $ toRowsL mt)

-- | Kronecker product
kronecker :: Num a => SpMatrix a -> SpMatrix a -> SpMatrix a
kronecker u v = fromListSM (rows,cols) $ map expand $ products [toListSM u, toListSM v]
  where expand [(lu,ru, wu), (lv,rv, wv)] =
          (offset (rows, nrows u) [lu,lv], offset (cols, ncols v) [ru,rv], wu * wv)
        rows = nrows u * nrows v
        cols = ncols u * ncols v

-- | k-fold kronecker product
kronecker' :: Num a => [SpMatrix a] -> SpMatrix a
kronecker' []    = eye 1
kronecker' (h:t) = foldr kronecker h t

-- | Map a tuple of indices to a row number
offset :: (Int,Int) -> [Int] -> Int
offset (card,mult) []    = 0
offset (card,mult) (h:t) = h + mult*(offset (card,mult) t)

-- | Inverse of offset, map a row number to a tuple of indices
indices :: (Int,Int) -> Int -> [Int]
indices (card,mult) k = take
    (round $ on logBase fromIntegral mult card) (indices' k <> repeat 0)
  where indices' 0 = []
        indices' k | k `div` mult == 0 = [k `mod` mult]
                   | otherwise         = (k `mod` mult) : indices' (k `div` mult)

-- | Cast from row vector to 1xn matrix
toSM :: Num a => SpVector a -> SpMatrix a
toSM v = fromListSM (1, dim v) [(0,j,v) | (j,v) <- toListSV v]

toSM' :: Num a => SpVector a -> SpMatrix a
toSM' v = fromListSM (1, dim v) [(i,0,v) | (i,v) <- toListSV v]

-- | Cast from 1xn matrix to row vector
toSV :: Num a => SpMatrix a -> SpVector a
toSV m = fromListSV (ncols m) [(j,v) | (i,j,v) <- toListSM m, i == 0]

-- | Patches a bug in the library
fromRowsL :: [SpVector a] -> SpMatrix a
fromRowsL vs = fromListSM (length vs, maximum $ map dim vs)
  [(row, col, val) | (row,v) <- zip [0..] vs, (col,val) <- toListSV v]

-- | Eliminate the leading coefficient of the topmost row from all other rows in the matrix
eliminate :: (Pretty a, Fractional a) => [SpVector a] -> Maybe (SpVector a, [SpVector a])
eliminate = map eliminate' . List.uncons
  where eliminate' (h,t) = (h, [v ^-^ factor h v | v <- t])
          where factor h c = (lookupDefaultSV 0 j c / v) *^ h
                  where (j,v) = pivot h

-- | Select pivot element from a row
pivot :: Fractional a => SpVector a -> (Int, a)
pivot = List.head . toListSV

-- | Convert matrix into row echelon form
echelon :: (Pretty a, Ord a, Num a, Fractional a) => SpMatrix a -> SpMatrix a
echelon =
  let -- | Move leading coefficients upwards
      order :: (Ord a, Fractional a) => [SpVector a] -> [SpVector a]
      order = sortBy ord .
              filter (not . null . toList) .
              map positive .
              map sparsify
        where ord :: (Ord a, Fractional a) => SpVector a -> SpVector a -> Ordering
              ord u v = case comparing (fst . pivot) u v of
                LT -> LT
                EQ -> comparing (abs . snd . pivot) u v
                GT -> GT
              positive v = signum (snd $ pivot v) *^ v
              sparsify   = filterSV (not . (== 0))
      
      -- | Normalize diagonal
      diagonal :: (Num a, Fractional a) => SpMatrix a -> SpMatrix a
      diagonal m = fromRowsL [dia (pivot rv) *^ rv | rv <- toRowsL m]
        where dia (j,v) = recip v
      
      -- | Apply reduction steps until row echelon form is reached
      reduce :: (Pretty a, Ord a, Fractional a) => SpMatrix a -> SpMatrix a
      reduce = fromRowsL . reduce' . toRowsL
        where reduce' vs = (eliminate $ order vs) &
                \ case Nothing     -> order vs
                       Just (a,r') -> a:(reduce' r')
      
  in diagonal . reduce

-- | Convert matrix into reduced row echelon form
echelon' :: (Pretty a, Ord a, Num a, Fractional a) => SpMatrix a -> SpMatrix a
echelon' = fromRowsL . reverse . reduce' . reverse . toRowsL . echelon
  where reduce' vs = eliminate vs &
          \ case Nothing     -> vs
                 Just (a,r') -> a:reduce' r'

-- | Inverse matrix
inverse :: (Pretty a, Ord a, Num a, Fractional a) => SpMatrix a -> SpMatrix a
inverse m | ncols m /= nrows m = error "Can't invert rectangular matrix"
          | otherwise          =
  let -- | Combine two matrices into an extended matrix
      extend :: SpMatrix a -> SpMatrix a -> SpMatrix a
      extend m n =
        fromRowsL $
          [fromListSV (ncols m + ncols n) (toListSV a <> patch (+ ncols m) (toListSV b))
            | (a,b) <- on zip toRowsL m n]
        where patch fx vx = [(fx j,v) | (j,v) <- vx]
      
      -- | Separate components of an extended matrix
      separate :: Int -> SpMatrix a -> (SpMatrix a, SpMatrix a)
      separate offset m =
        (fromListSM (nrows m, offset) ***
          (fromListSM (nrows m, ncols m - offset) . patch (\j -> j - offset)))
            (List.partition middle $ toListSM m)
        where middle (i,j,v) = j < offset 
              patch fx vx = [(i, fx j, v) | (i,j,v) <- vx]
      
      -- | Fix another bug in the library where equality depends on the existance of
      --   zeros in the sparse matrix  
      sparsify m = fromListSM (dim m) [(i,j,v) | (i,j,v) <- toListSM m, v /= 0]
      
      -- | Reduce matrix to turn extended matrix into its inverse
      reduce :: (Pretty a, Ord a, Num a, Fractional a) => SpMatrix a -> SpMatrix a
      reduce m | on (/=) sparsify e (eye $ nrows m) =
                   error ("Could not invert singular matrix\n" <> pshow m)
               | otherwise          = u
        where (e,u) = separate (ncols m) $ echelon' $ extend m $ eye (nrows m)
      
  in reduce m

-- | Moore-Penrose pseudo-inverse, assumes that the matrix has either full row or
--   column rank
--
--   m ≥ n : left  inverse
--   m ≤ n : right inverse
pinverse :: (Pretty a, Ord a, Num a, Fractional a) => SpMatrix a -> SpMatrix a
pinverse v | m >= n = (inverse (transposeSM v ## v)) ## (transposeSM v)
           | m <  n =           transposeSM v        ## (inverse (v ## transposeSM v))
  where (m,n) = dim v

-- | Slice is a special kind of vector matrix multiplication. Both arguments are assumed
--   to be k-fold kronecker products. It is then possible to partially apply the vector
--   to the matrix even while some of the fields are still unknown
--
--   slice
--    k  : position for the slice
--    v  : partial vector
--    mt : matrix to be reduced
--         reduced matrix
slice :: (Pretty a, Num a) => Int -> SpVector a -> SpMatrix a -> SpMatrix a
slice k v mt =
  let indexed mt = [(indices (dim mt) i,j,v) | (i,j,v) <- toListSM mt]
      
      group k = List.groupBy (project (==)) . List.sortBy (project compare)
        where project fx (i,j,v) (i',j',v') = on fx (!!k) i i'
      
      split k cl =
        (List.head *** fromListSM (nrows mt `div` ncols mt, ncols mt)) $
          (List.unzip $ map project cl)
        where project (i,j,v) = (h, (offset (dim mt) (a <> b), j, v))
                where (a,h:b) = splitAt k i
      
      rebuild v mx =
        fromListSM (ncols mt, ncols mt) $
          concat [conv i (v <# mk) | (i,mk) <- mx]
        where conv i v = [(i,j,v) | (j,v) <- toListSV v]
      
  in (rebuild v . map (split k) . group k . indexed) mt
