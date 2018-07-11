{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module QueLam.P where

import           Data.Char
import           Data.Functor.Const
import           Data.Row.Internal
import           Data.Row.Records          hiding (map, zip)
import qualified Data.Row.Records          as R
import           Data.Text.Prettyprint.Doc
import           GHC.TypeLits

import           QueLam.Core

newtype P (schema :: Row (Row *)) a = P { getP :: Int -> Int -> Doc () }

varNames :: Int -> String
varNames 0 = "x"
varNames 1 = "y"
varNames 2 = "z"
varNames n | ord 'a' + n < ord 'z' = chr (ord 'a' + n - 3) : []
           | otherwise   = "x" ++ show (ord 'a' + n - ord 'z')

parenthesize :: Bool -> Doc a -> Doc a
parenthesize True  = parens
parenthesize False = id

instance Symantics P where
  type Obs P a = Doc ()
  type Handle P schema = ()
  int i = P $ \_ _ -> pretty i
  bool b = P $ \_ _ -> pretty b
  string s = P $ \_ _ -> viaShow s
  lam f = P $ \n l -> let v = pretty $ varNames n in
    parenthesize (l > 0) $
    backslash <> v <> dot <+>
    getP (f $ P $ \_ _ -> v) (n+1) 0
  (P e1) $$ (P e2) = P $ \n l -> parenthesize (l > 10) $
    e1 n 10 <+> e2 n 11
  for (P e) f = P $ \n l ->
    let v = pretty $ varNames n in
      parenthesize (l > 0) $ align $ vsep
      [ "for" <+> parens (v <+> "<-" <+> e n 0)
      , getP (f $ P $ \_ _ -> v) (n+1) 0 ]
  where' (P c) (P e) = P $ \n _ -> align $ vsep
    [ "where" <+> c n 100
    , e n 0 ]
  yield (P e) = P $ \n _ -> "yield" <+> e n 0
  nil = P $ \_ _ -> "[]"
  (P e1) @% (P e2) = P $ \n l ->
    parenthesize (l > 5) $ e1 n 5 <+> "@" <+> e2 n 6
  (P e1) =% (P e2) = P $ \n l ->
    parenthesize (l > 4) $ e1 n 5 <+> "==" <+> e2 n 5
  (P e1) *% (P e2) = P $ \n l ->
    parenthesize (l > 7) $ e1 n 7 <+> "*" <+> e2 n 8
  (P e1) &&% (P e2) = P $ \n l ->
    parenthesize (l > 3) $ e1 n 3 <+> "&&" <+> e2 n 4
  (P e) .% lbl = P $ \n _ ->
    e n 5 <> dot <> pretty (symbolVal lbl)
  rcrd :: forall schema row. WellBehaved row
       => Rec (Map (P schema) row) -> P schema (Rec row)
  rcrd r = P $ \n _ ->
    let r' = transform' @row @(P schema) (\p -> Const [getP p n 0]) r
        vs = getConst $ R.sequence @(Const [Doc ()]) @row r'
        ls = labels' @row
    in
    align $ encloseSep langle rangle comma
    (map (\(l,v) -> l <+> "=" <+> v) $ zip ls vs)
  table _ t = P $ \_ _ -> "table" <+> pretty (symbolVal t)
  observe x = getP x 0 0

-- test :: P '[] (Int -> (Int -> Int) -> Int)
-- test = lam $ \x -> lam $ \f -> f $$ (f $$ x)

-- test2 :: P '[] ((Int -> Int -> Int) -> Int -> Int -> Int)
-- test2 = lam $ \f -> lam $ \x -> lam $ \y -> f $$ x $$ y *% y
