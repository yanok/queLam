{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module QueLam.P where

import           Data.Char
import           Data.Text.Prettyprint.Doc
import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core

newtype P (schema :: [(Symbol, [*])]) a = P { getP :: Int -> Int -> Bool -> Doc () }

varNames :: Int -> String
varNames 0 = "x"
varNames 1 = "y"
varNames 2 = "z"
varNames n | ord 'a' + n < ord 'z' = chr (ord 'a' + n - 3) : []
           | otherwise   = "x" ++ show (ord 'a' + n - ord 'z')

parenthesize :: Bool -> Doc a -> Doc a
parenthesize True = parens
parenthesize False = id

parenthesize' :: Bool -> (Int -> Doc a) -> Int -> Doc a
parenthesize' True e l = parens (e 0)
parenthesize' False e l = e l

instance Symantics P where
  type Obs P a = Doc ()
  type Handle P schema = ()
  int i = P $ \_ _ _ -> pretty i
  bool b = P $ \_ _ _ -> pretty b
  string s = P $ \_ _ _ -> viaShow s
  lam f = P $ \n l _ -> let v = pretty $ varNames n in
    parenthesize (l > 0) $
    backslash <> v <> dot <+>
    getP (f $ P $ \_ _ _ -> v) (n+1) 0 False
  (P e1) $$ (P e2) = P $ \n l _ -> parenthesize (l > 10) $
    e1 n 10 False <+> e2 n 11 False
  for (P e) f = P $ \n l _ ->
    let v = pretty $ varNames n in
      parenthesize (l > 0) $ align $ vsep
      [ "for" <+> parens (v <+> "<-" <+> e n 0 False)
      , getP (f $ P $ \_ _ _ -> v) (n+1) 0 False ]
  where' (P c) (P e) = P $ \n l _ -> align $ vsep
    [ "where" <+> c n 100 False
    , e n 0 False ]
  yield (P e) = P $ \n _ _ -> "yield" <+> e n 0 False
  nil = P $ \_ _ _ -> "[]"
  (P e1) @% (P e2) = P $ \n l _ ->
    parenthesize (l > 5) $ e1 n 5 False <+> "@" <+> e2 n 6 False
  (P e1) =% (P e2) = P $ \n l _ ->
    parenthesize (l > 4) $ e1 n 5 False <+> "==" <+> e2 n 5 False
  (P e1) *% (P e2) = P $ \n l _ ->
    parenthesize (l > 7) $ e1 n 7 False <+> "*" <+> e2 n 8 False
  (P e) .% lbl = P $ \n l _ ->
    e n 5 False <> dot <> pretty (symbolVal lbl)
  rnil' = P $ \_ _ isR -> if isR then mempty else "<>"
  (lbl := P e1) &% (P e2) = P $ \n l isR ->
    let contents = pretty (symbolVal lbl) <+> "=" <+> e1 n 0 False <>
          e2 n 0 True in
      if isR then comma <+> contents else angles contents
  table _ t = P $ \_ _ _ -> "table" <+> pretty (symbolVal t)
  observe x = getP x 0 0 False

-- test :: P '[] (Int -> (Int -> Int) -> Int)
-- test = lam $ \x -> lam $ \f -> f $$ (f $$ x)

-- test2 :: P '[] ((Int -> Int -> Int) -> Int -> Int -> Int)
-- test2 = lam $ \f -> lam $ \x -> lam $ \y -> f $$ x $$ y *% y
