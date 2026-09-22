module TemplateCurry (
    CurryProg (..), MName, QName, CVisibility (..), CTVarIName
  , CDefaultDecl (..), CClassDecl (..), CInstanceDecl (..)
  , CTypeDecl (..), CConsDecl (..), CFieldDecl (..)
  , CConstraint, CContext (..), CFunDep, CTypeExpr (..), CQualTypeExpr (..)
  , COpDecl (..), CFixity (..), Arity, CFuncDecl (..), CRhs (..), CRule (..)
  , CLocalDecl (..), CVarIName, CExpr (..), CCaseType (..), CStatement (..)
  , CPattern (..), CLiteral (..), CField, Q(..), qtoIO, mkName, mkGlobalName
  , newName) 
  where

import AbstractCurry.Types
import AbstractCurry.Build

data Env = Env Int

newtype Q a = Q { runQ :: Env -> IO (a, Env) }

instance Functor Q where
  fmap f (Q g) = Q (\env -> do
    (a, env') <- g env
    return (f a, env'))

instance Applicative Q where
  pure x = Q (\env -> return (x, env))
  Q f <*> Q g = Q(\env -> do
   (h, env') <- f env
   (i, env'') <- g env'
   return (h i, env''))

instance Monad Q where
  Q f >>= g = Q (\env -> do
    (h, env') <- f env
    runQ (g h) env')

qtoIO :: Q a -> IO a
qtoIO (Q f) = fst <$> f (Env 0)

---------------------------------------------------------------------------------------
-- Name generators
---------------------------------------------------------------------------------------

-- By convention it should be avoided at all cost to use your own names. Please
-- always generate names with one of the provided name generators. The compiler can
-- not enforce this. This is a trade-off, that was deliberately chosen. Using your own
-- variable names, therefore is completely on your own risk.

-- Creates a capturable name from the given String. The name is resolved
-- by standard scoping rules. (Might at a later point be used with qualified names.)
mkName :: String -> CVarIName
mkName str = (0, str)

-- Creates a global name from the given String.
mkGlobalName :: String -> QName
mkGlobalName str = let 
    rev = reverse str
    (n, m) = span (/= '.') rev
  in (reverse (drop 1 m), reverse n)

-- Generates a fresh, unique name from the given String, that can't be captured.
newName :: String -> Q CVarIName
newName str = Q (\(Env n) -> return ((n, str ++ "____" ++ show n), Env (n + 1)))
