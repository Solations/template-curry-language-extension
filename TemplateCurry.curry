module TemplateCurry (
    module AbstractCurry.Types
    , module AbstractCurry.Build
    , qtoIO, mkName, mkGlobalName, newName, Q(..)) 
  where

import AbstractCurry.Types
import AbstractCurry.Build

data Env = Env Int [String]

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
qtoIO (Q f) = fst <$> f (Env 1 [])

---------------------------------------------------------------------------------------
-- Name generators
---------------------------------------------------------------------------------------

-- By convention it should be avoided at all cost to use your own names. Please
-- always generate names with one of the provided name generators. The compiler can
-- not enforce this. This is a trade-off, that was deliberately chosen. Using your own
-- variable names, therefore is completely on your own risk.

-- Creates a capturable name from the given String. The name is resolved
-- by standard scoping rules. (Might at a later point be used with qualified names.)
mkName :: String -> Q CVarIName
mkName str = Q (\(Env n knownVars) -> 
  return ((0, str), Env n (str:knownVars)))

-- Creates a global name from the given String.
mkGlobalName :: String -> QName
mkGlobalName str = let 
    rev = reverse str
    (n, m) = span (/= '.') rev
  in (reverse (drop 1 m), reverse n)

-- Generates a fresh, unique name from the given String, that can't be captured.
newName :: String -> Q CVarIName
newName str = Q (\(Env n knownVars) -> 
  let var = str ++ show n in
  if (var `elem` knownVars) then
    do 
    runQ (newName str) (Env (n + 1) knownVars)
  else
    return ((n, var), Env (n + 1) (var:knownVars)))

-- qtoIO (mkName "x1" >>= \n1 -> newName "x" >>= \n2 -> newName "x" >>= \n3 -> return (n1,n2,n3))