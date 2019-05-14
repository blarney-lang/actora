module Syntax where

import Descend
import Control.Monad

-- Abstact syntax
-- ==============

type Id = String

type Guard = Maybe Exp

data Exp =
    Bind Exp Exp
  | Apply Exp [Exp]
  | Tuple [Exp]
  | Case Exp [(Exp, Guard, [Exp])]
  | If [(Exp, [Exp])]
  | Cond Exp [Exp] [Exp]
  | Lambda [([Exp], Guard, [Exp])]
  | Int Integer
  | List [Exp]
  | Cons Exp Exp
  | Id Id
  | Atom Id
  | Var Id
  | Fun Id Int
    deriving (Eq, Show)

type Decl = (Id, [Exp], Guard, [Exp])

-- Primitives
-- ==========

isPrim :: Id -> Bool
isPrim id = id `elem` ["+", "-", "==", "/=", "<", "<=", ">", ">="]

-- Traversal
-- =========

instance Descend Exp where
  descendM f (Bind p e) = Bind <$> f p <*> f e
  descendM f (Apply x es) = Apply <$> f x <*> mapM f es
  descendM f (Tuple es) = Tuple <$> mapM f es
  descendM f (Case e alts) = Case <$> f e <*>
    sequence [ (,,) <$> f p <*> mapM f g <*> mapM f es | (p, g, es) <- alts ]
  descendM f (If alts) = If <$>
    sequence [ (,) <$> f c <*> mapM f es | (c, es) <- alts ]
  descendM f (Cond cond es0 es1) =
    Cond <$> f cond <*> mapM f es0 <*> mapM f es1
  descendM f (Lambda eqns) = Lambda <$>
    sequence [ (,,) <$> mapM f ps <*> mapM f g <*> mapM f rhs
             | (ps, g, rhs) <- eqns ]
  descendM f (Id x) = return (Id x)
  descendM f (Int i) = return (Int i)
  descendM f (Fun g n) = return (Fun g n)
  descendM f (Atom a) = return (Atom a)
  descendM f (Var v) = return (Var v)
  descendM f (List es) = List <$> mapM f es
  descendM f (Cons h t) = Cons <$> f h <*> f t

onExp :: (Exp -> Exp) -> [Decl] -> [Decl]
onExp f ds = map exp ds
  where
    exp (v, ps, g, es) = (v, map f ps, f `fmap` g, map f es)
