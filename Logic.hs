module Logic where

data Term = Const String | Var String | Func [Term]

data LSig a = Object (String, a)| Relation String ([Object a] -> Bool)

type Object a = a

type LStructure a = [LSig a]

eval :: Term a -> LStructure a -> Object a
-- Pre: The item to be evaluated exist in the l-structure. 
eval (Const s) ((Object (s', a)) : ls)
  | s == s'   = a
  | otherwise = eval (Const s) ls
eval o (_ : ls)
  = eval o lsi

eval (Var s) ((
