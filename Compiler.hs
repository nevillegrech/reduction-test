module Compiler where
import Core
infixl 5 :=


-- A program is simply a list of statements
data Stmt = Def Id Id [Stmt]      -- Function declaration
          | Id := Expr            -- Assignment
          | Return Expr           -- Return from function
          | If Expr [Stmt] [Stmt] -- IF then else
          | While Expr [Stmt]     -- While Loop
          | Only Expr             -- An expression is also a valid statement

data Expr = Id Id           -- An Identifier is an expression
          | Co Const        -- So is a constant
          | Of Id Expr      -- And a function call

---------------------- Reduction rules for statements --------------------
transs :: Loc -> [Stmt] -> (Loc,[Inst])
transs n [] = (n,[])
transs n (x:xs) = (m', insts ++ insts')
  where (m,insts) = trans n x
        (m',insts') = transs m xs
---------------------- Individual reduction rules by pattern matching ----
trans :: Loc -> Stmt -> (Loc,[Inst])        
trans n (x := expr) = (m+1, insts ++ [SG x])
  where (m,insts) = transe n expr
trans n (Return expr) = (m, insts)
  where (m,insts) = transe n expr
trans n (If expr st1 st2) = (m'', insts ++
                          [JIF (m' + 1)] ++
                           insts' ++ [JP m''] ++ insts'')
  where (m,insts) = transe n expr
        (m',insts') = transs (m+1) st1
        (m'',insts'') = transs (m'+1) st2
trans n (While expr st) = (m'+1, insts ++
                             [JIF $ m'+1] ++
                             insts' ++ [JP n])
  where (m, insts)   = transe n expr
        (m', insts') = transs (m+1) st
                            
trans n (Def f x st) = (n+2, [LC (Prog ((SG x):insts)), SG f])
  where (_,insts) = transs 1 (fixup st)
trans n (Only expr) = transe n expr
-- Adds a return statement to functions without a return
fixup st = if hasRet st then st else st ++ [Return (Co None)]
  where hasRet [] = False
        hasRet p  = isRet $ last p
          where isRet (Return _)=True
                isRet _=False
---------------------- Reduction rules for expressions -------------------
transe :: Loc -> Expr -> (Loc, [Inst])
transe n (Id x) = (n+1, [LG x])
transe n (Co c) = (n+1, [LC c])
transe n (f `Of` expr) = (m+1, insts ++ [CF f])
  where (m,insts) = transe n expr
-- The main interface for the compiler, ast to bytecode program
compile :: [Stmt] -> Program
compile stmts =  res ++ [LC None]
  where (_,res) = (transs 0 stmts)