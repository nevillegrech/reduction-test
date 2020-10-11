module Core where
  
import Data.Map
import qualified Data.Map as Map

-- We define an operator, which updates a mapping with a (key,value) tuple
          
-- We define an operator, which updates a mapping with a (key,value) tuple
infixl 5 <+>
m <+> (k,v)  = Map.insert k v m

-------------------- Bytecode Syntax --------------------
data Inst = LC Const    -- Load Constant
-- Loads the constant supplied as operand in the top of stack
          | LG Id       -- Load Global
-- Loads the value of the global supplied as operand in the top of stack
          | SG Id       -- Store Global
-- Loads the value at the top of stack into the global supplied as operand
          | CF Id       -- Call Function
-- Treating the value of the global supplied as operand as a function,
-- calls the function
          | MF Program   -- Make Function
-- Given the program supplied as operand, perform some elementry checks and
-- place the program in the top of stack
          | JP Loc      -- Jump
-- Jump unconditionally to the location supplied as operand
          | JIF Loc     -- Jump if false
-- Jump  to the location supplied as operand if the top of stack contains
-- a truth value
          | ASRT Const  -- Assert
-- Halt the execution of the program displaying an error message supplied as
-- operand if the top of stack does not contain a truth value.
            deriving (Eq, Ord,Show)
-----------------------------------------------------------
-- Top of stack
tos = "TOS"
-- Constants
{-
  A constant can either be an integer, string, or program
-}
data Const = I Int
           | S String
           | Prog Program
           | None         
-- the following constants cannot be
-- accessed by programs
           | Truth | Falsity
           | Randbool | Usestr| Useint
           | Isint | Isstr | Isbool
             deriving (Show,Ord,Eq)

type Program = [Inst]
type Bindings = Map Id Const
type Loc = Int
type Id = String
-- Initial bindings from variable names to their respective values
initBindings=Map.fromList [("True",Truth), ("False",Falsity),
                           ("usestr",Usestr), ("useint",Useint)
                           ,("randbool",Randbool)
                          ]
