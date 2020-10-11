module Interpreter where
import Core

import qualified Data.Map
import Data.Map

-- Reduction rules for individual instructions
redi :: Inst -> (Bindings, Loc) -> (Bindings, Loc)
redi (LC c)  (env, pc) = (env <+> (tos,c), pc+1)
redi (LG x)  (env, pc) = (env <+> (tos, env!x), pc+1)
redi (SG x)  (env, pc) = (env <+> (x,env!tos), pc+1)
redi (JP t)  (env, _) = (env, t)
redi (JIF t) (env, pc) = (env, targ (env!tos) pc t)
  where targ Truth pc t=pc+1
        targ Falsity pc t=t
        targ v _ _ = error ("Type error: " ++ show v ++ " is not a Bool")

redi (ASRT (S s))   (env, pc) = (assert (env!tos), pc+1)
  where assert Truth = env
        assert Falsity = error ("Assertion error: " ++ s)
redi (CF f)  (env, pc) = (exec (env!f) env, pc+1)
  where exec Usestr env = if isString (env!tos)
                           then env <+> (tos,None)
                           else error ("Usestr used on "++ show (env!tos))
        exec Useint env = if isInt (env!tos)
                           then env <+> (tos,None)
                           else error ("Useint used on "++ show (env!tos))
        exec Randbool env = env <+> (tos,Falsity)
        exec (Prog p) env = inter p env
        exec p _ = error (show p ++" cannot be called")
        isString (S _)=True
        isString _=False
        isInt (I _)=True
        isInt _=False

-- Connecting the reduction rules
proc inst p (env,pc) = intern p (redi inst (env,pc))

intern p (env, pc) = proc (p!!pc) p (env,pc)

inter p env = fst (intern p (env, 0))
