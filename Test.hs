module Test where
import Viper
import Core
import Interpreter
import Compiler
import Data.Map
import Control.Monad

-- names
x="x"
y="y"
f="f"
main="main"
out="out"
g="g"
h="h"
randbool="randbool"
usestr="usestr"    
useint="useint"
true="True"
false="False"
randchoice=Of ("randbool") (Co None)
infixl 5 ⊔ -- for joining types
a ⊔ b = toUnion [a,b]

infixl 5 ⊕  -- for updating environments
m ⊕ (k,v) = insert k v m
initenv=fromList [(randbool, Randbool),(usestr,Usestr),(useint,Useint),
                  (true,Truth),(false,Falsity)]
------------------------ bytecode test programs ----------------------
bctest0main =  [LC (I 3),SG y,CF f,LC (I 2)]
bctest0f = [LC (I 3), SG x]
bctest0all=initenv ⊕ (main, (Prog bctest0main)) ⊕ (f, (Prog bctest0f))

counter3f=compile [   y := (Id x),
                      x := (Co (I 3)),
                      If (randchoice) [
                        Only (f `Of` Co None)
                      ] []
                    ]
counter3main=compile [x := Id true,
                     Only (f `Of` Co None)]
counter3all=initenv ⊕ (main, (Prog counter3main)) ⊕ (f, (Prog counter3f))

asttest6main = compile [x := Id true,
                        Only (h `Of` Co None)]
asttest6f = compile [x := Co (I 2)]
asttest6g = compile [y := Id x]
asttest6h = compile [While (randchoice) [
                         Only (g `Of` Co None),
                         Only (f `Of` Co None)
                        ]
                      ]
asttest6all=initenv ⊕ (main, (Prog asttest6main)) ⊕ (f, (Prog asttest6f)) ⊕ (g, (Prog asttest6g)) ⊕ (h, (Prog asttest6h))
               
lastLoc init = case init!"main" of
              (Prog p) -> [(p,length p-1)]
firstLoc init = case init!"main" of
              (Prog p) -> [(p,0)]

assertEquals typ res = if res == typ
                      then do { print "ok" }
                      else do {
                             print $ "expected: " ++ show typ;
                             print $ "got: " ++ (show res);
                           }

checkLast init x t = assertEquals t (inferp init (lastLoc init) x)
checkFirst init x t = assertEquals t (inferf init (firstLoc init) x)
run_tests = do 
  checkLast bctest0all tos Int
  checkLast bctest0all y Int
  checkLast counter3all y (Int ⊔ Bool)
  checkLast asttest6all y (Int ⊔ Bool ⊔ Un)
  checkFirst asttest6all y Un
  checkFirst asttest6all randbool Fn
  