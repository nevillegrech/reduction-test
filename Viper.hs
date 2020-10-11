module Viper where

import Core
import Data.Map
import Debug.Trace
import qualified Data.Map as M  
import qualified Data.Set as S
import Control.Monad.State

data Type = Int | Str | Bool | NoneType | Un
          | Fn | U (S.Set Type) deriving (Eq,Ord,Show)

type PLoc = [(Program, Loc)]
type Env = (Id → Type)

type Trail = [Spot]
type Spot = (PLoc,Id)
typeof (I _) = Int
typeof (S _) = Str
typeof Truth = Bool
typeof Falsity = Bool
typeof None = NoneType
typeof _ = Fn

toUnion a = if S.size res == 1 then S.toList res !! 0
            else U res
  where res=S.unions [getset t | t ← a]
getset (U a) = a
getset a = S.fromList [a]
                   
em=U S.empty
type PLocTuples=[(PLoc,PLoc)]
type MyState = State PLocTuples ()
ptuples :: Map Id Const → PLocTuples
ptuples init = res
  where
    getprog f=case M.lookup f init of
                (Just (Prog p)) -> Just p
                _ -> Nothing
    (_,res)=runState (ptup ([],[(force (getprog "main"),0)])) []
    lastinfn a=getinst a == Nothing
    ptup :: (PLoc,PLoc) → MyState
    ptup tup = do
      mems ← get
      modify (\mems → tup : mems)
      if elem tup mems || loc2==[]
      then return ()
      else do
        let getnxt [] = [[]]
            getnxt nxt =
              if lastinfn nxt'
              then concat [getnxt from | (from,to) ← mems , to==(f,0):rst]
              else [nxt']
              where
                (f,l):rst=nxt
                nxt' = (f,l+1):rst
        let nxt = case inst of
                JP n → totup [(f,n):rst]
                JIF n → totup (((f,n):rst):getnxt loc2)
                CF f | fprog /= Nothing → totup [loc]
                  where fprog = getprog f
                        loc=take acc ((force fprog,0):loc2)
                _ → totup (getnxt loc2)
        doall nxt 
        where
          doall [] = return ()
          doall (x:xs) = (ptup x) >> doall xs
          totup [] = []
          totup (x:xs) = ((loc2,x):totup xs)
          (loc1,loc2) = tup
          (f,l):rst = loc2
          inst=force $ getinst loc2
lst (_,a)=a
grouptuples [] m gk gv = m
grouptuples (tup:tups) m gk gv = grouptuples tups updated gk gv
  where updated=if M.member k m
                then M.adjust (\a → (v:a)) k m
                else M.insert k [v] m
        k=gk tup
        v=gv tup
           
--getinst init ((f, l):_) = case (M.lookup f init) of
--      (Just (Fn (Prog pr))) | (length pr > l) → Just (pr!!l)
--      _ → Nothing
--getinst _ _ = Nothing
getinst ((pr,l):_) = if l>=length pr then Nothing else Just (pr!!l)

force (Just a) = a

acc=2
inferp :: Map Id Const → PLoc → Id → Type
inferp init l x = gTp l x []
  where
    tuples = ptuples init
    prev=grouptuples tuples M.empty lst fst
    gTp :: PLoc → Id → Trail → Type
    gTp loc x t= case (force $ getinst loc) of
                _       | loc == [] → case M.lookup x init of
                                        Nothing → Un
                                        Just a → (typeof a)
                _       | elem spot t → em
                -- Load Constant
                (LC c)  | x==tos → typeof c
                -- Load Global
                (LG y)  | x==tos → env y
                 -- Store Global
                (SG y)  | x==y   → env tos
                -- Call Function
                (CF f)  | (x,M.lookup f init)==(tos,Just Randbool) → Bool
                _ → env x
      where
        spot=(loc,x)
        env x' = toUnion [gTp loc' x' (spot:t) | loc'←(prev!loc)]
inferf :: Map Id Const → PLoc → Id → Type
inferf init l x = gTf l x []
  where
    tuples = ptuples init
    next=grouptuples tuples M.empty fst lst
    gTf :: PLoc → Id → Trail → Type
    gTf loc x t= case (force $ getinst loc) of
                _       | loc == [] → Un
                _       | elem spot t → em
                -- Load Constant
                (LC c)  | x==tos → Un
                -- Load Global
                (LG y)  | x==tos → Un
                        | x==y   → env tos
                 -- Store Global
                (SG y)  | x==y   → Un
                        | x==tos → env y
                -- Jump if False 
                (JIF _) | x==tos → Bool
                -- Call Function
                (CF f)  | x==f → Fn
                        | (x,fn)==(tos,Just Usestr) → Str
                        | (x,fn)==(tos,Just Useint) → Int
                       where fn=M.lookup f init
                _ → env x
      where
        spot=(loc,x)
        env x' = toUnion [gTf loc' x' (spot:t) | loc'←next!loc]
