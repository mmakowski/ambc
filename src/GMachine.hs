{- | After "Implementing Functional Languages: a tutorial" 
-}
module GMachine
where

import Data.Map (Map)
import qualified Data.Map as Map


data GmState = GmState { code    :: GmCode
                       , stack   :: GmStack
                       , heap    :: GmHeap
                       , globals :: GmGlobals
                       , stats   :: GmStats 
                       }
             deriving (Eq, Show)

type GmCode = [Instruction]

data Instruction = Unwind
                 | Pushglobal Name
                 | Pushint Int
                 | Push Int
                 | Mkap
                 | Slide Int
                 deriving (Eq, Show)

type GmStack = [Addr]

type GmHeap = Heap Node

type Heap a = (Int, [Addr], Map Addr a)

hInitial :: Heap a
hInitial = (0, [1..], Map.empty)

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, next:free, cts) n = ((size+1, free, Map.insert next n cts), next)

hLookup :: Heap a -> Addr -> a
hLookup (_, _, cts) a = Map.findWithDefault (error $ "can't find node with address" ++ show a ++ " in heap") a cts

data Node = NNum Int
          | NAp Addr Addr
          | NGlobal Int GmCode
          deriving (Eq, Show)

type GmGlobals = Map Name Addr 

type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1

statGetSteps :: GmStats -> Int
statGetSteps = id

type Addr = Int

type Name = String

eval :: GmState -> [GmState]
eval state = state : restStates
  where 
    restStates | gmFinal state = []
               | otherwise     = eval nextState
    nextState = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin state = state { stats = statIncSteps (stats state) }

gmFinal :: GmState -> Bool
gmFinal (GmState [] _ _ _ _) = True
gmFinal _                    = False

step :: GmState -> GmState
step state = dispatch i $ state { code = is }
  where
    i:is = code state

dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n)    = pushint n
dispatch Mkap           = mkap
dispatch (Push n)       = push n
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

pushglobal :: Name -> GmState -> GmState
pushglobal f state = state { stack = a : stack state }
  where
    a = Map.findWithDefault (error $ "Undeclared global: " ++ f) f (globals state)

pushint :: Int -> GmState -> GmState
pushint n state = state { stack = a : stack state, heap = heap' }
  where
    (heap', a) = hAlloc (heap state) (NNum n)

mkap :: GmState -> GmState
mkap state = state { stack = a:as', heap = heap' }
  where
    (heap', a) = hAlloc (heap state) (NAp a1 a2)
    a1:a2:as' = stack state

push :: Int -> GmState -> GmState
push n state = state { stack = a:as }
  where 
    as = stack state
    a = getArg (hLookup (heap state) (as !! (n+1)))

getArg :: Node -> Addr
getArg (NAp _ a) = a
getArg n         = error $ "wrong node type; expected NAp but found " ++ show n

slide :: Int -> GmState -> GmState
slide n state = state { stack = a : drop n as }
  where
    a:as = stack state

unwind :: GmState -> GmState
unwind state@(GmState _ (a:as) h _ _) = newState (hLookup h a)
  where
    newState (NNum _) = state
    newState (NAp a1 _) = state { code = [Unwind], stack = a1:a:as }
    newState (NGlobal n c)
      | length as < n = error "Unwinding with too few arguments"
      | otherwise     = state { code = c }
