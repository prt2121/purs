module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, runST, writeSTRef)


data Tree a =
  Leaf
  | Node (Tree a) a (Tree a)


-- could blow up
instance showTree :: (Show a) ⇒ Show (Tree a) where
  show t = go t id where
    go Leaf k = k ""
    go (Node l v r) k = go l $ \sl →
                          go r $ \sr →
                            k (sl <> " " <> show v <> " " <> sr)


newtype TaggedNode a =
  TaggedNode
  { n :: a
  , m :: Int
  }


instance showTaggedNode :: (Show a) ⇒ Show (TaggedNode a) where
  show (TaggedNode { n, m }) =
    "(" <> show n <> ", " <> show m <> ")"


testTree :: Tree String
testTree =
  Node (Node Leaf "A" Leaf)
       "M"
       (Node (Node Leaf "X" Leaf)
             "N"
             (Node Leaf "Y" Leaf))


bigTestTree :: Int → Tree Int
bigTestTree n = big Leaf n where
  big t 0 = t
  big t m = big (Node t m Leaf) (m - 1)


tagTree :: ∀ st e t.
      Tree t
      → Eff
           ( st :: ST st
           | e
           )
           (Tree (TaggedNode t))
tagTree t =
  do
    n ← newSTRef 0
    t' ← loop n t
    pure t'
    where
      loop :: STRef st Int
              → Tree t
              → Eff
                 ( st :: ST st
                 | e
                 )
                 (Tree (TaggedNode t))
      loop _ Leaf = pure Leaf
      loop n' (Node l v r) =
        do
          l' ← loop n' l
          i ← readSTRef n'
          writeSTRef n' (i + 1)
          r' ← loop n' r
          pure (Node l' (TaggedNode { n : v, m : i }) r')


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Tree :"
  t ← runST (tagTree (bigTestTree 10000))
  log $ show t
