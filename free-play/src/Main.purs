module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Free.Trans (FreeT, hoistFreeT, liftFreeT, runFreeT)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Node.ReadLine (READLINE)
import Node.SimpleRepl (Repl, putStrLn, readLine, runRepl)

type CmdEff eff = Eff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | eff)
type CmdT       = FreeT CmdF
type Cmd        = CmdT Identity

data CmdF a =
  PrintStr String a
  | GetStr (String -> a)

instance cmdF :: Functor CmdF where
  map f (PrintStr s x) = PrintStr s (f x)
  map f (GetStr x)     = GetStr (x >>> f)

liftCmd :: ∀ m
           . Monad m
           => CmdF
           ~> CmdT m
liftCmd = liftFreeT

printStr :: ∀ m
            . Monad m
            => String
            -> CmdT m Unit
printStr str = liftCmd $ PrintStr str unit

getStr :: ∀ m
          . Monad m
          => CmdT m String
getStr = liftCmd $ GetStr id

interpretCmd :: ∀ eff
                . CmdF
                ~> Repl eff
interpretCmd = go where
  go (PrintStr s a) =
    const a <$> putStrLn s
  go (GetStr k) = k <$> readLine

runCmd :: ∀ eff
          . Cmd
          ~> Repl eff
runCmd =
  runFreeT interpretCmd <<< hoistFreeT (pure <<< unwrap)

main :: ∀ eff
        . CmdEff eff Unit
main =
  runRepl do
    loop
    where
      loop = do
        runCmd $ do
          str <- getStr
          printStr str
        loop
