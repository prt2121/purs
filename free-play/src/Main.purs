module Main where

import Prelude
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Trans
import Control.Monad.Eff.Class
import Control.Monad.Reader.Class
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Node.ReadLine (Interface, READLINE, close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)

type CmdEff eff = Eff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION | eff)
type Repl e a = ReaderT Interface (CmdEff e) a

data CmdF a =
  PrintStr String a
  | GetStr (String -> a)

instance cmdF :: Functor CmdF where
  map f (PrintStr s x) = PrintStr s (f x)
  map f (GetStr x)     = GetStr (x >>> f)

type CmdT = FreeT CmdF
type Cmd = CmdT Identity

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
                ~> CmdEff eff
interpretCmd = go where
  go (PrintStr s a) =
    const a <$> log s
  go (GetStr k) = k <$> pure "test"

runCmd :: ∀ eff
          . Cmd
          ~> CmdEff eff
runCmd =
  runFreeT interpretCmd <<< hoistFreeT (pure <<< unwrap)

main :: ∀ eff
        . CmdEff eff Unit
main = do
  runCmd $ do
    printStr "blah"
    str <- getStr
    printStr str
