module FileHandler where

import Control.Exception
import Control.Monad.State (evalState)
import Data.List (isSuffixOf)
import qualified Data.Map.Ordered as OMap
import Defs.ProofStatement
import Defs.Theorem (Theorem (..))
import HMIC.Parser
import Parser
import Program.Theorem (prove)
import System.Environment (getArgs)

-- the only not-so-fun part about functional languages...
dothestuff :: IO ()
dothestuff = do
  args <- getArgs
  if length args < 2
    then putStrLn "please provide a file name (.hmic)"
    else do
      let filename = args !! 1
      if not (".hmic" `isSuffixOf` filename)
        then putStrLn "file name must end in .hmic"
        else do
          res <- try (readFile filename) :: IO (Either IOException String)
          case res of
            Left _ -> putStrLn "could not open file"
            Right contents -> do
              case evalP contents of
                Left err -> putStrLn $ "Could not parse file:\n" ++ show err
                Right (Theorems theorems) -> do
                  print theorems
                  case evalState (prove theorems) OMap.empty of
                    Left err -> putStrLn err
                    Right _ -> putStrLn "ok"
