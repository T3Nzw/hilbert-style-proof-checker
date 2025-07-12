{-# LANGUAGE InstanceSigs #-}

module Program.Theorem where

import Control.Monad.State
import Data.Map.Ordered
import qualified Data.Set as S
import Defs
import qualified Defs.ProofStatement as PS
import Defs.Theorem
import qualified Program.ProofStatement as PS

getIden :: Theorem -> Identifier
getIden (Theorem iden _ _ _) = iden

prove' :: ProvedTheorems -> Theorem -> Either String (Maybe PS.ProofStatement)
prove' ts (Theorem iden (PS.Goal goal) initctx pss) =
  if PS.Oof `elem` pss
    then Right Nothing
    else case success of
      Nothing -> if S.member goal ctx then Right Nothing else Left "goal not derived"
      Just fail -> Right $ Just fail
  where
    (success, PS.Context ctx) = runState (PS.proofcheck ts pss) initctx

type TheoremState = State ProvedTheorems (Either String (Maybe PS.ProofStatement))

prove :: [Theorem] -> TheoremState
prove [] = pure $ Right Nothing
prove (t : ts) = do
  let thIden = "proof of theorem \"" ++ getIden t ++ "\""
  proved <- get
  case prove' proved t of
    Left err -> pure $ Left $ thIden ++ " failed: " ++ err
    Right m -> case m of
      Just ps -> pure $ Left $ thIden ++ " failed at proof statement:\n" ++ show ps
      Nothing -> do
        modify $ (<|) (thIden, t)
        prove ts

-- TODO pretty print proof process (expand axioms and substitutions)
