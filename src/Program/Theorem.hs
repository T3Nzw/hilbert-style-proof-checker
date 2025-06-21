{-# LANGUAGE InstanceSigs #-}

module Program.Theorem where

import Control.Monad.State
import qualified Data.Set as S
import Program.Formulae
import Program.ProofStatement (ProofStatement)
import qualified Program.ProofStatement as PS

type Identifier = String

data Theorem = Theorem Identifier PS.Goal PS.Context PS.ProofStatements

-- TODO make it prettier
instance Show Theorem where
  show :: Theorem -> String
  show (Theorem iden goal (PS.Context ctx) pss) =
    "Theorem \""
      ++ iden
      ++ "\"\n"
      ++ show goal
      ++ "\n"
      ++ show (S.toList ctx)
      ++ "\n"
      ++ show pss

getIden :: Theorem -> Identifier
getIden (Theorem iden _ _ _) = iden

prove' :: Theorem -> Either String (Maybe ProofStatement)
prove' (Theorem iden (PS.Goal goal) initctx pss) =
  case success of
    Nothing -> if S.member goal ctx then Right Nothing else Left "goal not derived"
    Just fail -> Right $ Just fail
  where
    (success, PS.Context ctx) = runState (PS.proofcheck pss) initctx

prove :: [Theorem] -> Either String (Maybe ProofStatement)
prove [] = Right Nothing
prove (t : ts) = do
  let thIden = "proof of theorem \"" ++ getIden t ++ "\""
  case prove' t of
    Left err -> Left $ thIden ++ " failed: " ++ err
    Right m -> case m of
      Just ps -> Left $ thIden ++ " failed at proof statement:\n" ++ show ps
      Nothing -> Right Nothing
  prove ts

-- TODO pretty print proof process (expand axioms and substitutions)
