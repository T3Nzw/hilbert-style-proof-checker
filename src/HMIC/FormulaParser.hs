module HMIC.FormulaParser where

import qualified Data.Map as M
import qualified Program.Formulae as Formula

type BindingPower = (Int, Int)

data Op
  = Sub
  | Neg
  | Forall
  | Exists
  | And
  | Or
  | Implies

data OpInfo = OpInfo Op BindingPower

-- maybe not ideal
opTable :: M.Map String OpInfo
opTable =
  M.fromList
    [ ("[", OpInfo Sub (70, 0)), -- cannot directly look it up? idk
      ("!", OpInfo Neg (0, 60)),
      ("\\forall", OpInfo Forall (60, 0)),
      ("\\exists", OpInfo Exists (60, 0)),
      ("&", OpInfo And (50, 51)),
      ("|", OpInfo Or (40, 41)),
      ("->", OpInfo Implies (31, 30))
    ]
