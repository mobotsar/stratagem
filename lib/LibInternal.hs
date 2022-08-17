module LibInternal where

import qualified Data.Set as Set
import Data.Set (Set, insert, partition, union)

import qualified Data.List.NonEmpty as DLNe

import Control.Monad (ap)

import Util

import qualified Aasam
import Aasam
import Data.Bifunctor (Bifunctor(first))
import GLL.Combinators (Token(BoolLit))

lambdificate :: Precedence -> ContextFree
lambdificate p = sullyWith cfg
  where
    cfg =
        case Aasam.m (Closed (DLNe.singleton "PURE") `insert` p) of
            Left x0 -> x0
            Right _ -> error "This is a bug in Stratagem. Please report with error number 8675309."
    sullyWith :: ContextFree -> ContextFree
    sullyWith (start, prods) = (start, partition isPureProd prods |> first f |> uncurry union |> union pure)
      where
        isPureProd :: CfgProduction -> Bool
        isPureProd (NonTerminal "CE", [Left (Terminal "PURE")]) = True
        isPureProd _ = False
        f :: Set CfgProduction -> Set CfgProduction
        f = Set.map (\(lhs, _) -> (lhs, [Right (NonTerminal "PURE")]))
        pure :: Set CfgProduction
        pure =
            Set.fromList
                [ (lhs, [lt "X"])
                , (lhs, [lt "(", start, start, lt ")"])
                , (lhs, [lt "(", start, lt ".", start, lt ")"])
                ]
          where
            lt = Left . Terminal
            rnt = Right . NonTerminal
            lhs = NonTerminal "PURE"
            start = rnt (show (foldl df 0 p) ++ "00")
              where
                df = flip =<< (`ap` prec) . (if' .) . flip ((>) . prec)
                  where
                    prec (Closed _) = 0
                    prec (Infixl x _) = x
                    prec (Infixr x _) = x
                    prec (Prefix x _) = x
                    prec (Postfix x _) = x
