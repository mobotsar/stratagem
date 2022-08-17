module LibInternal where

import qualified Data.Set as Set
import Data.Set (Set, insert, partition, union)

import qualified Data.List.NonEmpty as DLNe

import Control.Monad (ap)

import Util

import qualified Aasam
import Aasam
    ( CfgProduction
    , ContextFree
    , NonTerminal(NonTerminal)
    , Precedence
    , PrecedenceProduction(Closed, Infixl, Infixr, Postfix, Prefix)
    , Terminal(Terminal)
    , m
    )
import Data.Bifunctor (Bifunctor(first, second))

-- |Takes a Precedence and adds the required Closed production to it, then converts it to a CFG and adds the pure productions
lambdificate :: Precedence -> ContextFree
lambdificate p = sullyWith cfg
  where
    cfg@(start, _) =
        case Aasam.m (Closed (DLNe.singleton "PURE") `insert` p) of
            Left x0 -> x0
            Right _ -> error "This is a bug in Stratagem. Please report with error number 8675309."
    sullyWith :: ContextFree -> ContextFree
    sullyWith = second $ partition isPureProd >. first f >. uncurry union >. union pure
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
                , (lhs, [lt "(", Right start, Right start, lt ")"])
                , (lhs, [lt "(", Right start, lt ".", Right start, lt ")"])
                ]
          where
            lt = Left . Terminal
            lhs = NonTerminal "PURE"
