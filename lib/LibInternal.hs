module LibInternal where

import qualified Data.Set as Set
import Data.Set (Set, insert, partition, union)

import qualified Data.List.NonEmpty as DLNe

import Control.Monad ((<=<), ap)

import Util

import qualified Aasam
import Aasam
    ( AasamError
    , CfgProduction
    , ContextFree
    , NonTerminal(NonTerminal)
    , Precedence
    , PrecedenceProduction(Closed, Infixl, Infixr, Postfix, Prefix)
    , Terminal(Terminal)
    , m
    )
import Data.Bifunctor (Bifunctor(first, second))
import GHC.Event.Windows (CbResult)

data Rest =
    Rest
        { source :: String
        , reduction :: String
        , rc :: String
        }

newtype Oneshot =
    Oneshot Rest

newtype Repl =
    Repl Rest

newtype NonCreates =
    NonCreates (Either Oneshot Repl)

newtype Create =
    Create String

newtype Config =
    Config (Either Create NonCreates)

buildConfig :: String -> Maybe Config
buildConfig raw = Nothing

-- |Takes a Precedence and adds the required Closed production to it, then converts it to a CFG and adds the pure productions
lambdify :: Precedence -> Either AasamError ContextFree
lambdify = sullyWith <=< (swapEither . Aasam.m . insert (Closed (DLNe.singleton "PURE")))
  where
    sullyWith cfg = Right $ second (union pure . uncurry union . first f . partition isPureProd) cfg
      where
        isPureProd (NonTerminal "CE", [Left (Terminal "PURE")]) = True
        isPureProd _ = False
        f = Set.map (second $ const [Right (NonTerminal "PURE")])
        pure =
            Set.fromList
                [ (lhs, [lt "X"])
                , (lhs, [lt "(", start, start, lt ")"])
                , (lhs, [lt "(", start, lt ".", start, lt ")"])
                ]
          where
            lt = Left . Terminal
            lhs = NonTerminal "PURE"
            start = Right $ fst cfg
