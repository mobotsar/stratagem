module LibInternal where

import qualified Data.Set as Set
import Data.Set (Set, insert, partition, union)

import qualified Data.List.NonEmpty as DLNe

import Control.Monad ((<=<), ap)

import qualified Data.Text as Text
import Data.Text (Text)

import Util

import qualified GLL.Types.Grammar as GllGrm
import GLL.Types.Grammar (Grammar)

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
lambdify = sullyWith <=< (Aasam.m . insert (Closed $ (DLNe.singleton . Text.pack) "PURE"))
  where
    sullyWith cfg = Right $ second (union pure . uncurry union . first f . partition isPureProd) cfg
      where
        isPureProd (NonTerminal ce, [Left (Terminal pure)]) = ce == Text.pack "CE" && pure == Text.pack "PURE"
        isPureProd _ = False
        f = Set.map (second $ const [(Right . NonTerminal . Text.pack) "PURE"])
        pure =
            Set.fromList
                [ (lhs, [lt "X"])
                , (lhs, [lt "(", start, start, lt ")"])
                , (lhs, [lt "(", start, lt ".", start, lt ")"])
                ]
          where
            lt = Left . Terminal . Text.pack
            lhs = (NonTerminal . Text.pack) "PURE"
            start = Right $ fst cfg
