{-# LANGUAGE GADTs #-}

module Main where

import Infinitive
import PGF
import Data.List

data Fact = Fact {
  content  :: Maybe Fact,
  tense    :: Maybe GTemp,
  polarity :: Maybe GPol,
  source   :: Maybe GNP,
  attitude :: Maybe GVS,
  agent    :: Maybe GNP,
  action   :: Maybe GVP
  }

initFact = Fact Nothing Nothing Nothing Nothing Nothing Nothing Nothing

factTree fact = case (agent fact, action fact) of
  (Just np, Just vp) ->
     GUttS $ GUseCl
       (maybe presentTense id (tense fact))
       (maybe positivePol id (polarity fact))
       (GPredVP np vp)
  _ -> GUttNP Gnothing_NP

presentTense = GTTAnt GTPres GASimul
pastTense = GTTAnt GTPast GASimul
perfectTense = GTTAnt GTPres GAAnter
pluperfectTense = GTTAnt GTPast GAAnter
positivePol = GPPos


facts :: Infinitive.Tree a -> [Fact]
facts t = case t of
  GComplPresPartActVS vs np vp ->
    [initFact{attitude = Just vs, agent = Just np, action = Just vp}]
  GComplPastPartActVS vs np vp ->
    [initFact{tense = Just perfectTense, attitude = Just vs, agent = Just np, action = Just vp}]
  _ -> composOpMPlus facts t


shortest = take 1 . sortOn treesize 

treesize t = case unApp t of
  Just (f, xs) -> 1 + sum (map treesize xs)
  _ -> 1


treat gr fin cat s = [
  (showExpr [] t ++ "\t" ++ linearize gr fin t)
     | pt <- shortest (parse gr fin cat s),
       let gt = map (gf . factTree) (facts (fg pt :: GUtt)),
       t <- gt
       ]


main = do
  gr <- readPGF "Infinitive.pgf"
  putStrLn "gr"
  let Just fin = readLanguage "InfinitiveFin"
  let typ = startCat gr
  flip mapM_ [0..] $ \_ -> do
    putStr "> "
    s <- getLine
    let ss = treat gr fin typ s
    putStrLn $ unlines ss

