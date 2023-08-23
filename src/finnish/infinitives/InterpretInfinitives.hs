{-# LANGUAGE GADTs #-}

module Main where

import Infinitive
import PGF
import Data.List

data Fact = Fact {
  content  :: [Fact],
  tense    :: Maybe GTemp,
  polarity :: Maybe GPol,
  agent    :: Maybe GNP,
  action   :: Either GVS GVP
  }

initFact = Fact [] Nothing Nothing Nothing (Left undefined)

factTree fact = GUseCl mtense mpolarity (GPredVP magent mvp)
  where
    mvp = case action fact of
      Left vs -> GComplVS vs (factTree (head (content fact))) ---- head -> ambiguity 
      Right vp -> vp
    mtense = maybe presentTense id (tense fact)
    mpolarity = maybe positivePol id (polarity fact)
    magent = maybe GX_NP id (agent fact)

presentTense = GTTAnt GTPres GASimul
pastTense = GTTAnt GTPast GASimul
perfectTense = GTTAnt GTPres GAAnter
pluperfectTense = GTTAnt GTPast GAAnter
positivePol = GPPos
negativePol = GPNeg


facts :: Infinitive.Tree a -> [Fact]
facts t = case t of
  GUseCl temp pol s ->
    [f{
              tense = Just temp,
	      polarity = Just pol
	      } | f <- facts s]
  GAdjCN (GAgentPartAP np vpslash) cn ->
    [initFact{
              tense = Just perfectTense,
              agent = Just np,
	      action = Right (GComplSlash vpslash (GMassNP cn))}]
  GPredVP ag (GUseComp (GCompAP (GAgentPartAP np vpslash))) ->
    [initFact{
              tense = Just perfectTense,
              agent = Just np,
	      action = Right (GComplSlash vpslash ag)}]
  GPredVP np (GComplPresPartActReflVS vs vp) -> 
    [initFact{
              agent = Just np,
	      action = Left vs,
	      content = [
	        initFact{
		  agent = Just np,
		  action = Right vp
		  }]}]
  GPredVP np (GComplPastPartActReflVS vs vp) ->
      [initFact{
              agent = Just np,
	      action = Left vs,
	      content = [
	        initFact{
		  tense = Just perfectTense,
		  agent = Just np,
		  action = Right vp
		  }]}]
  GPredVP np (GAdvVP vp adv) ->
    [f{agent = Just np} | f <- facts vp ++ facts adv]
----  GPredVP np (GRAdvVP vp (GInf1LongRAdv vpa)) ->
----    [f{agent = Just np, action = } | f <- facts vp ++ facts adv]
  GPredVP np vp ->
    [f{agent = Just np} | f <- facts vp]
  GUseV v ->
    [initFact{
              action = Right t}]
  GUseV2 v ->
    [initFact{
              action = Right (GComplSlash (GSlashV2a v) GY_NP)}]
{-
  GComplPresPartActVS vs np vp ->
    [initFact{
              attitude =
	      Just vs,
	      agent = Just np,
	      action = Just vp}]
  GComplPastPartActVS vs np vp ->
    [initFact{
              tense = Just perfectTense,
	      attitude = Just vs,
              agent = Just np,
	      action = Just vp}]
  GInf3AbessAdv vp ->
    [initFact{
              polarity = Just negativePol,
	      action = Just vp}]
  GComplPresPartPassVS vs np vpslash -> 
    [initFact{
              attitude = Just vs,
	      action = Just (GComplSlash vpslash np)}]
  GComplPastPartPassVS vs np vpslash -> 
    [initFact{
              tense = Just perfectTense,
	      attitude = Just vs,
              action = Just (GComplSlash vpslash np)}]
-}
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

