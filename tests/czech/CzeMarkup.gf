resource CzeMarkup = CzeTests ** open Prelude, SyntaxCze, ParadigmsCze,
  (L = LexiconCze), (E = ExtendCze), (G = GrammarCze), (M = MarkupCze) in {
oper
  washing : S = mkS (mkCl (mkNP (E.ProDrop i_Pron)) wash_V) ;
  markedWashing : S = M.MarkupS M.b_Mark washing ;
  todayMarked : S = G.AdvS L.today_Adv markedWashing ;
  swimming : S = mkS (mkCl (mkNP (E.ProDrop i_Pron)) L.swim_V) ;
  loving : S = mkS (mkCl (mkNP (E.ProDrop i_Pron)) L.love_V2 (mkNP she_Pron)) ;
}
