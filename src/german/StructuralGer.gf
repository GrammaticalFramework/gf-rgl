concrete StructuralGer of Structural = CatGer ** 

  open MorphoGer, MakeStructuralGer, (X = ConstructX), 
       (P = ParadigmsGer), IrregGer, Prelude, (R = ResGer) in {

  flags optimize=all ;
    coding=utf8 ;

  lin

  above_Prep = mkPrep "über" P.dative ;
  after_Prep = mkPrep "nach" P.dative ;
--  all_Predet = {s = appAdj (regA "all") ; c = noCase ; a = PAgNone} ;
  all_Predet = {s = appAdj (regA "all") ; c = noCase ; a = PAg Pl} ; -- HL 5/2022
  almost_AdA, almost_AdN = ss "fast" ;
  although_Subj = ss "obwohl" ;
  always_AdV = ss "immer" ;
  and_Conj = {s1 = [] ; s2 = "und" ; n = Pl} ;
  because_Subj = ss "weil" ;
  before_Prep = mkPrep "vor" P.dative ;
  behind_Prep = mkPrep "hinter" P.dative ;
  between_Prep = mkPrep "zwischen" P.dative ;
  both7and_DConj = sd2 "sowohl" ["als auch"] ** {n = Sg} ;
  but_PConj = ss "aber" ;
  by8agent_Prep = mkPrep "durch" P.accusative ;
  by8means_Prep = mkPrep "mit" P.dative ;
  can8know_VV, can_VV = auxVV 
      (mkV 
        "können" "kann" "kannst" "kann" "könnt" "könn" 
        "konnte" "konntest" "konnten" "konntet"
        "könnte" "gekonnt" [] 
        VHaben) ;
  during_Prep = mkPrep "während" P.genitive ; --- no variants in the rgl | P.mkPrep P.accusative "über" ; 
  either7or_DConj = sd2 "entweder" "oder" ** {n = Sg} ;
  everybody_NP = nameNounPhrase Masc {s = caselist "jeder" "jeden" "jedem" "jedes"} ;
  every_Det = {
    s,sp = \\_,g,c => "jed" + detEnding ! (gennum g Sg) ! c ;
    n = Sg ; a = Weak ; isDef = False ; hasDefArt = False} ;
  everything_NP = nameNounPhrase Neutr {s = caselist "alles" "alles" "allem" "alles"} ;
  everywhere_Adv = ss "überall" ;
  few_Det = {
    s,sp = \\_,g,c => "wenig" + adjEnding ! (gennum g Pl) ! c ;
    n = Pl ; a = Strong ; isDef = False ; hasDefArt = False} ;
----  first_Ord = {s = (regA "erst").s ! Posit} ;
  for_Prep = mkPrep "für" P.accusative ;
  from_Prep = mkPrep "aus" P.dative ;
  he_Pron = mkPronPers "er" "ihn" "ihm" "seiner" "sein"  Masc Sg P3 ;
  here7to_Adv = ss ["hierher"] ;
  here7from_Adv = ss ["hieraus"] ;
  here_Adv = ss "hier" ;
  how_IAdv = ss "wie" ;
  how8much_IAdv = ss "wieviel" ;
  how8many_IDet = {s = \\g,c => "wie viel" + detEnding ! (gennum g Pl) ! c ; n = Pl} ;
  if_Subj = ss "wenn" ; --- no variants in the RGL! | ss "falls" ;
  in8front_Prep = mkPrep "vor" P.dative ;
  i_Pron = mkPronPers "ich" "mich" "mir" "meiner" "mein" Masc Sg P1 ;
  in_Prep = P.inDat_Prep ; 
  it_Pron = mkPronPers "es" "es" "ihm" "seiner" "sein"  Neutr Sg P3 ;
  less_CAdv = X.mkCAdv "weniger" "als" ;
  many_Det = {
    s,sp = \\_,g,c => "viel" + adjEnding ! (gennum g Pl) ! c ;
    n = Pl ; a = Strong ; isDef = False ; hasDefArt = False} ;
  more_CAdv = X.mkCAdv "mehr" "als" ;
--  most_Predet = {s = appAdj (regA "meist") ; c = noCase ; a = PAgNone} ;
  most_Predet = {                                                           -- HL 5/2022
    s = \\n,g,c => let gn = R.gennum g n ;
                      adj = (P.mkA "viel" "mehr" "meiste").s ! Superl
                   in
                      R.artDef ! gn ! c ++ adj ! (agrAdj Weak gn c) ;
    c = {p = [] ; k = PredCase Gen} ;
    a = PAg Pl} ;
  much_Det = {
    s = \\_,g,c => "viel" ;
    sp = \\_,g,c => "viel" + detEnding ! (gennum g Sg) ! c ;  ---- (GSg _ Sg) ! Gen ?
    n = Sg ; a = Strong ; isDef = False ; hasDefArt = False} ;
  must_VV = auxVV 
      (mkV 
        "müssen" "muss" "musst" "muss" "müsst" "müss" 
        "musste" "musstest" "mussten" "musstet"
        "müsste" "gemusst" [] 
        VHaben) ;
---  one_Quant = DEPREC
  only_Predet = {s = \\_,_,_ => "nur" ; c = noCase ; a = PAgNone} ;
  no_Utt = ss "nein" ;
---b  no_Phr = ss "nein" ;
  on_Prep = mkPrep "auf" P.dative ;
  or_Conj = {s1 = [] ; s2 = "oder" ; n = Sg} ;
  otherwise_PConj = ss "sonst" ;
  part_Prep = P.von_Prep ;    -- obsolete, better use PartNP cn np
  please_Voc = ss "bitte" ;
  possess_Prep = P.von_Prep ; -- obsolete, better use PossNP cn np
  quite_Adv = ss "ziemlich" ;
  she_Pron = mkPronPers "sie" "sie" "ihr" "ihrer" "ihr" Fem Sg P3 ;
  so_AdA = ss "so" ;
  somebody_NP = nameNounPhrase Masc {s = caselist "jemand" "jemanden" "jemandem" "jemands"} ;
  somePl_Det = {
    s,sp = \\_,g,c => "einig" + adjEnding ! (gennum g Pl) ! c ;
    n = Pl ; a = Strong ; isDef = True ; hasDefArt = False} ;
  someSg_Det = {
    s = \\_,g,c => "ein" + pronEnding ! GSg g ! c ;  -- ein, eine, ein
    sp = \\_,g,c => "ein" + detEnding ! GSg g ! c ;  -- einer, eine, eines
    n = Sg ; a = Mixed ; isDef = False ; hasDefArt = False
    } ;
  something_NP = nameNounPhrase Neutr {s = \\_ => "etwas"} ;
  somewhere_Adv = ss "irgendwo" ;
  that_Quant = let jener : GenNum => Case => Str = \\gn,c => "jen" + detEnding ! gn ! c
    in {s = \\_ => jener ; sp = jener ; a = Weak ; isDefArt,delCardOne = False} ;
---b  that_NP = nameNounPhrase Neutr {s = caselist "das" "das" "dem" "dessen"} ; ----
  there_Adv = ss "da" ; --- no variants in the rgl | ss "dort" ;
  there7to_Adv = ss "dahin" ;
  there7from_Adv = ss ["daher"] ;
  therefore_PConj = ss "deshalb" ;
---b  these_NP = {s = caselist "diese" "diese" "diesen" "dieser" ; a = agrP3 Pl} ;

  they_Pron = mkPronPers "sie" "sie" "ihnen" "ihrer" "ihr" Fem Pl P3 ;
  this_Quant = let dieser : GenNum => Case => Str = \\gn,c => "dies" + detEnding ! gn ! c
    in {s = \\_ => dieser ; sp = dieser ; a = Weak ; isDefArt, delCardOne = False} ;
---b  this_NP = nameNounPhrase Neutr {s = caselist "dies" "dies" "diesem" "dieses"} ; ----
---b  those_NP = {s = caselist "jene" "jene" "jenen" "jener" ; a = agrP3 Pl} ;
  through_Prep = mkPrep "durch" P.accusative ;
  too_AdA = ss "zu" ;
  to_Prep = mkPrep "nach" P.dative ;
  under_Prep = mkPrep "unter" P.dative ;
  very_AdA = ss "sehr" ;
  want_VV = auxVV 
      (mkV 
        "wollen" "will" "willst" "will" "wollt" "woll" 
        "wollte" "wolltest" "wollten" "wolltet"
        "wollte" "gewollt" [] 
        VHaben) ;
  we_Pron = mkPronPers "wir" "uns"  "uns"   "unser"  "unser" Fem Pl P1 ;

  whatSg_IP = {s = caselist "was" "was" "was" "wessen" ; n = Sg} ; ----
  whatPl_IP = {s = caselist "was" "was" "was" "wessen" ; n = Pl} ; -- HL 6/2016

  when_IAdv = ss "wann" ;
  when_Subj = ss "wenn" ;
  where_IAdv = ss "wo" ;
  which_IQuant = {s = \\gn,c => "welch" + detEnding ! gn ! c} ;
  whoSg_IP = {s = caselist "wer" "wen" "wem" "wessen" ; n = Sg} ;
  whoPl_IP = {s = caselist "wer" "wen" "wem" "wessen" ; n = Sg} ; -- HL 6/2016
  why_IAdv = ss "warum" ;
  without_Prep = mkPrep "ohne" P.accusative ;
  with_Prep = mkPrep "mit" P.dative ;
  youSg_Pron = mkPronPers "du" "dich" "dir" "deiner" "dein" Fem Sg P2 ;
  youPl_Pron = mkPronPers "ihr" "euch" "euch" "eurer" "euer" Fem Pl P2 ; ---- poss
  youPol_Pron = mkPronPers "Sie" "Sie" "Ihnen" "Ihrer" "Ihr" Fem Pl P3 ** {a = AgPlPol} ;
  yes_Utt = ss "ja" ;

  not_Predet = {s = \\_,_,_ => "nicht" ; c = noCase ; a = PAgNone} ;
  no_Quant = {
    s = \\_ => table {GSg g => \\c => "kein" + pronEnding ! GSg g ! c ;
                      GPl   => \\c => "kein" + detEnding ! GPl ! c} ;
    sp = \\gn,c => "kein" + detEnding ! gn ! c ;
    a = Mixed ; isDefArt = False ; delCardOne = True} ; -- HL kein+ein(er) => kein(er)
  if_then_Conj = {s1 = "wenn" ; s2 = "dann" ; n = Sg ; lock_Conj = <>} ;
  nobody_NP = 
    nameNounPhrase Masc {s = caselist "niemand" "niemanden" "niemandem" "niemands"} ;
  nothing_NP = 
    nameNounPhrase Neutr {s = \\_ => "nichts"} ; --maybe add: nameNounPhrase {s = \\_ => "garnichts"}
  at_least_AdN = ss "wenigstens" ;
  at_most_AdN = ss "höchstens" ;
  except_Prep = mkPrep "außer" P.dative ;

  as_CAdv = X.mkCAdv "ebenso" "wie" ;
  have_V2 = P.dirV2 IrregGer.haben_V ;
  that_Subj = ss "dass" ;

  lin language_title_Utt = ss "Deutsch" ;

oper
  asNum : (Gender => Case => Str) -> (Gender => Case => {quant,num:Str}) =
    \tab -> \\g,c => {quant = []; num = tab ! g ! c} ;
  pairTable : (Gender => Case => Str) -> (Gender => Case => Str) -> (Gender => Case => {quant,num:Str})
    = \qt,nt -> \\g,c => {quant = qt ! g ! c; num = nt ! g ! c} ;
  appAdjDegAdjf : Adjective -> Degree -> Adjf -> GenNum => Case => Str =
    \adj,deg,adjf -> \\gn,c => adj.s ! deg ! (agrAdj adjf gn c) ;

  -- (detLikeAdj b n str).s   = \\g,c => str + adjEnding ! (gennum g n) ! c
  -- (detUnLikeAdj b n str).s = \\g,c => str + detEnding ! (gennum g n) ! c
}
