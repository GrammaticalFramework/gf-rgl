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
  every_Det = let tab = (detUnlikeAdj False Sg "jed").s
    in {s,sp = asQuant tab ; n = Sg ; a = Weak ; isDef = False ; hasDefArt = False} ;
  everything_NP = nameNounPhrase Neutr {s = caselist "alles" "alles" "allem" "alles"} ;
  everywhere_Adv = ss "überall" ;
  few_Det = let tab = (detLikeAdj False Pl "wenig").s
    in {s,sp = asQuant tab ; n = Pl ; a = Weak ; isDef = False ; hasDefArt = False} ;
----  first_Ord = {s = (regA "erst").s ! Posit} ;
  for_Prep = mkPrep "für" P.accusative ;
  from_Prep = mkPrep "aus" P.dative ;
  he_Pron = mkPronPers "er" "ihn" "ihm" "seiner" "sein"  Masc Sg P3 ;
  here7to_Adv = ss ["hierher"] ;
  here7from_Adv = ss ["hieraus"] ;
  here_Adv = ss "hier" ;
  how_IAdv = ss "wie" ;
  how8much_IAdv = ss "wieviel" ;
  how8many_IDet = {s = \\g,c => (detUnlikeAdj False Pl "wie viel").s ! g ! c ; n = Pl} ;
  if_Subj = ss "wenn" ; --- no variants in the RGL! | ss "falls" ;
  in8front_Prep = mkPrep "vor" P.dative ;
  i_Pron = mkPronPers "ich" "mich" "mir" "meiner" "mein" Masc Sg P1 ;
--  in_Prep = mkPrep [] (NPP CInDat) ;
  in_Prep = P.inDat_Prep ; -- HL 7/2022
  it_Pron = mkPronPers "es" "es" "ihm" "seiner" "sein"  Neutr Sg P3 ;
  less_CAdv = X.mkCAdv "weniger" "als" ;
  many_Det = let tab = (detLikeAdj False Pl "viel").s
    in {s,sp = asQuant tab ; n = Pl ; a = Weak ; isDef = False ; hasDefArt = False} ;
  more_CAdv = X.mkCAdv "mehr" "als" ;
  most_Predet = {                                                           -- HL 5/2022
    s = \\n,g,c => let gn = R.gennum g n ;
                      adj = (P.mkA "viel" "mehr" "meiste").s ! Superl
                   in
                      R.artDef ! gn ! c ++ adj ! (agrAdj g Weak n c) ;
    c = {p = [] ; k = PredCase Gen} ;
    a = PAg Pl} ;
  much_Det = {s = asQuant (\\_,_ => "viel") ; sp = asQuant (\\_,_ => "vieles") ;
              n = Sg ; a = Weak ; isDef = False ; hasDefArt = False} ;
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
  part_Prep = P.von_Prep ;    -- obsolete, use PartNP cn np
  please_Voc = ss "bitte" ;
  possess_Prep = P.von_Prep ; -- obsolete, use PossNP cn np
  quite_Adv = ss "ziemlich" ;
  she_Pron = mkPronPers "sie" "sie" "ihr" "ihrer" "ihr" Fem Sg P3 ;
  so_AdA = ss "so" ;
  somebody_NP = nameNounPhrase Masc {s = caselist "jemand" "jemanden" "jemandem" "jemands"} ;
  somePl_Det = let tab = (detLikeAdj True Pl "einig").s
    in {s,sp = asQuant tab ; isDef = True ; n = Pl ; a = Weak ; hasDefArt = False} ;
  someSg_Det = {
      s,sp = asQuant (\\g,c => "ein" + pronEnding ! GSg g ! c) ;  ---- einer,eines
      n = Sg ;
      a = Strong ;
      hasNum = True ;
      isDef = False ; hasDefArt = False
      } ;
  something_NP = nameNounPhrase Neutr {s = \\_ => "etwas"} ;
  somewhere_Adv = ss "irgendwo" ;
  that_Quant = let 
     jener : Number => Gender => Case => Str = \\n => (detUnlikeAdj True n "jen").s in 
     {s,sp = \\_,_ => jener ; a,aPl = Weak ; hasDefArt = False} ;
---b  that_NP = nameNounPhrase Neutr {s = caselist "das" "das" "dem" "dessen"} ; ----
  there_Adv = ss "da" ; --- no variants in the rgl | ss "dort" ;
  there7to_Adv = ss "dahin" ;
  there7from_Adv = ss ["daher"] ;
  therefore_PConj = ss "deshalb" ;
---b  these_NP = {s = caselist "diese" "diese" "diesen" "dieser" ; a = agrP3 Pl} ;
  they_Pron = mkPronPers "sie" "sie" "ihnen" "ihrer" "ihr" Fem Pl P3 ;
  this_Quant = let 
     dieser : Number => Gender => Case => Str = \\n => (detUnlikeAdj True n "dies").s in 
     {s,sp = \\_,_ => dieser ; a,aPl = Weak ; hasDefArt = False} ;
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
  which_IQuant = {s = \\n,g,c => (detUnlikeAdj True n "welch").s ! g ! c} ;

  whoSg_IP = {s = caselist "wer" "wen" "wem" "wessen" ; n = Sg} ;
  whoPl_IP = {s = caselist "wer" "wen" "wem" "wessen" ; n = Sg} ; -- HL 6/2016
  why_IAdv = ss "warum" ;
  without_Prep = mkPrep "ohne" P.accusative ;
  with_Prep = mkPrep "mit" P.dative ;
  youSg_Pron = mkPronPers "du" "dich" "dir" "deiner" "dein" Fem Sg P2 ;
  youPl_Pron = mkPronPers "ihr" "euch" "euch" "eurer" "euer" Fem Pl P2 ; ---- poss
  youPol_Pron = mkPronPers "Sie" "Sie" "Ihnen" "Ihrer" "Ihr" Fem Pl P3 ;
  yes_Utt = ss "ja" ;

  not_Predet = {s = \\_,_,_ => "nicht" ; c = noCase ; a = PAgNone} ;
  no_Quant = let 
     keiner : Number => Gender => Case => Str = table {
       Sg => \\g,c => "kein" + pronEnding ! GSg g ! c ;
       Pl => (detUnlikeAdj False Pl "kein").s
       }
     in 
     {s,sp = \\_,_ => keiner ; a = Strong ; aPl = Weak ; hasDefArt = False} ;   ---- sp
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
    asQuant : (Gender => Case => Str) -> (Bool => Gender => Case => Str) =
      \tab -> \\_,g,c => tab ! g ! c ; 
    asNum : (Gender => Case => Str) -> (Gender => Case => {quant,num:Str}) =
      \tab -> \\g,c => {quant = []; num = tab ! g ! c} ;
    pairTable : (Gender => Case => Str) -> (Gender => Case => Str) -> (Gender => Case => {quant,num:Str})
      = \qt,nt -> \\g,c => {quant = qt ! g ! c; num = nt ! g ! c} ;

    appAdjDegAdjf : Adjective -> Degree -> Adjf -> Number => Gender => Case => Str =
      \adj,deg,adjf -> \\n,g,c => adj.s ! deg ! (agrAdj g adjf n c) ;

}
