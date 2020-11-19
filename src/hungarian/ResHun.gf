--# -path=.:../abstract:../common:../../prelude

--1 Hungarian auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work.
-- Some parameters, such as $Number$, are inherited from $ParamX$.
resource ResHun = NounMorphoHun ** open Prelude, Predef in {

--------------------------------------------------------------------------------
-- NP

-- Noun morphology is in NounMorphoHun

oper
  CNoun : Type = Noun ** {
    compl : Number => Case => Str ;
    postmod : Str ;
    } ;

  mkCaseNoun : Str -> Number => Case => Str = \s ->
    \\n,c => caseFromStem (\a,b -> a+b) (mkNoun s) c n ;
  mkCaseNoun2 : (n,a : Str) -> Number => Case => Str = \no,ac ->
    \\n,c => caseFromStem (\a,b -> a+b) (regNounNomAcc no ac) c n ;

  caseFromStem : (Str->Str->Str) -> Noun -> Case -> Number -> Str = \bind,cn,cas,n ->
    let applyCase' : NumCaseStem -> Str = applyCase bind cas cn in
    case <n,cas> of {
      <Sg,Nom> => cn.s ! SgNom ;
      <Sg,Acc> => bind (cn.s ! SgAccStem) "t" ;
      <Sg,Sup> => cn.s ! SgSup ;
      <Sg,All> => cn.s ! SgAll ;
      <Pl,Nom> => cn.s ! PlStem ; -- don't use applyCase', it adds a BIND which breaks everything!
      <Sg,Ins|Tra> => applyCase' SgInsStem ;
      <Pl,Ins|Tra> => bind (bind (cn.s ! PlStem) "k") (endCase cas ! cn.h) ;
      <Sg,_>   => applyCase' SgStem ;
      <Pl,_>   => applyCase' PlStem
      } ;

  caseFromPossStem : CNoun -> Determiner -> Case -> Str = \cn,det,cas ->
    let st : PossStem = case det.dt of {
          DetPoss x => x ;
          _ => Predef.error "caseFromPossStem: Not possessive Det" } ;
        casetable : Case->HarmForms = case <det.n,st> of {
          -- P3 Sg possessive suffix ends in vowel, others in consonant.
          <Sg, dSg_rP3 Sg> => endCasePossVow ;
          _                => endCase } ;
        stem : NumCaseStem = case det.n of {
          Pl => PossdPl ;
          Sg => case st of {
                  dSg_rP3 _  => PossdSg_PossrP3 ;
                  dSg_rPl1   => PossdSg_PossrPl1 ;
                  dSg_rSg1P2 => PossdSg_PossrSg1P2 }
          } ;

        -- possessive suffix e.g. "their cats-3pl" is just k. not uk/ük
        -- possessive suffix e.g. "her cat-3sg" is ∅, we store
        suf = case <det.n,st> of {
          <Pl, dSg_rP3 Pl> => "k" ;
          <Pl, dSg_rP3 Sg> => "" ;
          _               => det.poss ! cn.h } ;
     in case <cas,det.n,st> of {
         -- Possessor is P3 Sg, possessed is plural, case is Nom:
         -- just use the stored PossdPl stem, e.g. 'madarai'
         <Nom, Pl, dSg_rP3 Sg> => cn.s ! PossdPl ;

         -- Any number of possr or possd, case Nom = empty case ending
         <Nom> => glue (cn.s ! stem) suf ;

         -- Other forms have non-empty poss. suffix and case ending
          _ => applyCaseSuf suf cas cn stem casetable
        } ;

  BaseNP : Type = {
    agr : Person*Number ;
    objdef : ObjDef ;
    empty : Str ; -- standard trick for pro-drop
    } ;

  NounPhrase : Type = BaseNP ** {
    s : Possessor => Case => Str ;
    postmod : Str ;
    } ;

  emptyNP : NounPhrase = {
    s = \\_,_ => [] ;
    agr = <P3,Sg> ;
    objdef = Indef ;
    postmod, empty = [] ;
    } ;

  indeclNP : Str -> NounPhrase = \s -> emptyNP ** {s = \\p,c => s} ;

  defNP : Str -> Number -> NounPhrase = \s,n -> emptyNP ** {
    s = \\c => mkCaseNoun s ! n ;
    n = n ;
    objdef = Def ;
    } ;

  defNPPrefix : (p,n : Str) -> Number -> NounPhrase = \vala,mi,n -> emptyNP ** {
    s = \\p,c => vala + mkCaseNoun mi ! n ! c ;
    n = n ;
    objdef = Def ;
    } ;

  linCN : CNoun -> Str = \cn -> cn.s ! SgNom ++ cn.compl ! Sg ! Nom ++ cn.postmod ;
  linNP' : Possessor -> Case -> NounPhrase -> Str = \p,c,np -> np.s ! p ! c ++ np.postmod ;
  linNP : NounPhrase -> Str = linNP' NoPoss Nom ;
--------------------------------------------------------------------------------
-- Pronouns

  Pronoun : Type = BaseNP ** {
    s : Case => Str ;
    poss : HarmForms ; -- for PossPron : Pron -> Quant
    } ;

  possForms : Person*Number => HarmForms = \\agr => case agr of {
    <P1,Sg> => harm1 "m" ;
    <P2,Sg> => harm1 "d" ;
    <P3,Sg> => harm "a" "e" ; --TODO: wovel cases with "ja"? See verb forms below
    <P1,Pl> => harm1 "nk" ; -- u/ü/other vowel in stem
    <P2,Pl> => harm "tok" "tök" ;
    <P3,Pl> => harm "uk" "ük"
  } ;

  pronTable : Person*Number => Pronoun = \\agr => case agr of {
    <P1,Sg> => emptyNP ** {
                s = caseTable "én" "engem" "nekem"
                              "belém" "bennem" "belőlem" -- inner locatives
                              "hozzám" "nálam" "tőlem"   -- outer locatives
                              "rám" "rajtam" "rólam"     -- outer locatives
                              "értem" -- Causative
                              "velem" -- Instrumental
                              nonExist ; -- Translative
                agr = agr ;
                objdef = Def ;
                poss = possForms ! agr } ;
    <P2,Sg> => emptyNP ** {
                s = caseTable "te" "teged" "neked"
                              "beléd" "benned" "belőled"
                              "hozzád" "nálad" "tőled"
                              "rád" "rajtad" "rólad"
                              "érted" -- Causative
                              "veled" -- Instrumental
                              nonExist ; -- Translative
                agr = agr ;
                objdef = Def ;
                poss = possForms ! agr } ;
    <P3,Sg> => emptyNP ** {
                s = caseTable "ő" "őt" "neki"
                              "belé" "benne" "belőle"
                              "hozzá" "nála" "tőle"
                              "rá" "rajta" "róla"
                              "érte" -- Causative
                              "vele" -- Instrumental
                              nonExist ; -- Translative
                objdef = Def ;
                poss = possForms ! agr } ;
    <P1,Pl> => emptyNP ** {
                s = caseTable "mi" "minket" "nekünk"
                              "belénk" "bennünk" "belőlünk"
                              "hozzánk" "nálunk" "tőlünk"
                              "ránk" "rajtunk" "rólunk"
                              "értünk" -- Causative
                              "velünk" -- Instrumental
                              nonExist ; -- Translative
                agr = agr ;
                objdef = Def ;
                poss = possForms ! agr } ;
    <P2,Pl> => emptyNP ** {
                s = caseTable "ti" "titeket" "nektek"
                              "belétek" "bennetek" "belőletek"
                              "hozzátok" "nálatok" "tőletek"
                              "rátok" "rajtatok" "rólatok"
                              "értetek" -- Causative
                              "veletek" -- Instrumental
                              nonExist ; -- Translative
                agr = agr ;
                objdef = Def ;
                poss = possForms ! agr } ;
    <P3,Pl> => emptyNP ** {
                s = caseTable "ők" "őket" "nekik"
                              "beléjük" "bennük" "belőlük"
                              "hozzájuk" "náluk" "tőlük"
                              "rájuk" "rajtuk" "róluk"
                              "értük" -- Causative
                              "velük" -- Instrumental
                              nonExist ; -- Translative
                agr = agr ;
                objdef = Def ;
                poss = possForms ! agr }
  } ;
--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

  BaseQuant : Type = {
    poss : HarmForms ; -- Quants made by PossPron need this, empty for others
    caseagr : Bool ; -- If it agrees in case: "azoknak a nőknek" vs. "sok nőknek"
  } ;

  -- Quant has variable number:
  -- e.g. this_Quant has both "this" and "these"
  Quant : Type = BaseQuant ** {
    s, -- form that comes before noun: "{this} car"
    sp : Number => Case => Str ; -- independent form, "I like {this}" (DetNP)
    qt : QuantType ;
    } ;

  mkQuant : (s,sp : Str) -> Quant = \s,sp -> {
    s = mkCaseNoun s ;
    sp = mkCaseNoun sp ;
    qt = DefQuant ;
    caseagr = True ;
    poss = harm1 [] ;
    } ;

 -- Det is formed in DetQuant : Quant -> Num -> Det
 -- so it has an inherent number.
  Determiner : Type = BaseQuant ** {
    s,
    sp : Case => Str ;
    n : Number ;
    dt : DetType ;
    } ;

  mkDet : (s : Str) -> ObjDef -> Number -> Bool -> Determiner = \s,d,n,ca -> {
    s,
    sp = mkCaseNoun s ! n ;
    n = n ;
    numtype = NoNum ;
    caseagr = ca ;
    dt = objdef2dt d ;
    poss = harm1 [] ;
  } ;

  mkDet2 : (n,a : Str) -> ObjDef -> Number -> Bool -> Determiner = \no,ac,d,n,ca ->
    let reg : Determiner = mkDet no d n ca
     in reg ** {
          s,sp = mkCaseNoun2 no ac ! n ;
        } ;

  -- No need for number:
  -- https://en.wikisource.org/wiki/Simplified_Grammar_of_the_Hungarian_Language/Nouns
  -- "Nouns are used in the singular only, if preceded by a numeral or any other
  -- word expressing quantity; as két ember, two men; sok fa, many trees."
  Numeral : Type = {
    s : Place => Str ;  -- Independent or attribute
    -- TODO add ordinal
    } ;

  {- Numeral can become Num via
      Noun.gf:    NumNumeral : Numeral -> Card ;
      Noun.gf:    NumCard : Card -> Num ;
  -}
  Num : Type = Numeral ** {
    n : NumType ;        -- Singular, plural or numeral
  } ;

  baseNum : Num = {
    s = \\_ => [] ;
    n = NoNum Sg ;
    } ;

--------------------------------------------------------------------------------
-- Adpositions

  -- TODO: personal suffixes, e.g. felettem, általam, not *felett/által én
  Adposition : Type = {
    pr : Str ; -- Preposition
    s : Str ;  -- Postposition
    c : Case ;
    } ;

  nomAdp : Str -> Adposition = \s -> postpos Nom s ;

  caseAdp = overload {
    caseAdp : Case -> Adposition = \c -> postpos c [] ;
    caseAdp : Case -> Str -> Adposition = \c,s -> postpos c s ;
  } ;
  postpos : Case -> Str -> Adposition = \c,s-> {s=s ; c=c ; pr=[]} ;
  prepos : Case -> Str -> Adposition = \c,s -> {s=[] ; c=c ; pr=s} ;

  emptyAdp : Adposition = nomAdp [] ;

  applyAdp : Adposition -> NounPhrase -> Str = \adp,np ->
    adp.pr ++ np.s ! NoPoss ! adp.c ++ adp.s ++ np.postmod ;

  applyCase : (Str->Str->Str) -> Case -> Noun -> NumCaseStem -> Str =
    \bind,cas,cn,stem -> bind (cn.s ! stem) (endCase cas ! cn.h) ;

  applyCaseSuf : Str -> Case -> CNoun -> NumCaseStem -> (Case -> HarmForms) -> Str =
    \suf,cas,cn,stem,casetable ->
      glue (glue (cn.s ! stem) suf) (casetable cas ! cn.h) ;


------------------
-- Conj

  Conj : Type = {
    s1 : Str ;
    s2 : Str ;
    n : Number ;
    } ;

  mkConj : Str -> Number -> Conj = mkDConj [] ;

  mkDConj : (s1,s2 : Str) -> Number -> Conj = \s1,s2,num -> {
    s1 = s1 ;
    s2 = s2 ;
    n = num ;
    } ;
--------------------------------------------------------------------------------
-- Adjectives

  AdjPhrase : Type = {
    s : Number => Case => Str ;
    compl : Number => Str -- Discontinuous comparative: Én nagyobb vagyok nálad.
                          -- This depends on Number to allow postmodifier APs.
    } ;

  emptyAP : AdjPhrase = {
    s = \\_,_ => [] ;
    compl = \\_ => [] ;
    } ;

  Adjective : Type = {
    s : Degree => NumCaseStem => Str ;
    h : Harm ;
    } ;

  Adjective2 : Type = Adjective ** {
    c2 : Adposition ;
    isPost : Bool ; -- put adjective past the thing it modifies
    } ;

  mkAdj : Str -> Adjective = \sgnom -> mkAdj2 sgnom (regNoun sgnom) ;


  mkAdj2 : Str -> Noun -> Adjective = \sgnom,adjAsNoun -> adjAsNoun ** {
    s = \\d =>
       let adj : Noun = case d of {
             Compar => mkNoun (comparAdj sgnom) ;
             Superl => mkNoun ("leg" + comparAdj sgnom) ;
             _ => adjAsNoun } ;
       in adj.s ;
    } ;

  invarAP : Str -> AdjPhrase = \s -> emptyAP ** {s = \\_,_ => s} ;

  -- https://en.wikisource.org/wiki/Simplified_Grammar_of_the_Hungarian_Language/Adjectives
  comparAdj : Str -> Str = \stem ->
    case stem of {
--      Final a and e become lengthened at the end of a word, if the comparative suffix -bb is joined to it—e.g., drága, drágább; fekete, feketébb, &c.; ó shortens its sound only in jó; jobb, legjobb.
      "szép"   => "szebb" ;
      "könnyű" => "könnyebb" ;
      "ifju"   => "ifjabb" ;
      "hosszú" => "hosszabb" ;
      "sok"    => "több" ;
      "felső"  => "felsőbb" ;
      "belső"  => "belsőbb" ;
      _ + #v   => stem + "bb" ;
      _        => stem + harm "abb" "ebb" ! getHarm stem
    } ;

  pluralAdj : Str -> Str = \stem ->
    case vowFinal stem of {
      True => -- https://en.wikisource.org/wiki/Simplified_Grammar_of_the_Hungarian_Language/Adjectives
        case last stem of { "ü" => "ek" ;
                            "i" => harm "ak" "ek" ! getHarm stem ;
                            _   => "k" } ;

      False => harm3 "ok" "ek" "ök" ! getHarm stem
    } ;
--------------------------------------------------------------------------------
-- Verbs

  VerbEndings : Type = Person*Number => HarmForms ;
  -- TODO: incomplete
  endingsDef : VerbEndings = table {
    <P1,Sg> => harm3 "om" "em" "öm" ;
    <P2,Sg> => harm3 "od" "ed" "öd" ;
    <P3,Sg> => harm "ja" "i" ;
    <P1,Pl> => harm "juk" "jük" ;
    <P2,Pl> => harm "játok" "itek" ;
    <P3,Pl> => harm "ják" "ik"
    } ;

  -- by EG 2009, needs more special cases
  endingsIndef : VerbEndings = table {
    <P1,Sg> => harm3 "ok" "ek" "ök" ;
    <P2,Sg> => harm1 "sz" ;
    <P3,Sg> => harm1 [] ;
    <P1,Pl> => harm "unk" "ünk" ;
    <P2,Pl> => harm3 "tok" "tek" "tök" ; -- TODO allomorphs -otok, -etek, -ötök
    <P3,Pl> => harm "nak" "nek"  -- TODO allomorphs -anak, -enek
    } ;

  BaseVerb : Type = {
    sc : SubjCase ; -- subject case
  } ;
  Verb : Type = BaseVerb ** {
    s : VForm => Str ;
    } ;
  Verb2 : Type = BaseVerb ** {
    s : ObjDef => VForm => Str ;
    c2 : Case   -- object case
    } ;
  Verb3 : Type = Verb2 ** {
    -- c3 : Case   -- indirect object case
    } ;

  datV2 : Verb -> Verb2 = \v -> {
    s = \\_ => v.s ;
    sc = SCDat ;
    c2 = Nom
    } ;

  mkVerb2 : Str -> Verb2 = \sg3 -> vtov2 (mkVerb sg3) ;
  mkVerb3 : Str -> Verb3 = \sg3 -> v2tov3 (mkVerb2 sg3) ;

  vtov2 : Verb -> Verb2 = \v -> v ** {
    s = table {
          Def => let vDef : Verb = mkVerbReg endingsDef (v.s ! VInf) (v.s ! VPres P3 Sg)
                  in vDef.s ;
          Indef => v.s } ;
    c2 = Acc
    } ;
  v2tov3 : Verb2 -> Verb3 = \v -> v ** {c3 = Dat} ;

  mkVerb : (sg3 : Str) -> Verb = mkVerbReg endingsIndef "TODO:infinitive" ; -- TODO

  mkVerbReg : VerbEndings -> (inf, stem : Str) -> Verb = \hf,inf,stem ->
    let h : Harm = getHarm stem ;
        sg1 : Str = stem + hf ! <P1,Sg> ! h ;
        sg2 : Str = stem + hf ! <P2,Sg> ! h ;
        sg3 : Str = stem + hf ! <P3,Sg> ! h ;
        pl1 : Str = stem + hf ! <P1,Pl> ! h ;
        pl2 : Str = stem + hf ! <P2,Pl> ! h ;
        pl3 : Str = stem + hf ! <P3,Pl> ! h ;
     in mkVerbFull sg1 sg2 sg3 pl1 pl2 pl3 inf ;

  mkVerbFull : (x1,_,_,_,_,_,x7 : Str) -> Verb =
    \sg1,sg2,sg3,pl1,pl2,pl3,inf -> {
      s = table {
        VInf => inf ;
        VPres P1 Sg => sg1 ;
        VPres P2 Sg => sg2 ;
        VPres P3 Sg => sg3 ;
        VPres P1 Pl => pl1 ;
        VPres P2 Pl => pl2 ;
        VPres P3 Pl => pl3
      } ;
      sc = SCNom
    } ;

  copula : Verb = mkVerbFull
    "vagyok"
    "vagy"
    "van"
    "vagyunk"
    "vagytok"
    "vannak"
    "lenni" ;

  megvan : Verb = copula ** {
    s = \\vf => "meg" + copula.s ! vf ;
    } ;

------------------
-- VP

  VerbPhrase : Type = Verb ** {
    obj : Str ; -- Person*Number => Str, if we want open word order in have_V2
    adv : Str ;
    c2 : Case ; -- for RelSlash
    } ;  -- TODO more fields

  VPSlash : Type = Verb2 ** {
    adv : Str ;
    } ;

  useV : Verb -> VerbPhrase = \v -> v ** {
    obj = [] ;
    adv = [] ;
    c2 = Acc ; -- TODO check
    } ;

  useVc : Verb2 -> VPSlash = \v2 -> v2 ** {
    adv = [] ;
    } ;

  insertAdv : VerbPhrase -> SS -> VerbPhrase = \vp,adv -> vp ** {adv = adv.s} ;
  insertAdvSlash : VPSlash -> SS -> VPSlash = \vps,adv -> vps ** {adv = adv.s} ;

--------------------------------------------------------------------------------
-- Cl, S

  Clause : Type = {s : Tense => Anteriority => Polarity => Str} ;

  {- After PredVP, we might still want to add more adverbs (QuestIAdv),
     but we're done with verb inflection.
   -}
  ClSlash : Type = Clause ** {
    c2 : Case ; -- For RelSlash
    } ;

  QClause : Type = Clause ;


  Sentence : Type = {s : Str} ;

  predVP : NounPhrase -> VerbPhrase -> ClSlash = \np,vp -> vp ** {
    s = \\t,a,p => let subjcase : Case = case vp.sc of {
                                               SCNom => Nom ;
                                               SCDat => Dat }
                        in linNP' NoPoss subjcase np
                        ++ if_then_Pol p [] "nem"
                        ++ vp.s ! agr2vf np.agr
                        ++ vp.obj -- ! np.agr
                        ++ vp.adv
                        ++ np.empty -- standard trick for prodrop+metavariable problem
    } ;

  -- Relative

  RP : Type = {s : Number => Case => Str} ;
  RClause : Type = {s : Tense => Anteriority => Polarity => Number => Case => Str} ;

  relVP : RP -> VerbPhrase -> RClause = \rp -> relVP' (rp ** {agr=<P3,Sg>}) ;

  relVP' : RP ** {agr : Person*Number} -> VerbPhrase -> RClause = \rp,vp -> {
    s = \\t,a,p,n,c => let subjcase : Case = case vp.sc of {
                                               SCNom => Nom ;
                                               SCDat => Dat }
                        in rp.s ! n ! subjcase
                        ++ if_then_Pol p [] "nem"
                        ++ vp.obj -- ! <rp.agr.p1,n>
                        ++ vp.adv
                        ++ vp.s ! VPres rp.agr.p1 n -- variable by number
    } ;

  relSlash : RP -> ClSlash -> RClause = \rp,cls -> {
    s = \\t,a,p,n,c => let objcase : Case = case cls.c2 of {
                                              Acc => c ;
                                              _ => cls.c2 }
                    in rp.s ! n ! objcase
                    ++ cls.s ! t ! a ! p
   } ;
--------------------------------------------------------------------------------
-- linrefs

}
