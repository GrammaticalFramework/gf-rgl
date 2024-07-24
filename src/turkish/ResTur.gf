--# -path=.:../abstract:../common:../../prelude

resource ResTur = ParamX ** open Prelude, Predef, HarmonyTur, SuffixTur in {

--2 For $Noun$

  flags
    coding=utf8 ;

  param
    Case = Nom | Acc | Dat | Gen | Loc | Ablat | Abess Polarity | Instr ;
    Species = Indef | Def ;
    Contiguity = Con | Sep ; --Concatenate or Separate

  oper
    Agr = {n : Number ; p : Person} ;
    Noun = {
      s   : Number => Case => Str ;
      gen : Number => Agr => Str ;
      h   : Harmony
    } ;
    Pron = {s : Case => Str ;
            h : Harmony;
            a : Agr} ;

    agrP3 : Number -> Agr ;
    agrP3 n = {n = n; p = P3} ;
    -- For $Adjective$

    conjAgr : Agr -> Agr -> Agr = \a,b -> 
      {n=conjNumber a.n b.n; p=conjPerson a.p b.p} ;

  oper
    Adjective = Noun ** { adv : Str } ;

    -- For $Verb$.

  param
    VForm =
       VInf Polarity
     | VImp Polarity Number
     | VFin Tense Polarity Agr
     ;

  param
    VStem =
       VBase Softness
     | VProg
     | VFuture
     | VPass
     ;

  param
    AoristType =
        PlSyl -- more than one syllable, takes -ir
      | SgSylConIrreg -- one syllable ending with consonant, but takes -ir
                      -- (here is the list: al-, bil-, bul-, dur-, gel-, gör-,
                      -- kal-, ol-, öl-, var-, ver-, vur-, san- )
      | SgSylConReg ; -- one syllable ending with consonant, takes -er

  param
    ConjType = Infix | Mixfix ;

    UseGen = NoGen | YesGen Agr | UseIndef ;

  oper
    Verb : Type = {
      s : Str ;
      stems : VStem => Str ;
      aoristType : AoristType ;
      h : Harmony
      } ;

--2 For $Numeral$
  param
    DForm = unit | ten ;
    CardOrd = NCard | NOrd ;

  oper
    mkPron : (ben,beni,bana,banin,bende,benden,benli,bensiz,benimle:Str) -> Number -> Person -> Pron =
     \ben,beni,bana,benim,bende,benden,benli,bensiz,benimle,n,p -> {
     s = table {
       Nom => ben ;
       Acc => beni ;
       Dat => bana ;
       Gen => benim ;
       Loc => bende ;
       Ablat => benden ;
       Abess Pos => benli ;
       Abess Neg => bensiz ;
       Instr => benimle
       } ;
     h = getHarmony ben ;
     a = {n=n; p=p} ;
     } ;

    -- Prep
    noPrep = mkPrep [] Nom;

    mkPrep : Str -> Case -> {s : Str; c : Case; lock_Prep : {}} =
      \s, c -> lin Prep {s=s; c=c};

    mkNP : Noun -> Number -> Person -> {s : Case => Str; h : Harmony; a : Agr} =
      \noun, n, p -> {
        s = noun.s ! n;
        h = noun.h;
        a = {n = n; p = p}
      } ;

    mkVerbForms : Verb -> VForm => Str =
      \v -> table {
              VInf Pos => v.s ;
              VInf Neg => v.stems ! VBase Soft ++ BIND ++
                          suffixStr v.h negativeSuffix +
                          suffixStr v.h infinitiveSuffix ;
              VImp p n => v.stems ! VBase Soft ++
                          case <p,n> of {
                            <Pos,Sg> => [] ;
                            <Neg,Sg> => BIND ++
                                        suffixStr v.h negativeSuffix ;
                            <Pos,Pl> => BIND ++
                                        suffixStr v.h p2PlImperSuffix ;
                            <Neg,Pl> => BIND ++
                                        suffixStr v.h negativeSuffix +
                                        (let negHar = mkHar (case v.h.vow of {
                                                               I_Har  | U_Har  => I_Har ;
                                                               Ih_Har | Uh_Har => Ih_Har
                                                             }) SVow
                                         in suffixStr negHar p2PlImperSuffix)
                          } ;
              VFin t p agr =>
                let presHar = mkHar (case v.h.con of {
                                       SCon _ => case v.aoristType of {
                                                   SgSylConReg => Ih_Har ;
                                                   _           => v.h.vow
                                                 } ;
                                       SVow   => v.h.vow
                                     }) (SCon Soft) ;
                    pastHar = mkHar v.h.vow SVow ;
                    futSoft = (verbSuffixes ! agr).stemT ;
                    futHar  = mkHar (case v.h.vow of {
                                       I_Har  | U_Har  => I_Har ;
                                       Ih_Har | Uh_Har => Ih_Har
                                     }) (SCon futSoft) ;
                    negHar  = mkHar (case v.h.vow of {
                                       I_Har  | U_Har  => I_Har ;
                                       Ih_Har | Uh_Har => Ih_Har
                                     }) SVow ;
                    presNegHar =
                              mkHar negHar.vow (SCon Soft)
                in case p of {
                     Pos => case t of {
                              Pres => v.stems ! VBase Soft ++ BIND ++
                                      suffixStr v.h (case v.aoristType of {
                                                       SgSylConReg => aoristErSuffix ;
                                                       _           => aoristIrSuffix
                                                     }) +
                                      suffixStr presHar (verbSuffixes ! agr) ;
                              Past => v.stems ! VBase Hard ++ BIND ++
                                      suffixStr v.h pastSuffix +
                                      suffixStr pastHar (verbSuffixes ! agr) ;
                              Fut  => v.stems ! VFuture ++ BIND ++
                                      suffixStr v.h (case futSoft of {
                                                       Soft => softFutureSuffix ;
                                                       Hard => futureSuffix
                                                     }) +
                                      suffixStr futHar (verbSuffixes ! agr) ;
                              Cond => v.stems ! VBase Hard ++ BIND ++
                                      suffixStr v.h (condCopulaSuffixes ! agr)
                            } ;
                     Neg => case t of {
                              Pres => v.stems ! VBase Hard ++ BIND ++
                                      suffixStr v.h negativeSuffix +
                                      case agr of {
                                        {n=Sg; p=P1} => suffixStr negHar (verbSuffixes ! agr) ;
                                        {n=Pl; p=P1} => suffixStr negHar p1PlAoristSuffix ;
                                        _            => suffixStr negHar aoristIzSuffix + suffixStr presNegHar (verbSuffixes ! agr)
                                      } ;
                              Past => v.stems ! VBase Hard ++ BIND ++
                                      suffixStr v.h negativeSuffix +
                                      suffixStr negHar pastSuffix +
                                      suffixStr negHar (verbSuffixes ! agr) ;
                              Fut  => v.stems ! VBase Hard ++ BIND ++
                                      suffixStr v.h negativeSuffix +
                                      suffixStr negHar (case (verbSuffixes ! agr).stemT of {
                                                          Soft => softFutureSuffix ;
                                                          Hard => futureSuffix
                                                        }) +
                                      suffixStr futHar (verbSuffixes ! agr) ;
                              Cond => v.stems ! VBase Hard ++ BIND ++
                                      suffixStr v.h negativeSuffix +
                                      suffixStr negHar (condCopulaSuffixes ! agr)
                            }
                   }
          } ;

    mkDet : Str -> Number -> UseGen -> {s : Str; n : Number; useGen : UseGen} =
      \s, n, ug -> {s = s; n = n; useGen = ug} ;

    linCoord : Str -> Ints 4 => Str ;
    linCoord comma = table {0 => "hem"; 1=>"ya"; 2=>"ne"; 3=>comma; 4=>[]} ;

    caseSuffixes : Case => Suffix =
      table {
        Nom   => empSuffix ;
        Acc   => accSuffix ;
        Dat   => datSuffix ;
        Gen   => genSuffix ;
        Loc   => locSuffix ;
        Ablat => ablatSuffix ;
        Abess Pos => abessPosSuffix ;
        Abess Neg => abessNegSuffix ;
        Instr => instrSuffix
      } ;

    genSuffixes : Agr => Suffix =
      table {
        {n=Sg; p=P1} => genSgP1Suffix ;
        {n=Sg; p=P2} => genSgP2Suffix ;
        {n=Sg; p=P3} => genSgP3Suffix ;
        {n=Pl; p=P1} => genPlP1Suffix ;
        {n=Pl; p=P2} => genPlP2Suffix ;
        {n=Pl; p=P3} => genPlP3Suffix
      } ;

    verbSuffixes : Agr => Suffix =
      table {
        {n=Sg; p=P1} => p1SgVerbalSuffix ;
        {n=Sg; p=P2} => p2SgVerbalSuffix ;
        {n=Sg; p=P3} => empSuffix ;
        {n=Pl; p=P1} => p1PlVerbalSuffix ;
        {n=Pl; p=P2} => p2PlVerbalSuffix ;
        {n=Pl; p=P3} => p3PlVerbalSuffix
      } ;

    alethicCopulaSuffixes : Agr => Suffix =
      table {
        {n=Sg; p=P1} => p1SgAlethicCopulaSuffix ;
        {n=Sg; p=P2} => p2SgAlethicCopulaSuffix ;
        {n=Sg; p=P3} => p3SgAlethicCopulaSuffix ;
        {n=Pl; p=P1} => p1PlAlethicCopulaSuffix ;
        {n=Pl; p=P2} => p2PlAlethicCopulaSuffix ;
        {n=Pl; p=P3} => p3PlAlethicCopulaSuffix
      } ;

    condCopulaSuffixes : Agr => Suffix =
      table {
        {n=Sg; p=P1} => p1SgCondCopulaSuffix ;
        {n=Sg; p=P2} => p2SgCondCopulaSuffix ;
        {n=Sg; p=P3} => p3SgCondCopulaSuffix ;
        {n=Pl; p=P1} => p1PlCondCopulaSuffix ;
        {n=Pl; p=P2} => p2PlCondCopulaSuffix ;
        {n=Pl; p=P3} => p3PlCondCopulaSuffix
      } ;

}
