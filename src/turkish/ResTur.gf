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
    Aspect =
       Perf | Imperf ;

    VForm =
       VInf Polarity
     | VImperfPart Polarity
     | VPerfPart   Polarity
     | VProspPart  Polarity
     | VImp Polarity Number
     | VFin Tense Anteriority Polarity Agr
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

    mkVerbForms : Verb -> Aspect => VForm => Str =
      \v -> \\asp =>
            table {
              VInf Pos => case asp of {
                            Perf   => v.s ;
                            Imperf => v.stems ! VBase Soft ++ BIND ++
                                      suffixStr v.h progrSuffix +
                                      suffixStr progrHar infinitiveSuffix
                          } ;
              VInf Neg => v.stems ! VBase Soft ++ BIND ++
                          suffixStr v.h negativeSuffix +
                          case asp of {
                            Perf   => suffixStr v.h infinitiveSuffix ;
                            Imperf => suffixStr negHar progrSuffix +
                                      suffixStr progrHar infinitiveSuffix
                          } ;
              VImperfPart Pos => v.stems ! VFuture ++ BIND ++
                                 suffixStr v.h imperfParticipleSuffix ;
              VImperfPart Neg => v.stems ! VBase Hard ++ BIND ++
                                 suffixStr v.h negativeSuffix +
                                 suffixStr negHar imperfParticipleSuffix ;
              VPerfPart Pos   => v.stems ! VBase Hard ++ BIND ++
                                 suffixStr v.h perfParticipleSuffix ;
              VPerfPart Neg   => v.stems ! VBase Hard ++ BIND ++
                                 suffixStr v.h negativeSuffix +
                                 suffixStr negHar perfParticipleSuffix ;
              VProspPart Pos  => v.stems ! VFuture ++ BIND ++
                                 suffixStr v.h prospParticipleSuffix ;
              VProspPart Neg  => v.stems ! VBase Hard ++ BIND ++
                                 suffixStr v.h negativeSuffix +
                                 suffixStr negHar prospParticipleSuffix ;
              VImp p n => case asp of {
                            Perf   => v.stems ! VBase Soft ++
                                      case <p,n> of {
                                        <Pos,Sg> => [] ;
                                        <Neg,Sg> => BIND ++
                                                    suffixStr v.h negativeSuffix ;
                                        <Pos,Pl> => BIND ++
                                                    suffixStr v.h p2PlImperSuffix ;
                                        <Neg,Pl> => BIND ++
                                                    suffixStr v.h negativeSuffix +
                                                    suffixStr negHar p2PlImperSuffix
                                      } ;
                            Imperf => v.stems ! VBase Soft ++ BIND ++
                                      case <p,n> of {
                                        <Pos,Sg> => suffixStr v.h progrSuffix ;
                                        <Neg,Sg> => suffixStr v.h negativeSuffix +
                                                    suffixStr negHar progrSuffix ;
                                        <Pos,Pl> => suffixStr v.h progrSuffix +
                                                    suffixStr progrHar p2PlImperSuffix ;
                                        <Neg,Pl> => suffixStr v.h negativeSuffix +
                                                    suffixStr negHar progrSuffix +
                                                    suffixStr progrHar p2PlImperSuffix
                                      }
                          } ;
              VFin t a p agr =>
                let x = case a of {
                          Simul => {p=p; v=v} ;
                          Anter => let part = 
                                         v.stems ! VBase Hard ++ BIND ++
                                         case p of {
                                           Pos   => suffixStr v.h perfParticipleSuffix ;
                                           Neg   => suffixStr v.h negativeSuffix +
                                                    suffixStr negHar perfParticipleSuffix
                                         }
                                   in {p=p; v=olmak_V ** {
                                                s = part ++ olmak_V.s ;
                                                stems = \\vf => part ++ olmak_V.stems ! vf
                                              }}
                         } ;
                    presHar = mkHar (case x.v.h.con of {
                                       SCon _ => case x.v.aoristType of {
                                                   SgSylConReg => Ih_Har ;
                                                   _           => x.v.h.vow
                                                 } ;
                                       SVow   => x.v.h.vow
                                     }) (SCon Soft) ;
                    pastHar = mkHar x.v.h.vow SVow ;
                    pastIHar= mkHar U_Har SVow ;
                    futSoft = (verbSuffixes ! agr).stemT ;
                    futHar  = mkHar (case x.v.h.vow of {
                                       I_Har  | U_Har  => I_Har ;
                                       Ih_Har | Uh_Har => Ih_Har
                                     }) (SCon futSoft) ;
                    presNegHar =
                              mkHar negHar.vow (SCon Soft)
                in case x.p of {
                     Pos => case t of {
                              Pres => case asp of {
                                        Perf =>   x.v.stems ! VBase Soft ++ BIND ++
                                                  suffixStr x.v.h (case x.v.aoristType of {
                                                                     SgSylConReg => aoristErSuffix ;
                                                                     _           => aoristIrSuffix
                                                                   }) +
                                                  suffixStr presHar (verbSuffixes ! agr) ;
                                        Imperf => x.v.stems ! VProg ++ BIND ++
                                                  suffixStr x.v.h progrSuffix +
                                                  suffixStr progrHar (verbSuffixes ! agr)
                                      } ;
                              Past => case asp of {
                                        Perf => x.v.stems ! VBase Hard ++ BIND ++
                                                suffixStr x.v.h pastSuffix +
                                                suffixStr pastHar (verbSuffixes ! agr) ;
                                        Imperf => x.v.stems ! VProg ++ BIND ++
                                                  suffixStr x.v.h progrSuffix +
                                                  case agr of {
                                                    {n=Pl; p=P3} => suffixStr progrHar (verbSuffixes ! agr) +
                                                                    suffixStr (mkHar Ih_Har (SCon Soft)) pastSuffix ;
                                                    _            => suffixStr progrHar pastSuffix +
                                                                    suffixStr pastIHar (verbSuffixes ! agr)
                                                  }
                                      } ;
                              Fut  => x.v.stems ! VFuture ++ BIND ++
                                      suffixStr x.v.h (case futSoft of {
                                                       Soft => softFutureSuffix ;
                                                       Hard => futureSuffix
                                                     }) +
                                      suffixStr futHar (verbSuffixes ! agr) ;
                              Cond => x.v.stems ! VBase Hard ++ BIND ++
                                      suffixStr x.v.h (condCopulaSuffixes ! agr)
                            } ;
                     Neg => case t of {
                              Pres => case asp of {
                                        Perf =>   x.v.stems ! VBase Hard ++ BIND ++
                                                  suffixStr x.v.h negativeSuffix +
                                                  case agr of {
                                                    {n=Sg; p=P1} => suffixStr negHar (verbSuffixes ! agr) ;
                                                    {n=Pl; p=P1} => suffixStr negHar p1PlAoristSuffix ;
                                                    _            => suffixStr negHar aoristIzSuffix + suffixStr presNegHar (verbSuffixes ! agr)
                                                  } ;
                                        Imperf => x.v.stems ! VBase Soft ++ BIND ++
                                                  tk 1 (suffixStr x.v.h negativeSuffix) +
                                                  suffixStr x.v.h progrSuffix +
                                                  suffixStr progrHar (verbSuffixes ! agr)
                                      } ;
                              Past => case asp of {
                                        Perf =>   x.v.stems ! VBase Hard ++ BIND ++
                                                  suffixStr x.v.h negativeSuffix +
                                                  suffixStr negHar pastSuffix +
                                                  suffixStr negHar (verbSuffixes ! agr) ;
                                        Imperf => x.v.stems ! VBase Soft ++ BIND ++
                                                  tk 1 (suffixStr x.v.h negativeSuffix) +
                                                  suffixStr x.v.h progrSuffix +
                                                  suffixStr progrHar pastSuffix +
                                                  suffixStr pastIHar (verbSuffixes ! agr)
                                      } ;
                              Fut  => x.v.stems ! VBase Hard ++ BIND ++
                                      suffixStr x.v.h negativeSuffix +
                                      suffixStr negHar (case (verbSuffixes ! agr).stemT of {
                                                          Soft => softFutureSuffix ;
                                                          Hard => futureSuffix
                                                        }) +
                                      suffixStr futHar (verbSuffixes ! agr) ;
                              Cond => x.v.stems ! VBase Hard ++ BIND ++
                                      suffixStr x.v.h negativeSuffix +
                                      suffixStr negHar (condCopulaSuffixes ! agr)
                            }
                   }
          }
        where {
          negHar = mkHar (case v.h.vow of {
                            I_Har  | U_Har  => I_Har ;
                            Ih_Har | Uh_Har => Ih_Har
                          }) SVow ;
          progrHar = mkHar U_Har (SCon Soft)
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

    olmak_V : Verb = {
      s = "olmak" ;
      stems = \\_ => "ol" ;
      aoristType = SgSylConIrreg ;
      h = mkHar U_Har (SCon Soft)
    } ;
}
