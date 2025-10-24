resource ResMkd = ParamX - [Tense] ** open Prelude, Predef in {

oper Compl = {s : Str; c : Case} ;

param Species = Indef | Def Distance ;
param Distance = Unspecified | Distal | Proximal ;
param NRelType = Pref | AdjMod | AdvMod ;
      NNumber =
          NNum Number
        | NCountable
        ;
param Gender = Masc | Fem | Neuter ;
oper Agr = {g : GenNum; p : Person} ;
oper Noun = {s: Species => Number => Str; count_form: Str; vocative: Number => Str; rel: Species => GenNum => Str; relType : NRelType; g: Gender} ; -- 24855
oper mkNoun : (_,_,_,_,_,_,_,_,_,_,_ : Str) -> Gender -> Noun =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,g ->
          { s = table {
                  Indef => table {
                             Sg => f1 ;
                             Pl => f2
                           } ;
                  Def Unspecified => table {
                                       Sg => f3 ;
                                       Pl => f4
                                     } ;
                  Def Proximal => table {
                                    Sg => f5 ;
                                    Pl => f6
                                  } ;
                  Def Distal => table {
                                  Sg => f7 ;
                                  Pl => f8
                                }
                } ;
            count_form = f9 ;
            vocative = table {
                         Sg => f10 ;
                         Pl => f11
                       } ;
            rel = \\_,_ => f1 ;
            relType = Pref ;
            g = g
          } ;


param Aspect = Imperfective | Perfective ;
param GenNum = GSg Gender | GPl ;
param VType = VNormal | VMedial Case ;
      Tense = VPresent
            | VPastSimple     --# notpresent
            | VPastImperfect  --# notpresent
            | VFut            --# notpresent
            | VCond           --# notpresent
            ;

      Order = Main | Quest ;

oper Verb = {present: Aspect => Number => Person => Str; aorist: Number => Person => Str; imperfect: Aspect => Number => Person => Str; imperative: Aspect => Number => Str; participle: {aorist: Aspect => GenNum => Str; imperfect: Aspect => GenNum => Str; perfect: Aspect => Str; adjectival: Aspect => Str; adverbial: Str}; noun_from_verb: Str; vtype: VType} ; -- 8174
oper mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32 ->
          { present = \\_ =>
                      table {
                        Sg => table {
                                P1 => f1 ;
                                P2 => f2 ;
                                P3 => f3
                              } ;
                        Pl => table {
                                P1 => f4 ;
                                P2 => f5 ;
                                P3 => f6
                              }
                      } ;
            aorist = table {
                       Sg => table {
                               P1 => f7 ;
                               P2 => f8 ;
                               P3 => f9
                             } ;
                       Pl => table {
                               P1 => f10 ;
                               P2 => f11 ;
                               P3 => f12
                             }
                     } ;
            imperfect = \\_ =>
                        table {
                          Sg => table {
                                  P1 => f13 ;
                                  P2 => f14 ;
                                  P3 => f15
                                } ;
                          Pl => table {
                                  P1 => f16 ;
                                  P2 => f17 ;
                                  P3 => f18
                                }
                        } ;
            imperative = \\_ =>
                         table {
                           Sg => f19 ;
                           Pl => f20
                         } ;
            participle = { aorist = \\_ =>
                                    table {
                                      GSg Masc => f21 ;
                                      GSg Fem => f22 ;
                                      GSg Neuter => f23 ;
                                      GPl => f24
                                    } ;
                           imperfect = \\_ =>
                                       table {
                                         GSg Masc => f25 ;
                                         GSg Fem => f26 ;
                                         GSg Neuter => f27 ;
                                         GPl => f28
                                       } ;
                           perfect = \\_ => f29 ;
                           adjectival = \\_ => f30 ;
                           adverbial = f31
                         } ;
            noun_from_verb = f32 ;
            vtype = VNormal
          } ;


oper Adj = {s: Species => GenNum => Str; adverb: Str} ; -- 15929
oper mkAdj : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Adj =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17 ->
          { s = table {
                  Indef => table {
                             GSg Masc => f1 ;
                             GSg Fem => f2 ;
                             GSg Neuter => f3 ;
                             GPl => f4
                           } ;
                  Def Unspecified => table {
                                       GSg Masc => f5 ;
                                       GSg Fem => f6 ;
                                       GSg Neuter => f7 ;
                                       GPl => f8
                                     } ;
                  Def Proximal => table {
                                    GSg Masc => f9 ;
                                    GSg Fem => f10 ;
                                    GSg Neuter => f11 ;
                                    GPl => f12
                                  } ;
                  Def Distal => table {
                                  GSg Masc => f13 ;
                                  GSg Fem => f14 ;
                                  GSg Neuter => f15 ;
                                  GPl => f16
                                }
                } ;
            adverb = f17
          } ;



oper Adv = {s: Str} ; -- 1963
oper mkAdv : Str -> Adv =
       \f1 ->
          { s = f1
          } ;


param Case = Acc | Dat ;
param Role = RSubj | RObj Case | RPrep ;
oper Pronoun = {
        s : Role => Str;
        clitic : Case => Str;
        poss : Species => GenNum => Str ;
        poss_clitic : Str ;
        a : {g : GenNum; p : Person}
     } ;

conjGenNum : GenNum -> GenNum -> GenNum = \a,b ->
  case <a,b> of {
    <GSg _,GSg g> => GSg g ;
    _             => GPl
  } ;

genNum : Gender -> Number -> GenNum = \g,n ->
  case n of {
    Sg => GSg g ;
    Pl => GPl
  } ;
  
nnum2num : NNumber -> Number = \n ->
  case n of {
    NNum n => n ;
    NCountable => Pl
  } ;

genNum2num : GenNum -> Number = \gn ->
  case gn of {
    GSg _ => Sg ;
    GPl   => Pl
  } ;

auxBe = {
  present : Number => Person => Str
          = table {
              Sg => table {
                      P1 => "сум" ;
                      P2 => "си" ;
                      P3 => "е"
                    } ;
              Pl => table {
                      P1 => "сме" ;
                      P2 => "сте" ;
                      P3 => "се"
                    }
            } ;
  imperfect : Number => Person => Str
          = table {
              Sg => table {
                      P1 => "бев" ;
                      P2 => "беше" ;
                      P3 => "беше"
                    } ;
              Pl => table {
                      P1 => "бевме" ;
                      P2 => "бевте" ;
                      P3 => "беа"
                    }
            } ;
  imperative : Number => Str =
            table {
              Sg => "биди" ;
              Pl => "бидете"
            } ;
  participle = {
    aorist : GenNum => Str
           = table {
               GSg Masc => "бил" ;
               GSg Fem => "била" ;
               GSg Neuter => "било" ;
               GPl => "биле"
             } ;
    imperfect : GenNum => Str
           = table {
               GSg Masc => "бидел" ;
               GSg Fem => "бидела" ;
               GSg Neuter => "бидело" ;
               GPl => "биделе"
             }
    }
} ;

auxHave = {
  present : Number => Person => Str
          = table {
              Sg => table {
                      P1 => "имам" ;
                      P2 => "имаш" ;
                      P3 => "има"
                    } ;
              Pl => table {
                      P1 => "имаме" ;
                      P2 => "имате" ;
                      P3 => "имаат"
                    }
            } ;
  imperfect : Number => Person => Str
          = table {
              Sg => table {
                      P1 => "имав" ;
                      P2 => "имаше" ;
                      P3 => "имаше"
                    } ;
              Pl => table {
                      P1 => "имавме" ;
                      P2 => "имавте" ;
                      P3 => "имаа"
                    }
            } ;
  imperative : Number => Str =
            table {
              Sg => "имај" ;
              Pl => "имајте"
            } ;
  participle = {
    imperfect : GenNum => Str
           = table {
               GSg Masc => "имал" ;
               GSg Fem => "имала" ;
               GSg Neuter => "имало" ;
               GPl => "имале"
             }
    }
} ;

mkClause : Str -> Agr -> Verb ** {compl : Agr => Str} -> Order => Tense => Anteriority => Polarity => Str
   = \subj,agr,vp ->
         let n = case agr.g of {
                   GSg _ => Sg ;
                   GPl   => Pl
                 }
         in \\o,t,a,p =>
                case <t,a> of {
                  <VPresent,Simul> => subj ++ neg ++ se ++ vp.present ! Imperfective ! n ! agr.p ++ li ++ vp.compl ! agr ;
                  <VPresent,Anter> => case o of {
                                        Main => subj ++ neg ++ auxBe.present ! n ! agr.p ++ se ++ vp.participle.imperfect ! Perfective ! agr.g ++ vp.compl ! agr ;
                                        Quest => subj ++ neg ++ se ++ vp.participle.imperfect ! Perfective ! agr.g ++ li ++ auxBe.present ! n ! agr.p ++ vp.compl ! agr
                                      } ;
                  <VPastSimple,Simul> => subj ++ neg ++ se ++ vp.aorist ! n ! agr.p ++ li ++ vp.compl ! agr ;
                  <VPastSimple,Anter> => subj ++ neg ++ se ++ auxBe.imperfect ! n ! agr.p ++ li ++ vp.participle.imperfect ! Perfective ! agr.g ++ vp.compl ! agr ;
                  <VPastImperfect,Simul> => subj ++ neg ++ se ++ vp.imperfect ! Perfective ! n ! agr.p ++ li ++ vp.compl ! agr ;
                  <VPastImperfect,Anter> => subj ++ neg ++ se ++ auxBe.imperfect ! n ! agr.p ++ li ++ vp.participle.imperfect ! Perfective ! agr.g ++ vp.compl ! agr ;
                  <VFut, Simul> => subj ++ fut.p1 ++ se ++ vp.present ! Perfective ! n ! agr.p ++ fut.p2 ++ vp.compl ! agr ;
                  <VFut, Anter> => subj ++ fut.p1 ++ se ++ auxHave.present ! n ! agr.p ++ fut.p2 ++ vp.participle.perfect ! Perfective ++ vp.compl ! agr ;
                  <VCond,Simul> => subj ++ neg ++ "би" ++ li ++ se ++ vp.participle.imperfect ! Perfective ! agr.g ++ vp.compl ! agr ;
                  <VCond,Anter> => subj ++ neg ++ "би" ++ li ++ auxHave.participle.imperfect ! agr.g ++ se ++ vp.participle.perfect ! Perfective ++ vp.compl ! agr
                } where {
                    neg = case p of {
                            Pos => "" ;
                            Neg => "не"
                          } ;
                    fut = case <p,o> of {
                            <Pos,Main> => <"ке",[]> ;
                            <Neg,Main> => <"нема да",[]> ;
                            <Pos,Quest> => <"ке","ли"> ;
                            <Neg,Quest> => <"нема ли да",[]>
                          } ;
                    li  = case o of {
                            Main  => "" ;
                            Quest => "ли"
                          } ;
                    se  = case vp.vtype of {
                            VNormal     => "" ;
                            VMedial Acc => "се" ;
                            VMedial Dat => "си"
                          }
                } ;

linCoord : Str -> Ints 4 => Str ;
linCoord comma = table {0 => "и"; 1=>"или"; 2=>"ниту"; 3=>comma; 4=>[]} ;

}
