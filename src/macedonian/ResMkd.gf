resource ResMkd = open Prelude in {

oper Compl = {s : Str} ;

param Species = Indef | Def Distance ;
param Distance = Unspecified | Distal | Proximal ;
param Number = Sg | Pl ;
param NRelType = Pref | AdjMod | AdvMod ;
param Gender = Masc | Fem | Neuter ;
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
param Person = P1 | P3 | P2 ;
param GenNum = GSg Gender | GPl ;
oper Verb = {present: Aspect => Number => Person => Str; aorist: Number => Person => Str; imperfect: Aspect => Number => Person => Str; Imperative: Aspect => Number => Str; participle: {aorist: Aspect => GenNum => Str; imperfect: GenNum => Str; perfect: Aspect => Str; adjectival: Aspect => Str; adverbial: Str}; noun_from_verb: Str; isRefl: Bool} ; -- 8174
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
            Imperative = \\_ =>
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
                           imperfect = table {
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
            isRefl = False
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
oper Pron = {s : Role => Str; clitic : Case => Str} ;

genNum : Gender -> Number -> GenNum = \g,n ->
  case n of {
    Sg => GSg g ;
    Pl => GPl
  } ;

}
