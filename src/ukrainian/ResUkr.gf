resource ResUkr = open (R = ParamX), Prelude in {

param Case = Nom | Acc | Dat | Gen | Loc | Instr ;
param Number = Sg | Pl ;
param Gender = Masc | Neuter | Fem ;
oper N = {s: Case => Number => Str; voc: Number => Str; g: Gender} ; -- 11407
oper mkN : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Gender -> N =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,g ->
          { s = table {
                  Nom => table {
                           Sg => f1 ;
                           Pl => f2
                         } ;
                  Acc => table {
                           Sg => f3 ;
                           Pl => f4
                         } ;
                  Dat => table {
                           Sg => f5 ;
                           Pl => f6
                         } ;
                  Gen => table {
                           Sg => f7 ;
                           Pl => f8
                         } ;
                  Loc => table {
                           Sg => f9 ;
                           Pl => f10
                         } ;
                  Instr => table {
                             Sg => f11 ;
                             Pl => f12
                           }
                } ;
            voc = table {
                    Sg => f13 ;
                    Pl => f14
                  } ;
            g = g
          } ;


param Aspect = Perf | Imperf ;
param Person = P1 | P2 | P3 ;
param Tense = Past | Pres ;
oper V = {active: Aspect => {past: Str; pres: Person => Number => Str}; imperative1: Str; imperative2: Number => Str; infinitive: Str; participle: Gender => Number => Str; passive: Aspect => Tense => Str} ; -- 4822
oper mkV : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> V =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28 ->
          { active = table {
                       Imperf => { past = f1 ;
                                   pres = table {
                                            P1 => table {
                                                    Sg => f2 ;
                                                    Pl => f3
                                                  } ;
                                            P2 => table {
                                                    Sg => f4 ;
                                                    Pl => f5
                                                  } ;
                                            P3 => table {
                                                    Sg => f6 ;
                                                    Pl => f7
                                                  }
                                          }
                                 } ;
                       Perf => { past = f8 ;
                                 pres = table {
                                          P1 => table {
                                                  Sg => f9 ;
                                                  Pl => f10
                                                } ;
                                          P2 => table {
                                                  Sg => f11 ;
                                                  Pl => f12
                                                } ;
                                          P3 => table {
                                                  Sg => f13 ;
                                                  Pl => f14
                                                }
                                        }
                               }
                     } ;
            imperative1 = f15 ;
            imperative2 = table {
                            Sg => f16 ;
                            Pl => f17
                          } ;
            infinitive = f18 ;
            participle = table {
                           Masc => table {
                                     Sg => f19 ;
                                     Pl => f20
                                   } ;
                           Fem => table {
                                    Sg => f21 ;
                                    Pl => f22
                                  } ;
                           Neuter => table {
                                       Sg => f23 ;
                                       Pl => f24
                                     }
                         } ;
            passive = table {
                        Imperf => table {
                                    Pres => f25 ;
                                    Past => f26
                                  } ;
                        Perf => table {
                                  Pres => f27 ;
                                  Past => f28
                                }
                      }
          } ;

param GenNum = GSg Gender | GPl ;
oper genNum : Gender -> Number -> GenNum = \g,n ->
       case n of {
         Sg => GSg g ;
         Pl => GPl
       } ;

oper A = {s: Case => GenNum => Str} ; -- 4394
oper mkA : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> A =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24 ->
          { s = table {
                  Nom => table {
                           GSg Masc => f1 ;
                           GSg Fem => f2 ;
                           GSg Neuter => f3 ;
                           GPl => f4
                         } ;
                  Acc => table {
                           GSg Masc => f5 ;
                           GSg Fem => f6 ;
                           GSg Neuter => f7 ;
                           GPl => f8
                         } ;
                  Dat => table {
                           GSg Masc => f9 ;
                           GSg Fem => f10 ;
                           GSg Neuter => f11 ;
                           GPl => f12
                         } ;
                  Gen => table {
                           GSg Masc => f13 ;
                           GSg Fem => f14 ;
                           GSg Neuter => f15 ;
                           GPl => f16
                         } ;
                  Loc => table {
                           GSg Masc => f17 ;
                           GSg Fem => f18 ;
                           GSg Neuter => f19 ;
                           GPl => f20
                         } ;
                  Instr => table {
                             GSg Masc => f21 ;
                             GSg Fem => f22 ;
                             GSg Neuter => f23 ;
                             GPl => f24
                           }
                }
          } ;


oper Compl = {s : Str; c : Case} ;
oper noPrep : Compl = {s=""; c=Nom} ;

oper CommonNoun = N ;
oper AdjPhrase = A ;

oper Agr = {g : Gender; n : Number; p : Person} ;
oper agrP3 : Gender -> Number -> Agr = \g,n -> {g=g; n=n; p=P3} ;
oper defaultAgr : Agr = agrP3 Masc Sg ;

oper neg : R.Polarity -> Str = \p -> case p of {
  R.Pos => [] ;
  R.Neg => "не"
  } ;

oper auxBe : R.Tense -> Number -> Person -> Str =
  \t,n,p -> case t of {
    R.Pres => [] ;
    R.Past => case n of {
      Sg => "був" ;
      Pl => "були"
      } ;
    R.Fut => case <p,n> of {
      <P1,Sg> => "буду" ;
      <P2,Sg> => "будеш" ;
      <P3,Sg> => "буде" ;
      <P1,Pl> => "будемо" ;
      <P2,Pl> => "будете" ;
      <P3,Pl> => "будуть"
      } ;
    R.Cond => case n of {
      Sg => "був би" ;
      Pl => "були б"
      }
    } ;

oper finiteVerb : V -> R.Tense -> R.Polarity -> Gender -> Number -> Person -> Str =
  \v,t,pol,g,n,p -> neg pol ++ case t of {
    R.Pres => (v.active ! Imperf).pres ! p ! n ;
    R.Past => (v.active ! Imperf).pres ! p ! n ;
    R.Fut  => auxBe R.Fut n p ++ v.infinitive ;
    R.Cond => (v.active ! Imperf).pres ! p ! n ++ "би"
    } ;

oper copula : R.Tense -> R.Polarity -> Gender -> Number -> Person -> Str =
  \t,pol,g,n,p -> neg pol ++ auxBe t n p ;

oper prepNP : Compl -> {s : Case => Str} -> Str =
  \prep,np -> prep.s ++ np.s ! prep.c ;

oper constN : Str -> Gender -> N =
  \s,g -> {
    s = \\_,_ => s ;
    voc = \\_ => s ;
    g = g
  } ;

oper possPron : Person -> Gender -> Number -> Gender -> Number -> Str =
  \p,pg,pn,g,n -> case <p,pg,pn,g,n> of {
    <P1,_,Sg,Masc,Sg> => "мій" ;
    <P1,_,Sg,Fem,Sg> => "моя" ;
    <P1,_,Sg,Neuter,Sg> => "моє" ;
    <P1,_,Sg,_,Pl> => "мої" ;
    <P1,_,Pl,Masc,Sg> => "наш" ;
    <P1,_,Pl,Fem,Sg> => "наша" ;
    <P1,_,Pl,Neuter,Sg> => "наше" ;
    <P1,_,Pl,_,Pl> => "наші" ;
    <P2,_,Sg,Masc,Sg> => "твій" ;
    <P2,_,Sg,Fem,Sg> => "твоя" ;
    <P2,_,Sg,Neuter,Sg> => "твоє" ;
    <P2,_,Sg,_,Pl> => "твої" ;
    <P2,_,Pl,Masc,Sg> => "ваш" ;
    <P2,_,Pl,Fem,Sg> => "ваша" ;
    <P2,_,Pl,Neuter,Sg> => "ваше" ;
    <P2,_,Pl,_,Pl> => "ваші" ;
    <P3,Masc,Sg,_,_> => "його" ;
    <P3,Fem,Sg,_,_> => "її" ;
    <P3,_,Pl,_,_> => "їхній" ;
    _ => "свій"
  } ;

}
