resource ResFao = ParamX - [Tense,Pres,Past] ** {

param
  CardOrd = NCard | NOrd Number ;

param Species = Indef | Def ;
param Case = Nom | Acc | Dat | Gen ;
param Gender = Masc | Fem | Neuter ;
oper Noun = {s: Species => Number => Case => Str; g : Gender} ; -- 2135
oper mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Gender -> Noun =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,g ->
          { s = table {
                  Indef => table {
                             Sg => table {
                                     Nom => f1 ;
                                     Acc => f2 ;
                                     Dat => f3 ;
                                     Gen => f4
                                   } ;
                             Pl => table {
                                     Nom => f5 ;
                                     Acc => f6 ;
                                     Dat => f7 ;
                                     Gen => f8
                                   }
                           } ;
                  Def => table {
                           Sg => table {
                                   Nom => f9 ;
                                   Acc => f10 ;
                                   Dat => f11 ;
                                   Gen => f12
                                 } ;
                           Pl => table {
                                   Nom => f13 ;
                                   Acc => f14 ;
                                   Dat => f15 ;
                                   Gen => f16
                                 }
                         }
                } ;
            g = g
          } ;


param Declension = Strong | Weak ;
oper Adj = {
  s : Declension => Gender => Number => Case => Str ;
} ; -- 346

-- The weak adjective is used attributively in definite noun phrases.  The
-- generated morphology contains the strong paradigm; the three weak forms
-- can be recovered from it (e.g. langa -> langi, longum -> longu).
oper weakAdj : Adj -> Gender -> Number -> Case -> Str =
  \a,g,n,c -> a.s ! Weak ! g ! n ! c ;

oper weakAdjForms : Str -> Gender => Number => Case => Str = \lemma ->
  let stem : Str = case lemma of {
    x + "ur" => x ;
    x + "nin" => x + "n" ;
    x + "in" => x + "n" ;
    x + "il" => x + "il" ;
    x => x
  } in table {
    Masc => table {
      Sg => table {Nom => stem + "i" ; _ => stem + "a"} ;
      Pl => \\_ => stem + "u"
    } ;
    _ => table {
      Sg => \\_ => stem + "a" ;
      Pl => \\_ => stem + "u"
    }
  } ;

oper reflPoss : Gender -> Number -> Case -> Str = \g,n,c ->
  case <g,n,c> of {
    <Masc,Sg,Nom> => "sín" ; <Masc,Sg,Acc> => "sín" ;
    <Masc,Sg,Dat> => "sínum" ; <Masc,Sg,Gen> => "síns" ;
    <Fem,Sg,Nom> => "sín" ; <Fem,Sg,Acc> => "sína" ;
    <Fem,Sg,Dat> => "síni" ; <Fem,Sg,Gen> => "sínar" ;
    <Neuter,Sg,Nom> => "sítt" ; <Neuter,Sg,Acc> => "sítt" ;
    <Neuter,Sg,Dat> => "sínum" ; <Neuter,Sg,Gen> => "síns" ;
    <Masc,Pl,Nom> => "sínir" ; <Masc,Pl,Acc> => "sínar" ;
    <Fem,Pl,Nom> => "sínar" ; <Fem,Pl,Acc> => "sínar" ;
    <Neuter,Pl,Nom> => "síni" ; <Neuter,Pl,Acc> => "síni" ;
    <_,Pl,Dat> => "sínum" ; <_,Pl,Gen> => "sína"
  } ;
oper mkAdj : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Adj =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24 ->
          { s = table {
              Strong => table {
                  Masc => table {
                            Sg => table {
                                    Nom => f1 ;
                                    Acc => f2 ;
                                    Dat => f3 ;
                                    Gen => f4
                                  } ;
                            Pl => table {
                                    Nom => f5 ;
                                    Acc => f6 ;
                                    Dat => f7 ;
                                    Gen => f8
                                  }
                          } ;
                  Fem => table {
                           Sg => table {
                                   Nom => f9 ;
                                   Acc => f10 ;
                                   Dat => f11 ;
                                   Gen => f12
                                 } ;
                           Pl => table {
                                   Nom => f13 ;
                                   Acc => f14 ;
                                   Dat => f15 ;
                                   Gen => f16
                                 }
                         } ;
                  Neuter=> table {
                             Sg => table {
                                     Nom => f17 ;
                                     Acc => f18 ;
                                     Dat => f19 ;
                                     Gen => f20
                                   } ;
                             Pl => table {
                                     Nom => f21 ;
                                     Acc => f22 ;
                                     Dat => f23 ;
                                     Gen => f24
                                   }
                           }
                } ;
              Weak => weakAdjForms f1
            }
          } ;

param Tense = Pres | Past ;

param PersNum = PSg Person | PPl ;
oper persNum : Number -> Person -> PersNum =
       \n,p -> case n of {
                 Sg => PSg p ;
                 Pl => PPl
               } ;
oper persNumNumber : PersNum -> Number =
       \pn -> case pn of {
                PSg _ => Sg ;
                PPl   => Pl
              } ;

oper Verb = {Converb: Str; imperative: Number => Str; Indicative: Tense => PersNum => Str; Nonfinite: Str; Participle: Tense => Str ; particle : Str} ; -- 596
oper mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14 ->
          { Converb = f1 ;
            imperative = table {
                           Sg => f2 ;
                           Pl => f3
                         } ;
            Indicative = table {
                           Pres => table {
                                     PSg P1 => f4 ;
                                     PSg P2 => f5 ;
                                     PSg P3 => f6 ;
                                     PPl => f7
                                   } ;
                           Past => table {
                                     PSg P1 => f8 ;
                                     PSg P2 => f9 ;
                                     PSg P3 => f10 ;
                                     PPl => f11
                                   }
                         } ;
            Nonfinite = f12 ;
            Participle = table {
                           Pres => f13 ;
                           Past => f14
                         } ;
           particle = []
          } ;


oper Compl = {s : Str; c : Case} ;
oper noPrep : Compl = {s=""; c=Acc} ;

oper CommonNoun = {
  s : Species => Number => Case => Str ;
  p : Number => Case => Str ;
  g : Gender
} ;
oper AdjPhrase = Adj ;
oper VerbPhrase = {
  Converb : Str ;
  Imperative : Number => Str ;
  Indicative : Tense => Polarity => Gender => PersNum => Str ;
  Finite : Tense => PersNum => Str ;
  Remainder : Polarity => Gender => PersNum => Str ;
  Nonfinite : Str ;
  Participle : Tense => Str ;
} ;
oper Clause = {
  Converb : Str ;
  Indicative : Tense => Polarity => Str ;
  Interrogative : Tense => Polarity => Str ;
  Future : Polarity => Str ;
  FutureInterrogative : Polarity => Str ;
  Conditional : Polarity => Str ;
  ConditionalInterrogative : Polarity => Str ;
  Anterior : Tense => Polarity => Str ;
  AnteriorInterrogative : Tense => Polarity => Str ;
  Nonfinite : Str ;
  Participle : Tense => Str
} ;

oper
  copula : Tense => PersNum => Str =
    table {
      Pres => table {
                PSg P1 => "eri" ;
                PSg P2 => "ert" ;
                PSg P3 => "er" ;
                PPl => "eru"
              } ;
      Past => table {
                PSg P1 => "var" ;
                PSg P2 => "vart" ;
                PSg P3 => "var" ;
                PPl => "vóru"
              }
    } ;

  futureAux : PersNum => Str = table {
    PSg P1 => "skal" ; PSg P2 => "skalt" ; PSg P3 => "skal" ; PPl => "skulu"
  } ;
  conditionalAux : PersNum => Str = table {
    PSg _ => "skuldi" ; PPl => "skuldu"
  } ;
  perfectAux : Tense => PersNum => Str = table {
    Pres => table {PSg P1 => "havi" ; PSg P2 => "hevur" ; PSg P3 => "hevur" ; PPl => "hava"} ;
    Past => table {PSg _ => "hevði" ; PPl => "høvdu"}
  } ;

oper
  negStr : Polarity -> Str = \pol -> case pol of {
    Pos => [] ;
    Neg => "ikki"
  } ;

  mkNP : Str -> Gender -> Number -> Person -> {s : Case => Str ; g : Gender ; n : Number ; p : Person} =
    \str,g,n,p -> {
      s = \\_ => str ;
      g = g ;
      n = n ;
      p = p
    } ;

  mkCN : Str -> Gender -> CommonNoun =
    \str,g -> {
      s = \\_,_,_ => str ;
      p = \\_,_ => str ;
      g = g
    } ;

  mkVP : Str -> VerbPhrase =
    \str -> {
      Converb = str ;
      Imperative = \\_ => str ;
      Indicative = \\_,pol,_,_ => str ++ negStr pol ;
      Finite = \\_,_ => str ;
      Remainder = \\pol,_,_ => negStr pol ;
      Nonfinite = str ;
      Participle = \\_ => str
    } ;

}
