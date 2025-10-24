--# -path=.:abstract:common:prelude

-- Contents of this file are automatically ported paradigms from
-- https://github.com/PeterisP/morphology/blob/master/src/main/resources/Lexicon_v2.xml
-- NB: Do NOT edit this without consulting lauma@ailab.lv or normundsg@ailab.lv
--     Otherwise your changes might get accidentally revoked!

resource PortedMorphoParadigmsLav = open PortedMorphoStemchangesLav, ResLav in {

flags coding = utf8 ;

oper

  noun_1a : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "us" ;
        Dat => stem + "iem" ;
        Loc => stem + "os" ;
        Nom => stem + "i" ;
        Voc => stem + "i" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "u" ;
        Dat => stem + "am" ;
        Loc => stem + "ā" ;
        Nom => stem + "s" ;
        Voc => variants { stem + "s" ; stem + "" } ;
        Gen => stem + "a"
      }
    } ;
    gend = Masc
  } ;

  noun_1b : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "us" ;
        Dat => stem + "iem" ;
        Loc => stem + "os" ;
        Nom => stem + "i" ;
        Voc => stem + "i" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "u" ;
        Dat => stem + "am" ;
        Loc => stem + "ā" ;
        Nom => stem + "š" ;
        Voc => variants { stem + "š" ; stem + "" } ;
        Gen => stem + "a"
      }
    } ;
    gend = Masc
  } ;

  noun_2a : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stemchangeSimple 1 stem + "us" ;
        Dat => stemchangeSimple 1 stem + "iem" ;
        Loc => stemchangeSimple 1 stem + "os" ;
        Nom => stemchangeSimple 1 stem + "i" ;
        Voc => stemchangeSimple 1 stem + "i" ;
        Gen => stemchangeSimple 1 stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "im" ;
        Loc => stem + "ī" ;
        Nom => stem + "is" ;
        Voc => stem + "i" ;
        Gen => stemchangeSimple 1 stem + "a"
      }
    } ;
    gend = Masc
  } ;

  noun_2c : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stemchangeSimple 1 stem + "us" ;
        Dat => stemchangeSimple 1 stem + "iem" ;
        Loc => stemchangeSimple 1 stem + "os" ;
        Nom => stemchangeSimple 1 stem + "i" ;
        Voc => stemchangeSimple 1 stem + "i" ;
        Gen => stemchangeSimple 1 stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "im" ;
        Loc => stem + "ī" ;
        Nom => stem + "s" ;
        Voc => variants { stem + "" ; stem + "s" } ;
        Gen => stem + "s"
      }
    } ;
    gend = Masc
  } ;

  noun_2d : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stemchangeSimple 1 stem + "us" ;
        Dat => stemchangeSimple 1 stem + "iem" ;
        Loc => stemchangeSimple 1 stem + "os" ;
        Nom => stemchangeSimple 1 stem + "i" ;
        Voc => stemchangeSimple 1 stem + "i" ;
        Gen => stemchangeSimple 1 stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "im" ;
        Loc => stem + "ī" ;
        Nom => stem + "s" ;
        Voc => stem + "i" ;
        Gen => stemchangeSimple 1 stem + "a"
      }
    } ;
    gend = Masc
  } ;

  noun_3m : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "us" ;
        Dat => stem + "iem" ;
        Loc => stem + "os" ;
        Nom => stem + "i" ;
        Voc => stem + "i" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "u" ;
        Dat => stem + "um" ;
        Loc => stem + "ū" ;
        Nom => stem + "us" ;
        Voc => variants { stem + "us" ; stem + "u" } ;
        Gen => stem + "us"
      }
    } ;
    gend = Masc
  } ;

  noun_4f : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "as" ;
        Dat => stem + "ām" ;
        Loc => stem + "ās" ;
        Nom => stem + "as" ;
        Voc => stem + "as" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "u" ;
        Dat => stem + "ai" ;
        Loc => stem + "ā" ;
        Nom => stem + "a" ;
        Voc => variants { stem + "a" ; stemchangeSimple 17 stem + "" } ;
        Gen => stem + "as"
      }
    } ;
    gend = Fem
  } ;

  noun_4m : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "as" ;
        Dat => stem + "ām" ;
        Loc => stem + "ās" ;
        Nom => stem + "as" ;
        Voc => stem + "as" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "u" ;
        Dat => stem + "am" ;
        Loc => stem + "ā" ;
        Nom => stem + "a" ;
        Voc => stem + "a" ;
        Gen => stem + "as"
      }
    } ;
    gend = Masc
  } ;

  noun_5fa : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "es" ;
        Dat => stem + "ēm" ;
        Loc => stem + "ēs" ;
        Nom => stem + "es" ;
        Voc => stem + "es" ;
        Gen => stemchangeSimple 1 stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "ei" ;
        Loc => stem + "ē" ;
        Nom => stem + "e" ;
        Voc => variants { stem + "e" ; stemchangeSimple 17 stem + "" } ;
        Gen => stem + "es"
      }
    } ;
    gend = Fem
  } ;

  noun_5ma : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "es" ;
        Dat => stem + "ēm" ;
        Loc => stem + "ēs" ;
        Nom => stem + "es" ;
        Voc => stem + "es" ;
        Gen => stemchangeSimple 1 stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "em" ;
        Loc => stem + "ē" ;
        Nom => stem + "e" ;
        Voc => stem + "e" ;
        Gen => stem + "es"
      }
    } ;
    gend = Masc
  } ;

  noun_6a : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "is" ;
        Dat => stem + "īm" ;
        Loc => stem + "īs" ;
        Nom => stem + "is" ;
        Voc => stem + "is" ;
        Gen => stemchangeSimple 1 stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "ij" ;
        Loc => stem + "ī" ;
        Nom => stem + "s" ;
        Voc => stem + "s" ;
        Gen => stem + "s"
      }
    } ;
    gend = Fem
  } ;

  noun_3f : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "us" ;
        Dat => stem + "ūm" ;
        Loc => stem + "ūs" ;
        Nom => stem + "us" ;
        Voc => stem + "us" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "u" ;
        Dat => stem + "ui" ;
        Loc => stem + "ū" ;
        Nom => stem + "us" ;
        Voc => variants { stem + "us" ; stem + "u" } ;
        Gen => stem + "us"
      }
    } ;
    gend = Fem
  } ;

  noun_6b : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "is" ;
        Dat => stem + "īm" ;
        Loc => stem + "īs" ;
        Nom => stem + "is" ;
        Voc => stem + "is" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "ij" ;
        Loc => stem + "ī" ;
        Nom => stem + "s" ;
        Voc => stem + "s" ;
        Gen => stem + "s"
      }
    } ;
    gend = Fem
  } ;

  noun_5fb : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "es" ;
        Dat => stem + "ēm" ;
        Loc => stem + "ēs" ;
        Nom => stem + "es" ;
        Voc => stem + "es" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "ei" ;
        Loc => stem + "ē" ;
        Nom => stem + "e" ;
        Voc => variants { stem + "e" ; stemchangeSimple 17 stem + "" } ;
        Gen => stem + "es"
      }
    } ;
    gend = Fem
  } ;

  noun_5mb : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "es" ;
        Dat => stem + "ēm" ;
        Loc => stem + "ēs" ;
        Nom => stem + "es" ;
        Voc => stem + "es" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "em" ;
        Loc => stem + "ē" ;
        Nom => stem + "e" ;
        Voc => stem + "e" ;
        Gen => stem + "es"
      }
    } ;
    gend = Masc
  } ;

  noun_2b : Str -> Noun = \stem ->
  {
    s = table {
      Pl => table {
        Acc => stem + "us" ;
        Dat => stem + "iem" ;
        Loc => stem + "os" ;
        Nom => stem + "i" ;
        Voc => stem + "i" ;
        Gen => stem + "u"
      } ;
      Sg => table {
        Acc => stem + "i" ;
        Dat => stem + "im" ;
        Loc => stem + "ī" ;
        Nom => stem + "is" ;
        Voc => stem + "i" ;
        Gen => variants { stem + "a" ; stemchangeSimple 1 stem + "a" }
      }
    } ;
    gend = Masc
  } ;
}

