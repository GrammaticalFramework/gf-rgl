--# -path=.:abstract:common:prelude

-- Contents of this file are automatically ported paradigms from
-- https://github.com/LUMII-AILab/Morphology/blob/master/src/main/resources/Lexicon_v2.xml
-- NB: Do NOT edit this without consulting lauma@ailab.lv or normundsg@ailab.lv
--     Otherwise your changes might get accidentally revoked!

resource PortedMorphoParadigmsLav = open PortedMorphoStemchangesLav, ResLav in {

flags coding = utf8 ;

oper

  noun_1a_fromStems : Str -> Noun = \stem ->
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

  noun_1a_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "s" => noun_1a_fromStems stem ;
      _ => Predef.error ("noun_1a_fromLemma is only applicable for words that end in -s, tried to apply to" ++ lemma)
    } ;

  noun_1a_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "i" => noun_1a_fromStems stem ;
      _ => Predef.error ("noun_1a_fromNomPl is only applicable for words that end in -i, tried to apply to" ++ lemma)
    } ;

  noun_1b_fromStems : Str -> Noun = \stem ->
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

  noun_1b_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "š" => noun_1b_fromStems stem ;
      _ => Predef.error ("noun_1b_fromLemma is only applicable for words that end in -š, tried to apply to" ++ lemma)
    } ;

  noun_1b_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "i" => noun_1b_fromStems stem ;
      _ => Predef.error ("noun_1b_fromNomPl is only applicable for words that end in -i, tried to apply to" ++ lemma)
    } ;

  noun_2a_fromStems : Str -> Noun = \stem ->
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

  noun_2a_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "is" => noun_2a_fromStems stem ;
      _ => Predef.error ("noun_2a_fromLemma is only applicable for words that end in -is, tried to apply to" ++ lemma)
    } ;

  noun_2a_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "i" => noun_2a_fromStems stem ;
      _ => Predef.error ("noun_2a_fromNomPl is only applicable for words that end in -i, tried to apply to" ++ lemma)
    } ;

  noun_2c_fromStems : Str -> Noun = \stem ->
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

  noun_2c_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "s" => noun_2c_fromStems stem ;
      _ => Predef.error ("noun_2c_fromLemma is only applicable for words that end in -s, tried to apply to" ++ lemma)
    } ;

  noun_2c_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "i" => noun_2c_fromStems stem ;
      _ => Predef.error ("noun_2c_fromNomPl is only applicable for words that end in -i, tried to apply to" ++ lemma)
    } ;

  noun_2d_fromStems : Str -> Noun = \stem ->
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

  noun_2d_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "s" => noun_2d_fromStems stem ;
      _ => Predef.error ("noun_2d_fromLemma is only applicable for words that end in -s, tried to apply to" ++ lemma)
    } ;

  noun_2d_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "i" => noun_2d_fromStems stem ;
      _ => Predef.error ("noun_2d_fromNomPl is only applicable for words that end in -i, tried to apply to" ++ lemma)
    } ;

  noun_3m_fromStems : Str -> Noun = \stem ->
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

  noun_3m_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "us" => noun_3m_fromStems stem ;
      _ => Predef.error ("noun_3m_fromLemma is only applicable for words that end in -us, tried to apply to" ++ lemma)
    } ;

  noun_3m_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "i" => noun_3m_fromStems stem ;
      _ => Predef.error ("noun_3m_fromNomPl is only applicable for words that end in -i, tried to apply to" ++ lemma)
    } ;

  noun_4f_fromStems : Str -> Noun = \stem ->
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

  noun_4f_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "a" => noun_4f_fromStems stem ;
      _ => Predef.error ("noun_4f_fromLemma is only applicable for words that end in -a, tried to apply to" ++ lemma)
    } ;

  noun_4f_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "as" => noun_4f_fromStems stem ;
      _ => Predef.error ("noun_4f_fromNomPl is only applicable for words that end in -as, tried to apply to" ++ lemma)
    } ;

  noun_4m_fromStems : Str -> Noun = \stem ->
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

  noun_4m_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "a" => noun_4m_fromStems stem ;
      _ => Predef.error ("noun_4m_fromLemma is only applicable for words that end in -a, tried to apply to" ++ lemma)
    } ;

  noun_4m_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "as" => noun_4m_fromStems stem ;
      _ => Predef.error ("noun_4m_fromNomPl is only applicable for words that end in -as, tried to apply to" ++ lemma)
    } ;

  noun_5fa_fromStems : Str -> Noun = \stem ->
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

  noun_5fa_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "e" => noun_5fa_fromStems stem ;
      _ => Predef.error ("noun_5fa_fromLemma is only applicable for words that end in -e, tried to apply to" ++ lemma)
    } ;

  noun_5fa_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "es" => noun_5fa_fromStems stem ;
      _ => Predef.error ("noun_5fa_fromNomPl is only applicable for words that end in -es, tried to apply to" ++ lemma)
    } ;

  noun_5ma_fromStems : Str -> Noun = \stem ->
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

  noun_5ma_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "e" => noun_5ma_fromStems stem ;
      _ => Predef.error ("noun_5ma_fromLemma is only applicable for words that end in -e, tried to apply to" ++ lemma)
    } ;

  noun_5ma_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "es" => noun_5ma_fromStems stem ;
      _ => Predef.error ("noun_5ma_fromNomPl is only applicable for words that end in -es, tried to apply to" ++ lemma)
    } ;

  noun_6a_fromStems : Str -> Noun = \stem ->
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

  noun_6a_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "s" => noun_6a_fromStems stem ;
      _ => Predef.error ("noun_6a_fromLemma is only applicable for words that end in -s, tried to apply to" ++ lemma)
    } ;

  noun_6a_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "is" => noun_6a_fromStems stem ;
      _ => Predef.error ("noun_6a_fromNomPl is only applicable for words that end in -is, tried to apply to" ++ lemma)
    } ;

  noun_3f_fromStems : Str -> Noun = \stem ->
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

  noun_3f_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "us" => noun_3f_fromStems stem ;
      _ => Predef.error ("noun_3f_fromLemma is only applicable for words that end in -us, tried to apply to" ++ lemma)
    } ;

  noun_3f_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "us" => noun_3f_fromStems stem ;
      _ => Predef.error ("noun_3f_fromNomPl is only applicable for words that end in -us, tried to apply to" ++ lemma)
    } ;

  noun_6b_fromStems : Str -> Noun = \stem ->
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

  noun_6b_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "s" => noun_6b_fromStems stem ;
      _ => Predef.error ("noun_6b_fromLemma is only applicable for words that end in -s, tried to apply to" ++ lemma)
    } ;

  noun_6b_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "is" => noun_6b_fromStems stem ;
      _ => Predef.error ("noun_6b_fromNomPl is only applicable for words that end in -is, tried to apply to" ++ lemma)
    } ;

  noun_5fb_fromStems : Str -> Noun = \stem ->
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

  noun_5fb_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "e" => noun_5fb_fromStems stem ;
      _ => Predef.error ("noun_5fb_fromLemma is only applicable for words that end in -e, tried to apply to" ++ lemma)
    } ;

  noun_5fb_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "es" => noun_5fb_fromStems stem ;
      _ => Predef.error ("noun_5fb_fromNomPl is only applicable for words that end in -es, tried to apply to" ++ lemma)
    } ;

  noun_5mb_fromStems : Str -> Noun = \stem ->
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

  noun_5mb_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "e" => noun_5mb_fromStems stem ;
      _ => Predef.error ("noun_5mb_fromLemma is only applicable for words that end in -e, tried to apply to" ++ lemma)
    } ;

  noun_5mb_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "es" => noun_5mb_fromStems stem ;
      _ => Predef.error ("noun_5mb_fromNomPl is only applicable for words that end in -es, tried to apply to" ++ lemma)
    } ;

  noun_2b_fromStems : Str -> Noun = \stem ->
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

  noun_2b_fromLemma : Str -> Noun = \lemma ->
    case lemma of {
      stem + "is" => noun_2b_fromStems stem ;
      _ => Predef.error ("noun_2b_fromLemma is only applicable for words that end in -is, tried to apply to" ++ lemma)
    } ;

  noun_2b_fromNomPl : Str -> Noun = \lemma ->
    case lemma of {
      stem + "i" => noun_2b_fromStems stem ;
      _ => Predef.error ("noun_2b_fromNomPl is only applicable for words that end in -i, tried to apply to" ++ lemma)
    } ;
}

