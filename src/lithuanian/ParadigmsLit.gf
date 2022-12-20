--# -path=.:../prelude:../common:../abstract
--# -coding=utf8

--1 A polish Resource Morphology 

-- Adam Slaski, 2009, 2010, 2011 <adam.slaski@gmail.com>

-- Description of the Polish morphology

resource ParadigmsLit = 
    Prelude, 
    ResLit, 
    MorphoLit, -- (< MorphoPronounLit (mkPronXis, mkPronXs, mkPronXas))
    ParadigmsVerbLit, 
    ParadigmsPronounLit,
    ParadigmsAdjectiveLit,
    ParadigmsNounLit ** open (Predef=Predef) in {

     flags  coding=utf8; 

oper

  mkAdv : Str -> { s :Str };
  mkAdv x = { s = x };

  compN : Adj -> CommNoun -> CommNoun ;
  compN adj noun = 
    let adjTable = mkAtable adj.pos in 
    {
       s = table { 
            SF Sg Nom => adjTable!(cast_aform_exp!<noun.g,Sg,Nom>) ++ noun.s!SF Sg Nom;
            SF Sg Acc => adjTable!(cast_aform_exp!<noun.g,Sg,Acc>) ++ noun.s!SF Sg Acc;
            SF Sg Gen => adjTable!(cast_aform_exp!<noun.g,Sg,Gen>) ++ noun.s!SF Sg Gen;
            SF Sg Ins => adjTable!(cast_aform_exp!<noun.g,Sg,Ins>) ++ noun.s!SF Sg Ins;
            SF Sg Dat => adjTable!(cast_aform_exp!<noun.g,Sg,Dat>) ++ noun.s!SF Sg Dat;
            SF Sg Loc => adjTable!(cast_aform_exp!<noun.g,Sg,Loc>) ++ noun.s!SF Sg Loc;
            SF Sg VocL => adjTable!(cast_aform_exp!<noun.g,Sg,VocL>) ++ noun.s!SF Sg VocL;

            SF Pl Nom => adjTable!(cast_aform_exp!<noun.g,Pl,Nom>) ++ noun.s!SF Pl Nom;
            SF Pl Acc => adjTable!(cast_aform_exp!<noun.g,Pl,Acc>) ++ noun.s!SF Pl Acc;
            SF Pl Gen => adjTable!(cast_aform_exp!<noun.g,Pl,Gen>) ++ noun.s!SF Pl Gen;
            SF Pl Ins => adjTable!(cast_aform_exp!<noun.g,Pl,Ins>) ++ noun.s!SF Pl Ins;
            SF Pl Dat => adjTable!(cast_aform_exp!<noun.g,Pl,Dat>) ++ noun.s!SF Pl Dat;
            SF Pl Loc => adjTable!(cast_aform_exp!<noun.g,Pl,Loc>) ++ noun.s!SF Pl Loc;
            SF Pl VocL => adjTable!(cast_aform_exp!<noun.g,Pl,VocL>) ++ noun.s!SF Pl VocL
          } ;
          g = noun.g ;
          nomType = Reg
   } ;

-- Nouns used as functions need a preposition. The most common is with Genitive.

  mkN2 : CommNoun -> CommNoun2 ;
  mkN2 n = mkFun n nullPrep ;

  mkFun  : CommNoun -> Complement -> CommNoun2;
  mkFun f p = { s = f.s; g = f.g; nomType = Reg; cplCase = { cas = p.cas; s = p.s } } ;

  mkN3 : CommNoun -> Complement -> Complement -> CommNoun3;
  mkN3 f p r = { s = f.s; g = f.g; nomType = Reg; cplCase = {s=p.s; cas=p.cas} ; cplCase2 = {s=r.s; cas=r.cas} }; 

-- Prepositions   

-- The commonest cases are functions with Genitive.
  nullPrep : Complement = mkPrep [] Gen; --{s = []; c= GenC};  

-- A preposition is formed from a string and a case.

  mkPrep : Str -> Case -> Complement;
  mkPrep s c = mkCompl s c;

-- definitions for structural objects

    kuris = mkPronXis "kuris" ;
    koks = mkPronXs "koks" ; -- jaki
    toks = mkPronXs "toks" ; -- taki

    visi : NounPhrase = {
      nom = "visi" ;
      voc = "visi" ;
      dep = table {
        AccC => "visus";
        GenC => "visų";
        InsC => "visais";
        DatC => "visiems";
        LocC => "visuose"
      };
      p = P3 ;
      gn = MascPl ; -- in fact it is plurale tantum ver. 3 ;
      nomType = Pro
    };
    
-- wszystko
    viskas : NounPhrase = {
      nom = "viskas" ;
      voc = "viskas" ; -- does not exist
      dep = table {
        GenC => "visko";
        DatC => "viskam";
        AccC => "viską";
        InsC => "viskuo";
        LocC => "viskame"
      };
      p = P3;
      gn = Neut ;
      nomType = Pro
    };
    
--    ktos : NounPhrase = { 
    kazkasAnim : NounPhrase = { 
      nom = "kažkas" ;
      voc = "kažkas" ; -- does not exist
      dep = table {
        GenC => "kažko";
        DatC => "kažkam";
        AccC => "kažką";
        InsC => "kažkuo";
        LocC => "kažkame"
      };
      p = P3 ;
      gn = MascSg ;
      nomType = Pro
    };
    
--    cos : NounPhrase = {
    kazkas : NounPhrase = { 
      nom = "kažkas" ;
      voc = "kažkas" ; -- does not exist
      dep = table {
        GenC => "kažko";
        DatC => "kažkam";
        AccC => "kažką";
        InsC => "kažkuo";
        LocC => "kažkame"
      };
      p = P3 ;
      gn = Neut ;
      nomType = Pro
    };
	 

--    kto : NounPhrase = { 
    kasAnim : NounPhrase = { 
      nom = "kas" ;
      voc = "kas" ;
      dep = table {
	     GenC => "ko";
	     DatC => "kam";
	     AccC => "ką";
	     InsC => "kuo";
	     LocC => "kame"
	     };
      p = P3 ;
      gn = MascSg ;
      nomType = Pro
      };
	
--    co : NounPhrase = {
    kas : NounPhrase = {
      nom = "kas" ;
      voc = "kas" ;
      dep = table {
	     GenC => "ko";
	     DatC => "kam";
	     AccC => "ką";
	     InsC => "kuo";
	     LocC => "kame"
	     };
      p = P3 ;
      gn = Neut ;
      nomType = Pro
      };

--    kazdyDet : Determiner = {
-- Neut should not exist
    kiekvienasDet : Determiner = {
	  s = table {
	    Nom => table {SingPlur Masc => "kiekvienas"; SingPlur Fem => "kiekviena"; _ => "kiekvienas" };
            Acc => table {SingPlur Masc => "kiekvieną"; SingPlur Fem => "kiekvieną"; _ => "kiekvieną" };
            Gen => table {SingPlur Masc => "kiekvieno"; SingPlur Fem => "kiekvienos"; _ => "kiekvieno" };
            Ins => table {SingPlur Masc => "kiekvienu"; SingPlur Fem => "kiekviena"; _ => "kiekvienu" };
            Dat => table {SingPlur Masc => "kiekvienam"; SingPlur Fem => "kiekvienai"; _ => "kiekvienam" };
            Loc => table {SingPlur Masc => "kiekviename"; SingPlur Fem => "kiekvienoje"; _ => "kiekviename" };
            VocL => table {SingPlur Masc => "kiekvienas"; SingPlur Fem => "kiekviena"; _ => "kiekvienas" }
          };
          detType=NormalDet ;
	  nb = Sg;
	  numAgr = AgrComb;
	};
	
--	pareDet : Determiner = {
	keletasDet : Determiner = {
	  s = table {
	    Nom => table {_ => "keletas" };
	    Acc => table {_ => "keletą" };
	    Gen => table {_ => "keleto" };
	    Ins => table {_ => "keletu" };
	    Dat => table {_ => "keletam" };
	    Loc => table {_ => "keletame" };
	    VocL => table {_ => "keletas" }
	  };
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = GenComb
	};
	
--	wieleDet : Determiner = {
	daugybeDet : Determiner = {
	  s = table {
	    Nom => table { _ => "daugybė" };
	    Acc => table { _ => "daugybę" };
	    Gen => table { _ => "daugybės" };
	    Ins => table { _ => "daugybe" };
	    Dat => table { _ => "daugybei" };
	    Loc => table { _ => "daugybėje" };
	    VocL => table { _ => "daugybe" }
	  };
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = GenComb
	};

--	duzoDet : Determiner = {
	daugDet : Determiner = {
	  s = \\_,_=>"daug";
--      daug vandens...
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = GenComb
	};

--	ileDet : IDeterminer = {
	koksKiekisDet : IDeterminer = {
	  s = table {
	    Nom => table { _ => "koks kiekis" };
	    Acc => table { _ => "kokį kiekį" };
	    Gen => table { _ => "kokio kiekio" };
	    Ins => table { _ => "kokiu kiekiu" };
	    Dat => table { _ => "kokiam kiekam" };
	    Loc => table { _ => "kokiame kiekyje" };
	    VocL => table { _ => "koks kiekis" }
	  };
          detType=NormalDet ;
	  nb = Sg;
	  numAgr = GenComb
	};

	kiekDet : IDeterminer = {
	  s = \\_,_=>"kiek";
--      kiek vandens...
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = GenComb
	};

	
	-- for "nobody", "noone", "none"
--  oper niktNP : NounPhrase =
  oper niekasAnimNP : NounPhrase =
	 { voc,nom="niekas";
	   dep = table {
	     GenC => "nieko";
	     DatC => "niekam";
	     AccC => "nieką";
	     InsC => "niekuo";
	     LocC => "niekame"
	     };
           p=P3;
	   gn= MascSg ;
	   nomType = Pro
	 };

-- for "nothing"
--  oper nicNP : NounPhrase =
  oper niekasNP : NounPhrase =
	 { voc,nom="niekas";
	   dep = table {
	     GenC => "nieko";
	     DatC => "niekam";
	     AccC => "nieką";
	     InsC => "niekuo";
	     LocC => "niekame"
	     };
	   p=P3;
	   gn= Neut ;
	   nomType = Pro
	 };
	 
  joksQuant = mkPronXs "joks"; 

-- Nesu tikras ar tai geriausias vertimas (someSg)
-- Neutre à supprimer
    kazkiekDet : Determiner = {
	  s = \\_,_=>"kažkiek";
--      kažkiek vandens...
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = GenComb
	};

-- Nesu tikras ar tai geriausias vertimas (somePl)
-- Keleri Avec
    keliDet : Determiner = {
	  s = table {
	    Nom => table { SingPlur Masc => "keli"; SingPlur Fem=>"kelios"; _ => "keli" };
	    Acc => table { SingPlur Masc => "kelis"; SingPlur Fem=>"kelias"; _ => "kelis" };
	    Gen => table { SingPlur Masc => "kelių"; SingPlur Fem=>"kelių"; _ => "kelių" };
	    Ins => table { SingPlur Masc => "keliais"; SingPlur Fem=>"keliomis"; _=> "keliais" };
	    Dat => table { SingPlur Masc => "keliems"; SingPlur Fem=>"kelioms"; _ => "keliems" };
	    Loc => table { SingPlur Masc => "keliuose"; SingPlur Fem=>"keliose"; _=> "keliuose" };
	    VocL => table {SingPlur Masc => "keli"; SingPlur Fem=>"kelios"; _ => "keli" }
	  };
          detType=NormalDet ;
	  nb = Pl;
	  numAgr = AgrComb
	};

}
