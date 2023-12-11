--# -path=.:../abstract:../common:../prelude

-- Adam Slaski, 2009, 2010 <adam.slaski@gmail.com>

concrete NumeralLit of Numeral = CatLit [Numeral,Digits] ** open ResLit, Prelude, MorphoAdjectiveLit, ParadigmsLit in {

  flags  coding=utf8 ;

  lincat
    Digit =  { unit,teen,ten:     Case * NounAgrCat => Str; 
               ounit,oteen,oten: AForm => Str;
               numAgr:NumComb };       -- 2..9 
    Sub10, Sub100, Sub1000, Sub1000000 = 
             { s:Case * NounAgrCat => Str; 
               o:AForm => Str;
               numAgr:NumComb; nb:Number }; 

  lin 
--   num : Sub1000000 -> Numeral ;
    num a = { s = \\x,y=>a.s!<x,y>; o=a.o; numAgr=a.numAgr; nb=a.nb };

--   n2, n3, n4, n5, n6, n7, n8, n9 : Digit ;
n2 = {unit = table {
        <_, NoAgr> => "numbers cannot be NoAgr";
	<(Nom|VocL|Acc), (SingPlur Masc|PlurOnly Masc)> => "du" ;
	<(Nom|VocL|Acc), (SingPlur Fem|PlurOnly Fem)> => "dvi" ;
	<(Ins|Dat), _> => "dviem" ;
	<Loc, (SingPlur Masc|PlurOnly Masc)> => "dvejuose" ;
	<Loc, (SingPlur Fem|PlurOnly Fem)> => "dvejose" ;
	<Gen, _> => "dviejų"
      };
      teen     = mkCardXlika "dvy" ;
      ten      = desimt "dvi" ;
      ounit    = mkAtable(guessAdjModel "antras");
      oteen    = mkAtable(guessAdjModel "dvyliktas");
      oten     = mkAtable(guessAdjModel "dvidešimtas");
      numAgr = AgrComb
    };
n3 = { 
      unit     = trys ;
      teen     = mkCardXlika "try" ;
      ten      = desimt "tris" ;
      ounit    = mkAtable(guessAdjModel "trečias");
      oteen    = mkAtable(guessAdjModel "triliktas");
      oten     = mkAtable(guessAdjModel "trisdešimtas");
      numAgr = AgrComb
    };
n4 = {unit     = mkCardXi "ketur" ;
      teen     = mkCardXlika "keturio" ;
      ten      = desimt "keturias" ;
      ounit    = mkAtable(guessAdjModel "ketvirtas");
      oteen    = mkAtable(guessAdjModel "keturioliktas");
      oten     = mkAtable(guessAdjModel "keturiasdešimtas");
      numAgr = AgrComb
    };
n5 = {unit     = mkCardXi "penk" ;
      teen     = mkCardXlika "penkio" ;
      ten      = desimt "penkias" ;
      ounit    = mkAtable(guessAdjModel "penktas");
      oteen    = mkAtable(guessAdjModel "pekioliktas");
      oten     = mkAtable(guessAdjModel "penkiasdešimtas");
      numAgr = AgrComb
    };
n6 = {unit     = mkCardXi "šeš" ;
      teen     = mkCardXlika "šešio" ;
      ten      = desimt "šešias" ;
      ounit    = mkAtable(guessAdjModel "šeštas");
      oteen    = mkAtable(guessAdjModel "šešioliktas");
      oten     = mkAtable(guessAdjModel "šešiasdešimtas");
      numAgr = AgrComb
    };
n7 = {unit     = mkCardXi "septyn" ;
      teen     = mkCardXlika "septynio" ;
      ten      = desimt "septynias" ;
      ounit    = mkAtable(guessAdjModel "septintas");
      oteen    = mkAtable(guessAdjModel "septinioliktas");
      oten     = mkAtable(guessAdjModel "septyniasdešimtas");
      numAgr = AgrComb
    };
n8 = {unit     = mkCardXi "aštuon" ;
      teen     = mkCardXlika "aštuonio" ;
      ten      = desimt "aštuonias" ;
      ounit    = mkAtable(guessAdjModel "aštuntas");
      oteen    = mkAtable(guessAdjModel "aštuonioliktas");
      oten     = mkAtable(guessAdjModel "aštuoniasdesimtas");
      numAgr = AgrComb
    };
n9 = { unit    = mkCardXi "devyn" ;
      teen     = mkCardXlika "devynio" ;
      ten      = desimt "devynias" ;
      ounit    = mkAtable(guessAdjModel "devintas");
      oteen    = mkAtable(guessAdjModel "devynioliktas");
      oten     = mkAtable(guessAdjModel "devyniasdešimtas");
      numAgr = AgrComb
    };

    
--    pot01 : Sub10 ;                               -- 1
    pot01 = {
      s = table {
        <_, NoAgr> => "numbers cannot be NoAgr";
        <(Nom|VocL), SingPlur Masc> => "vienas";
        <Acc, SingPlur Masc> => "vieną";
        <Gen, SingPlur Masc> => "vieno";
        <Ins, SingPlur Masc> => "vienu";
        <Dat, SingPlur Masc> => "vienam";
        <Loc, SingPlur Masc> => "viename";
        <(Nom|VocL), SingPlur Fem> => "viena";
        <Acc, SingPlur Fem> => "vieną";
        <Gen, SingPlur Fem> => "vienos";
        <Ins, SingPlur Fem> => "viena";
        <Dat, SingPlur Fem> => "vienai";
        <Loc, SingPlur Fem> => "vienoje";
        <(Nom|VocL), PlurOnly Masc> => "vieneri";
        <Acc, PlurOnly Masc> => "vienerius";
        <Gen, PlurOnly Masc> => "vienerių";
        <Ins, PlurOnly Masc> => "vieneriais";
        <Dat, PlurOnly Masc> => "vieneriems";
        <Loc, PlurOnly Masc> => "vieneriuose";
        <(Nom|VocL), PlurOnly Fem> => "vienerios";
        <Acc, PlurOnly Fem> => "vienerias";
        <Gen, PlurOnly Fem> => "vienerių";
        <Ins, PlurOnly Fem> => "vieneriomis";
        <Dat, PlurOnly Fem> => "vienerioms";
        <Loc, PlurOnly Fem> => "vieneriose"
      };
      o    = mkAtable(guessAdjModel "pirmas");
--      ohundred = mkAtable(guessAdjModel "setny");
      numAgr = AgrComb;
      nb = Sg
    };
    
--    pot0 : Digit -> Sub10 ;                       -- d * 1
    pot0 d = {
        s = d.unit;   -- hundred  = d.hundred;
        o = d.ounit;  -- ohundred = d.ohundred;
        numAgr = d.numAgr;
        nb = Pl
    };
    
--    pot110 : Sub100 ;                             -- 10
    pot110 = {
      s =table {
        _     => "dešimt"
      };
      o = mkAtable(guessAdjModel "dešimtas");
      numAgr = GenComb;
      nb = Pl
    };

--    pot111 : Sub100 ;                             -- 11
    pot111 = {
      s = mkCardXlika "vienuo" ;
      o = mkAtable(guessAdjModel "vienuoliktas");
      numAgr = GenComb;
      nb = Pl
    };

--    pot1to19 : Digit -> Sub100 ;                  -- 10 + d
    pot1to19 d = {
        s = d.teen;
        o = d.oteen;
        numAgr = GenComb;
        nb = Pl
    };

--    pot0as1 : Sub10 -> Sub100 ;                   -- coercion of 1..9
    pot0as1 s = {
        s = s.s;
        o = s.o;
        numAgr = s.numAgr; 
        nb = s.nb
    };
    
--    pot1 : Digit -> Sub100 ;                      -- d * 10
    pot1 d = {
        s = d.ten;
        o = d.oten;
        numAgr = GenComb;
        nb = Pl
    };

--    pot1plus : Digit -> Sub10 -> Sub100 ;         -- d * 10 + n
    pot1plus d s = {
        s = \\x => d.ten!x  ++ s.s!x;
        o = \\x => d.oten!x ++ s.o!x;
        numAgr = s.numAgr;
        nb = s.nb
    };

--    pot1as2 : Sub100 -> Sub1000 ;                 -- coercion of 1..99
    pot1as2 s = {
        s = s.s;
        o = s.o;
        numAgr = s.numAgr; 
        nb = s.nb
    };

--   pot2 : Sub10 -> Sub1000 ;                     -- m * 100
    pot2 s = {
        s = \\x => case s.nb of { Sg => ""; Pl => s.s!<x.p1,SingPlur Masc> } 
            ++ simtas!<(accom_case! <s.numAgr,x.p1, SingPlur Masc>),s.nb>;
        o = \\x => s.o!x ++ (mkAtable (guessAdjModel "šimtasis"))!x; --FIXME dwu tysieczny, nie dwa tysieczny
        numAgr = GenComb;
        nb = Pl
    };
    
--    pot2plus : Sub10 -> Sub100 -> Sub1000 ;       -- m * 100 + n
    pot2plus s10 s100 = {
        s = \\x => case s10.nb of { Sg => ""; Pl => s10.s!<x.p1,SingPlur Masc> } 
            ++ simtas!<(accom_case! <s10.numAgr,x.p1, SingPlur Masc>),s10.nb> 
            ++ s100.s!x ; 
        o = \\x => case s10.nb of { Sg => ""; Pl => s10.s!<Nom,SingPlur Masc> } 
            ++ simtas!<(accom_case! <s10.numAgr,Nom, SingPlur Masc>),s10.nb> 
            ++ s100.o!x;
        numAgr = s100.numAgr ;
        nb = s100.nb
    };


--    pot2as3 : Sub1000 -> Sub1000000 ;             -- coercion of 1..999
    pot2as3 s = {
        s = s.s;
        o = s.o;
        numAgr = s.numAgr; 
        nb = s.nb -- was Pl - why? very strange ASL
    };


--    pot3 : Sub1000 -> Sub1000000 ;                -- m * 1000
    pot3 s = {
        s = \\x => case s.nb of { Sg => ""; Pl => s.s!<x.p1,SingPlur Masc> } 
            ++ tukstantis!<(accom_case! <s.numAgr,x.p1, SingPlur Masc>),s.nb>;
        o = \\x => s.o!x ++ (mkAtable (guessAdjModel "tysięczny"))!x; --FIXME dwu tysieczny, nie dwa tysieczny
        numAgr = GenComb;
        nb = Pl
    };

--    pot3plus : Sub1000 -> Sub1000 -> Sub1000000 ; -- m * 1000 + n
    pot3plus s s2 = {
        s = \\x => case s.nb of { Sg => ""; Pl => s.s!<x.p1,SingPlur Masc> } 
            ++ tukstantis!<(accom_case! <s.numAgr,x.p1, SingPlur Masc>),s.nb> 
            ++ s2.s!x ;
        o = \\x => case s.nb of { Sg => ""; Pl => s.s!<Nom,SingPlur Masc> } 
            ++ tukstantis!<(accom_case! <s.numAgr,Nom, SingPlur Masc>),s.nb> 
            ++ s2.o!x;
        numAgr = s2.numAgr ;
        nb = s2.nb
    };

oper mkCardXi : Str -> Case * NounAgrCat => Str
  = \n -> table {
		  <_, NoAgr> => "numbers cannot be NoAgr";
		  <(Nom|VocL), (SingPlur Masc|PlurOnly Masc)> => n + "i" ;
		  <Acc, (SingPlur Masc|PlurOnly Masc)> => n + "is" ;
		  <Gen, _> => n + "ių" ;
		  <Dat, (SingPlur Masc|PlurOnly Masc)> => n + "iems" ;
		  <Ins, (SingPlur Masc|PlurOnly Masc)> => n + "iais" ;
		  <Loc, (SingPlur Masc|PlurOnly Masc)> => n + "iuose" ;
		  <(Nom|VocL), (SingPlur Fem|PlurOnly Fem)> => n + "ios" ;
		  <Acc, (SingPlur Fem|PlurOnly Fem)> => n + "ias" ;
		  <Dat, (SingPlur Fem|PlurOnly Fem)> => n + "ioms" ;
		  <Ins, (SingPlur Fem|PlurOnly Fem)> => n + "iomis" ;
		  <Loc, (SingPlur Fem|PlurOnly Fem)> => n + "iose" 
		} ;

oper mkCardXlika : Str -> Case * NounAgrCat => Str
  = \n -> table {
		  <_, NoAgr> => "numbers cannot be NoAgr";
		  <(Nom|VocL|Acc|Ins), _> => n + "lika" ;
		  <Gen, _> => n + "likos" ;
		  <Dat, _> => n + "likai" ;
		  <Loc, _> => n + "likoje"
		} ;


-- Formes déclinées ???
oper desimt : Str -> Case * NounAgrCat => Str
  = \n -> table {
		  <_, _> => n + "dešimt"
		} ;

oper trys : Case * NounAgrCat => Str
  = table {
		  <_, NoAgr> => "numbers cannot be NoAgr";
		  <(Nom|VocL), _> => "trys";
		  <Acc, _> => "tris";
		  <Gen, _> => "trijų";
		  <Dat, _> => "trims";
		  <Ins, _> => "trimis";
		  <Loc, (SingPlur Masc|PlurOnly Masc)> => "trijuose";
		  <Loc, (SingPlur Fem|PlurOnly Fem)> => "trijose"
		} ;
-- sera collé... Dirty fix
oper tukstantis = table {
    <(Nom|VocL), Sg> => "tūkstantis";
    <Acc, Sg>        => "tūkstantį";
    <Gen, Sg>        => "tūkstančio";
    <Dat, Sg>        => "tūkstančiui";
    <Ins, Sg>        => "tūkstančiu";
    <Loc,Sg>         => "tūkstantyje";
    <(Nom|VocL), Pl> => "tūkstančiai";
    <Acc, Pl>        => "tūkstančius";
    <Gen, Pl>        => "tūkstančių";
    <Dat, Pl>        => "tūkstančiams";
    <Ins, Pl>        => "tūkstančiais";
    <Loc, Pl>        => "tūkstančiuose"
  };

oper simtas : Case * Number => Str
  = table {
        <Nom|VocL,Sg > => "šimtas";
        <Acc,Sg > => "šimtą";
        <Gen,Sg > => "šimto";
        <Dat,Sg > => "šimtam";
        <Ins,Sg > => "šimtu";
        <Loc,Sg > => "šimtame";
        <Nom|VocL,Pl > => "šimtai";
        <Acc,Pl > => "šimtus";
        <Gen,Pl > => "šimtų";
        <Dat,Pl > => "šimtams";
        <Ins,Pl > => "šimtais";
        <Loc,Pl > => "šimtame"
      };

-- -- Numerals as sequences of digits have a separate, simpler grammar
  lincat 
    Dig = {s:Str; o:Str; nb:Number; numAgr:NumComb};  -- single digit 0..9

  lin
--     IDig  : Dig -> Digits ;       -- 8
    IDig d = d;
    
--     IIDig : Dig -> Digits -> Digits ; -- 876
    IIDig d dd = { s = d.s ++ BIND ++ dd.s; o = d.s ++ BIND ++ dd.o; nb=Pl; numAgr=dd.numAgr };

    D_0 = { s = "0"; o="0."; nb=Pl; numAgr=GenComb };
    D_1 = { s = "1"; o="1."; nb=Sg; numAgr=AgrComb };
    D_2 = { s = "2"; o="2."; nb=Pl; numAgr=AgrComb };
    D_3 = { s = "3"; o="3."; nb=Pl; numAgr=AgrComb };
    D_4 = { s = "4"; o="4."; nb=Pl; numAgr=AgrComb };
    D_5 = { s = "5"; o="5."; nb=Pl; numAgr=AgrComb };
    D_6 = { s = "6"; o="6."; nb=Pl; numAgr=AgrComb };
    D_7 = { s = "7"; o="7."; nb=Pl; numAgr=AgrComb };
    D_8 = { s = "8"; o="8."; nb=Pl; numAgr=AgrComb };
    D_9 = { s = "9"; o="9."; nb=Pl; numAgr=AgrComb };

    PosDecimal d = d ** {hasDot=False} ;
    NegDecimal d = {
      s = "-" ++ BIND ++ d.s;
      o = "-" ++ BIND ++ d.o;
      nb=Pl;
      numAgr=d.numAgr;
      hasDot=False
      } ;
    IFrac d i = {
      s = d.s ++
          if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
          i.s;
      nb = Pl ;
      numAgr=d.numAgr;
      hasDot=True
    } ;

}
