--# -path=.:../../prelude:../common
--# -coding=utf8

--1 A Lithuanian Resource Morphology 
--
-- Ilona Nowak, Wintersemester 2007/08
--
-- Adam Slaski, 2009 <adam.slaski@gmail.com>
--
-- L.Boizou, 2022 <lboizou@gmail.com>
--

resource MorphoPronounLit = ResLit ** open Prelude, (Predef=Predef) in {

     flags  coding=utf8; 
     
-- for "šis" ("this"), "kuris"
  oper mkPronXis:  Str -> { s : AForm => Str } = \s ->   
	 let
	   x = Predef.tk 2 s
	 in
	  { s = table {
	     AF _ _ VocL => "[" ++ x +"is" ++ [": the vocative form does not exist]"];
	     NeutAFNom => "[" ++ x +"is" ++ [": neutral forms do not exist]"];

	     AF Sg Masc Nom => x + "is";
	     AF Sg Masc Acc => x + "į";
	     AF Sg Masc Gen => x + "io";
	     AF Sg Masc Ins => x + "iuo";
	     AF Sg Masc Dat => x + "iam";
	     AF Sg Masc Loc => x + "iame";

	     AF Sg Fem Nom => x + "i";
	     AF Sg Fem Acc => x + "ią";
	     AF Sg Fem Gen => x + "ios";
	     AF Sg Fem Ins => x + "ia";
	     AF Sg Fem Dat => x + "iai";
	     AF Sg Fem Loc => x + "ioje";

	     AF Pl Masc Nom => x + "ie";
	     AF Pl Masc Acc => x + "iuos";
	     AF Pl Masc Gen => x + "ių";
	     AF Pl Masc Dat => x + "iems";
	     AF Pl Masc Ins => x + "iais";
	     AF Pl Masc Loc => x + "iuose";

	     AF Pl Fem Nom => x + "ios";
	     AF Pl Fem Acc => x + "ias";
	     AF Pl Fem Gen => x + "ių";
	     AF Pl Fem Dat => x + "ioms";
	     AF Pl Fem Ins => x + "iomis"; 
	     AF Pl Fem Loc => x + "iose"
	 } } ;

-- for "tas" ("that") and others (potentially)
  oper mkPronXas:  Str -> { s : AForm => Str } = \s ->   
	 let
	   x = Predef.tk 2 s
	 in
	  { s = table {
	     AF _ _ VocL => "[" ++ x +"as" ++ [": the vocative form does not exist]"];
	     NeutAFNom => "[" ++ x +"as" ++ [": neutral forms do not exist]"];

	     AF Sg Masc Nom => x + "as";
	     AF Sg Masc Acc => x + "ą";
	     AF Sg Masc Gen => x + "o";
	     AF Sg Masc Ins => x + "uo";
	     AF Sg Masc Dat => x + "am";
	     AF Sg Masc Loc => x + "ame";

	     AF Sg Fem Nom => x + "a";
	     AF Sg Fem Acc => x + "ą";
	     AF Sg Fem Gen => x + "os";
	     AF Sg Fem Ins => x + "a";
	     AF Sg Fem Dat => x + "ai";
	     AF Sg Fem Loc => x + "oje";

	     AF Pl Masc Nom => x + "ie";
	     AF Pl Masc Acc => x + "uos";
	     AF Pl Masc Gen => x + "ų";
	     AF Pl Masc Dat => x + "iems";
	     AF Pl Masc Ins => x + "ais";
	     AF Pl Masc Loc => x + "uose";

	     AF Pl Fem Nom => x + "os";
	     AF Pl Fem Acc => x + "as";
	     AF Pl Fem Gen => x + "ų";
	     AF Pl Fem Dat => x + "oms";
	     AF Pl Fem Ins => x + "omis"; 
	     AF Pl Fem Loc => x + "ose"
	 } } ;

-- for "koks" ("which"), "toks" ("such"), "joks" (none) and others (potentially)
  oper mkPronXs:  Str -> { s : AForm => Str } = \s ->   
	 let
	   x = Predef.tk 1 s
	 in
	  { s = table {
	     AF _ _ VocL => "[" ++ x +"s" ++ [": the vocative form does not exist]"];
	     NeutAFNom => "[" ++ x +"s" ++ [": neutral forms do not exist]"];

	     AF Sg Masc Nom => x + "s";
	     AF Sg Masc Acc => x + "į";
	     AF Sg Masc Gen => x + "io";
	     AF Sg Masc Ins => x + "iu";
	     AF Sg Masc Dat => x + "iam";
	     AF Sg Masc Loc => x + "iame";

	     AF Sg Fem Nom => x + "ia";
	     AF Sg Fem Acc => x + "ią";
	     AF Sg Fem Gen => x + "ios";
	     AF Sg Fem Ins => x + "ia";
	     AF Sg Fem Dat => x + "iai";
	     AF Sg Fem Loc => x + "ioje";

	     AF Pl Masc Nom => x + "ie";
	     AF Pl Masc Acc => x + "ius";
	     AF Pl Masc Gen => x + "ių";
	     AF Pl Masc Dat => x + "iems";
	     AF Pl Masc Ins => x + "iais";
	     AF Pl Masc Loc => x + "iuose";

	     AF Pl Fem Nom => x + "ios";
	     AF Pl Fem Acc => x + "ias";
	     AF Pl Fem Gen => x + "ių";
	     AF Pl Fem Dat => x + "ioms";
	     AF Pl Fem Ins => x + "iomis"; 
	     AF Pl Fem Loc => x + "iose"
	 } } ;

}
