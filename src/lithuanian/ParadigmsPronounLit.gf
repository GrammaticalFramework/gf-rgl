--# -path=.:../../prelude:../common
--# -coding=utf8

--1 A polish Resource Morphology 
--
-- Ilona Nowak, Wintersemester 2007/08
--
-- Adam Slaski, 2009 <adam.slaski@gmail.com>
--
-- L.Boizou, 2022 <lboizou@gmail.com>
--

resource ParadigmsPronounLit = ResLit ** open Prelude, (Predef=Predef) in {

     flags  coding=utf8; 
     
--4 Pronouns   

--4.1 General

--4.2 Personal pronouns and their possessive forms  

{-
Defined in ResLit because it is used in VerbLit and AdjLit
  oper save: Pron =
	 { nom = "[SAVE]"; -- does not exist
	   voc = "[SAVE]"; -- does not exist
	   -- A corriger
	   dep = table {
	     GenC => "sanęs"; -- Can be 'mano' with participles
	     DatC => "sau";
	     AccC => "save"; 
	     InsC => "savimi";
	     LocC => "savyje"
	     };
	   possForms = \\_ => "savo";
	   p  = P3 ; -- Should be removed
	   gn = MascSg -- Should be removed
	 };
-}

-- for "I", "my", "mine"
  oper pronAs: GenNum -> Pron = \gn ->
	 { nom = "aš";
	   voc = "aš";
	   -- A corriger
	   dep = table {
	     GenC => "manęs"; -- Can be 'mano' with participles
	     DatC => "man";
	     AccC => "mane"; 
	     InsC => "manimi";
	     LocC => "manyje"
	     };
	   possForms = \\_ => "mano";
	   p  = P1 ;
	   gn = gn ;
	   nomType = PersMark
	 };
       

-- for "you", "yours"
  oper pronTu: GenNum -> Pron = \gn ->
	 {
	   nom = "tu" ; 
	   voc = "tu" ;
	   -- A corriger
	   dep = table {
	     GenC => "tavęs"; -- Can be 'tavo' with paticiples
	     DatC => "tau";
	     AccC => "tave";
	     InsC => "tavimi";
	     LocC => "tavyje"
	     };
	   possForms = \\_ => "tavo";
	   p  = P2 ;
	   gn = gn ;
	   nomType = PersMark
	 };

-- Could be 'tamsta'
-- for "you polite" (very idiomatic: pron you = 'sir') male version
  oper pronPonas: Pron = 
	 { nom = "ponas" ;
	   voc = "ponai" ;
	   dep = table {
	     GenC => "pono";
	     DatC => "panui";
	     AccC => "paną";
	     InsC => "panu";
	     LocC => "pane"
	     };
	   possForms = \\_ => "pono";
	   p = P3 ;
	   gn = MascSg ;
	   nomType = Reg   
	 };

-- for "you polite" (very idiomatic: pron you = 'madam') female version
  oper pronPonia: Pron = 
	 { nom = "ponia" ;
	   voc = "ponia" ;
	   dep = table {
	     GenC => "ponios";
	     DatC => "poniai";
	     AccC => "ponią"; 
	     InsC => "ponia";
	     LocC => "ponioje"
	     };
	   possForms = \\_ => "ponios";
	   p  = P3 ;
	   gn = FemSg ;
	   nomType = Reg
	 };

-- for "he", "his" 
  oper pronJis: Pron = 
	 { nom = "jis" ;
	   voc = "jis(does not exist...)" ;
	   dep = table {
	     GenC => "jo";
	     DatC => "jam";
	     AccC => "jį"; 
	     InsC => "juo";
	     LocC => "jame"
	     };
	   possForms = \\_ => "jo";
	   p  = P3 ;
	   gn = MascSg ;
	   nomType = Pro
	 };


-- for "she", "her", "hers"
  oper pronJi: Pron = 
	 { nom = "ji" ;
	   voc = "ji(does not exist)" ;
	   dep = table {
	     GenC  => "jos";
	     DatC => "jai";
	     AccC => "ją"; 
	     InsC => "ja";
	     LocC => "joje"
	     };
	   possForms = \\_ => "jos";
	   p  = P3 ;
	   gn = FemSg ;
	   nomType = Pro
	 };


-- for "it", "its"
-- largement fantaisiste
  oper pronTai: Pron = 
	 { nom = "tai" ;
	   voc = "tai(does not exist)" ;
	   dep= table {
	     GenC => "Does not exist";
	     DatC => "tai";
	     AccC => "tai"; 
	     InsC => "tai"; 
	     LocC => "Does not exist"
	     };
	   possForms = \\_ => "to";
	   p = P3 ;
	   gn = Neut ;
	   nomType = Pro
	 };


-- for "we", "our", "us", "ours"
  oper pronMes: Pron = 
	 { nom = "mes"; 
	   voc = "mes";
	   dep = table {
	     GenC => "mūsų";
	     DatC => "mums"; 
	     AccC => "mes";
	     InsC => "mumis";
	     LocC => "mumyse"
	     };
	   possForms = \\_ => "mūsų" ;
	   p  = P1 ;
	   gn = MascPl ;
	   nomType = PersMark
	 };
      

-- for "you", "yours", "your"
  oper pronJus: Pron = 
	 { nom = "jūs" ;
	   voc = "jūs" ;
	   dep = table {
	     GenC => "jūsų";
	     DatC => "jums"; 
	     AccC => "jus";
	     InsC => "jumis";
	     LocC => "jumyse"
	     };
	   possForms = \\_ => "jūsų" ;
	   p  = P2 ;
	   gn = MascPl ;
	   nomType = PersMark
	 };


-- for "they", "their", "theirs" (Sg he)= Masculinum 
 oper pronJie: Pron = 
	 { nom = "jie" ;
	   voc = "jie(does not exist)" ;
	   dep = table {
	     GenC => "jų";
	     DatC => "jiems";
	     AccC => "juos";
	     InsC => "jais"; 
	     LocC => "juose"
	     };
	   possForms = \\_ => "jų";
	   p  = P3 ;
	   gn = MascPl ;
	   nomType = Pro
	 };


-- for "they", "their", "theirs" (Sg she, it)= Fem), Neut)
  oper pronJos: Pron = 
	 { nom = "jos" ;
	   voc = "jos(does not exist)" ;
	   dep = table {
	     GenC => "jų";
	     DatC => "joms";
	     AccC => "jas";
	     InsC => "jomis"; 
	     LocC => "jose"
	     };
	   possForms = \\_ => "jų";
	   p  = P3 ;
	   gn = FemPl ;
	   nomType = Pro
	 };
--4.3 Interrogative pronouns  
{-
-- for "who", "whose"
   oper pronKto : Pron =
	 { s = table {
	     PF Nom _ NonPoss => "kto" ;
	     (GenNoPrep|Gen) NonPoss => "kogo";
	     (DatNoPrep|DatPrep) NonPoss => "komu";
	     (AccNoPrep|AccPrep) NonPoss => "kogo";
	     InstrC NonPoss => "kim";
	     LocPrep NonPoss => "kim";
	     PF VocP _ NonPoss => nonExist;
	     PF _ _ (Poss _ _) => nonExist -- exists in my opinion [asl] : czyje
	     };
	   n = Sg;
	   p = P3 ;
	   g = PGen (Masc Personal);
	   pron = False
	 };


-- for "what"
  oper pronCo : Pron =
	 { s = table {
	     PF Nom _ NonPoss => "co";
	     (GenNoPrep|Gen) NonPoss => "czego";
	     (DatNoPrep|DatPrep) NonPoss => "czemu";
	     (AccNoPrep|AccPrep) NonPoss => "co";
	     InstrC NonPoss => "czym";
	     LocPrep NonPoss => "czym";
	     PF VocP _ NonPoss => nonExist;
	     PF _ _ (Poss _ _) => nonExist 
	     };
	   n = Sg;
	   p = P3 ;
	   g = PGen (Masc Personal);
	   pron = False
	 };



--4.4 Indefinite pronouns  

-- for "somebody", "someone", "someone's"
-- in negative sentence, question for "anybody", "anyone"

-- ktoś

-- for "someone", "somebody", "someone's", "somebody's"
-- in question for "anyone", "anybody", "anyone's", "anybody's"
  oper pronKtokolwiek : Pron =
	 { s = table {
	     PF Nom _ NonPoss => "ktokolwiek";
	     (GenNoPrep|Gen) NonPoss => "kogokolwiek";
	     (DatNoPrep|DatPrep) NonPoss => "komukolwiek";
	     (AccNoPrep|AccPrep) NonPoss => "kogokolwiek";
	     InstrC NonPoss => "kimkolwiek";
	     LocPrep NonPoss => "kimkolwiek";
	     PF VocP _ NonPoss => nonExist;
	     PF _ _ (Poss _ _) => nonExist 
	     };
	   n = Sg;
	   p = P3 ;
	   g = PGen (Masc Personal);
	   pron = False
	 };



-- for "something", "its"
-- in negativ sentence, question or if-sentence for "anything"

-- coś

-- for "something", "its"
-- in question for "anything"

-- doesn't seam to true, doesn't seam to be necessary

--   oper pronCokolwiek : Pron =
-- 	 { s = table {
-- 	     PF Nom _ NonPoss => "cokolwiek";
-- 	     (GenNoPrep|Gen) NonPoss => "czegokolwiek";
-- 	     (DatNoPrep|DatPrep) NonPoss => "czemukolwiek";
-- 	     (AccNoPrep|AccPrep) NonPoss => "cokolwiek";
-- 	     InstrC NonPoss => "czymkolwiek";
-- 	     LocPrep NonPoss => "czymkolwiek";
-- 	     PF VocP _ NonPoss => nonExist;
-- 	     PF _ _ (Poss _ _) => nonExist 
-- 	     };
-- 	   n = Sg;
-- 	   p = P3 ;
-- 	   g = PGen (Neut));
-- 	   pron = False
-- 	 };




--4.5 Negation pronouns 



-- for "nobody". Sg and Pl forms given. It is used like 
-- an adjective before a noun. So the end product of this 
-- oper is an adjectiv and no pronoun.
--   oper pronZaden :  Str -> Adjective = \zaden ->
-- 	 let   x = fleetingEminus zaden 
-- 	 in 
-- 	 table {
-- 		AF MascSg Nom => zaden;
-- 		AF MascSg GenC => x +"ego";
-- 		AF MascSg DatC => x +"emu";
-- 		AF MascInaniSg AccC => zaden;
-- 		AF MascSg AccC => x +"ego";
-- 		AF MascSg VocP => zaden;
-- 		AF MascSg _ => x + "ym";
--          ---------------------------
-- 		AF FemSg Nom => x +"a";
-- 		AF FemSg AccC => x +"ą";
-- 		AF FemSg Instr => x + "ą";
-- 		AF FemSg VocP => x + "a";
-- 		AF FemSg _ => x + "ej";
--          ---------------------------
-- 		AF Neut GenC => x +"ego"; 
-- 		AF Neut DatC => x +"emu"; 
-- 		AF Neut Instr => x + "ym"; 
-- 		AF Neut LocC => x + "ym";
-- 		AF Neut _ => x + "e";
--         -----------------------------
-- 		AF MascPersPl Nom => x;
-- 		AF MascPersPl DatC => x + "ym";
-- 		AF MascPersPl Instr => x + "ymi";
-- 		AF MascPersPl VocP => x; 
-- 		AF MascPersPl _ => x + "ych";
--           ---------------------------
-- 		AF (MascPersPl|OthersPl) Nom => x + "e";
-- 		AF (MascPersPl|OthersPl) DatC => x +"ym";
-- 		AF (MascPersPl|OthersPl) AccC => x + "e";
-- 		AF (MascPersPl|OthersPl) Instr => x + "mi"; 
-- 		AF (MascPersPl|OthersPl) VocP => x + "e";
-- 		AF (MascPersPl|OthersPl) _ => x + "ych"
-- 	        };
-- 
-- 
-}
--4.6 Demonstrativ pronouns  

-- See MorphoPronounLit

-- Pas de forme neutre...
  oper visas : AForm => Str = 
	 table {
	     AF Sg Masc Nom => "visas"; 
	     AF Sg Masc Acc => "visą"; 
	     AF Sg Masc Gen => "viso";
	     AF Sg Masc Ins => "visu";
	     AF Sg Masc Dat => "visam"; 
	     AF Sg Masc Loc => "visame"; 
	     AF Sg Masc VocL => "visas";
	     
	     AF Sg Fem Nom => "visa"; 
	     AF Sg Fem Acc => "visą"; 
	     AF Sg Fem Gen => "visos";
	     AF Sg Fem Ins => "visa";
	     AF Sg Fem Dat => "visai"; 
	     AF Sg Fem Loc => "visoje";
	     AF Sg Fem VocL => "visa";   
	         
	         -- Does not exist...
	     NeutAFNom => "viskas" ; 
	     NeutAFGen => "visko";
	
	     AF Pl Masc Nom => "visi"; 
	     AF Pl Masc Acc => "visus"; 
	     AF Pl Masc Gen => "visų";
	     AF Pl Masc Ins => "visais";
	     AF Pl Masc Dat => "visiems"; 
	     AF Pl Masc Loc => "visuose";
	     AF Pl Masc VocL => "visi";

	     AF Pl Fem Nom => "visos"; 
	     AF Pl Fem Acc => "visas"; 
	     AF Pl Fem Gen => "visų";
	     AF Pl Fem Ins => "visomis";
	     AF Pl Fem Dat => "visoms"; 
	     AF Pl Fem Loc => "visose";
	     AF Pl Fem VocL => "visos"
	     };

{-

--4.8 Pronouns used in funtion of DET, PREDET 

-- Here, I have to define "wszystek" again, but only for the plural.
-- I need it in declension of pronKazdy, because "każdy" has only 
-- sg forms. In pl they are used forms of "wszyscy".


--   oper pronWszystekDet : Str -> Adjective = \wszyscy ->
-- 	 table {
-- 	   AF MascPersPl Nom => "wszyscy";
-- 	   AF (MascPersPl|OthersPl) _) Nom => "wszystkie";
-- 	   AF (MascPersPl|OthersPl) _) GenC => "wszystkich";
-- 	   AF (MascPersPl|OthersPl) _) DatC => "wszystkim";
-- 	   AF MascPersPl AccC => "wszystkich";
-- 	   AF (MascPersPl|OthersPl) _) AccC => "wszystkie";
-- 	   AF (MascPersPl|OthersPl) _) Instr => "wszystkimi";
-- 	   AF (MascPersPl|OthersPl) _) LocC => "wszystkich";
-- 	   _ => nonExist 
-- 	 };


-- I need this oper for building of pronouns like "każdy". 
-- This pronoun has not any plural forms. For plural it is used
-- the pronoun "wszyscy" ( Pl form of "wszystek")

--   oper pronKazdy : (x : Str ) -> {s : Number => Adjective} = \x ->
-- 	 {s = table {
-- 	    Sg => table {af => ((AdjectivDeclension "każdy") ! af)}; 
-- 	    Pl => table {af => ((pronWszystekDet "wszyscy") ! af)} 
-- 	    	 }};
-}
}
