--1 Bantu auxiliary operations.

interface ResBantu = DiffBantu ** open CommonBantu in {

flags 
  optimize=all ;
  coding=utf8 ;

--2 Constants uniformly defined in terms of language-dependent constants

oper
  npNom : NPCase = NCase Nom ;
  npLoc : NPCase = NCase Loc ;
  npcase2case : NPCase -> Case = \nc -> case nc of {NCase c => c ; _ => Nom} ;


  mkNP : (i,my : Str) -> Gender -> Number -> Person ->  
    {s : NPCase => Str ; a : Agr} = \i,my,g,n,p -> 
    { s = table {
        NCase Nom => i ;
        NPAcc => my ;
        NCase Loc | NPNomPoss => my -- works for normal genitives, "whose", etc.
        } ;
      a = Ag g n p ;
    };

  regNP : Str ->Gender -> Number -> {s : NPCase => Str ; a : Agr} = \that,g, n -> 
    mkNP that that g n P3  ;

  mkPron: (i, mine : Str) -> Gender -> Number -> Person ->
   {s: PronForm => Str ; a : Agr} = \i,mine, g,n,p -> 
        { s = table { 
            Pers => i;
            Poss n g => case <n,g> of {
              <Sg ,_> => ProunSgprefix g + mine ; 
              <Pl,_> => ProunPlprefix g + mine}       
            } ;
          a = Ag g n p } ; 

  Verb : Type      = {s : Bool => VForm => Str} ;
  VerbForms : Type = Tense => Anteriority => Bool => Agre => Str; 
  VP : Type        = {s:VerbForms ; obj : Agr => Str};
  SlashVP          = VP ** {c2 : Str} ;

  predV : Verb -> VP = \verb -> {
    s = \\t,ant,b,agre => 
      let
        inf  = verb.s !b! VInf ;
        pres = verb.s !True! VPres agre.g agre.n agre.p ;
        presn = verb.s !False! VPres agre.g agre.n agre.p ;
        past = verb.s !True! VPast agre.g agre.n agre.p ;
        pastn = verb.s !False! VPast agre.g agre.n agre.p ;
        fut  = verb.s !True! VFut  agre.g agre.n  agre.p ; 
        futn  = verb.s !False! VFut  agre.g agre.n  agre.p ;
      in
      case <t,ant,b> of {
        <Pres,_, True>  => pres ;
        <Fut, _, True>  => fut ;   --# notpresent
        <Pas, _, True>  => past ;  --# notpresent
        <Pres,_, False> => presn ;
        <Fut, _, False> => futn ;  --# notpresent
        <Pas, _, False> => pastn 
        -- ; <_, _, _>       => inf     --# notpresent -- never reached
      } ;
    obj = \\_ => [] };

  Clause : Type = {
    s : Tense => Anteriority => Bool => Str
    } ;

  mkClause : Str -> Agre -> VP -> Clause =
    \subj,agr,vp -> {
      s = \\t,a,b => 
        let 
          verb  = vp.s ! t ! a ! b ! agr 
        in
          subj ++ verb
    } ;

  finalComma : Str = pre {"," | "." => []; "" => SOFT_BIND ++ ","; _ => []} ;
  frontComma : Str = SOFT_BIND ++ "," ; 
}

-- insertObject:
-- p -cat=Cl -tr "la femme te l' envoie"
-- PredVP (DetCN (DetSg DefSg NoOrd) (UseN woman_N)) 
--  (ComplV3 send_V3 (UsePron he_Pron) (UsePron thou_Pron))
-- la femme te l' a envoyé
--
-- p -cat=Cl -tr "la femme te lui envoie"
-- PredVP (DetCN (DetSg DefSg NoOrd) (UseN woman_N)) 
--   (ComplV3 send_V3 (UsePron thou_Pron) (UsePron he_Pron))
-- la femme te lui a envoyée
