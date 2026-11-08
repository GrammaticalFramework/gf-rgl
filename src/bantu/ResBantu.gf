--1 Bantu auxiliary operations.

interface ResBantu = DiffBantu ** open CommonBantu in {

flags 
  optimize=all ;
  coding=utf8 ;

--2 Constants uniformly defined in terms of language-dependent constants

oper
  npNom : NPCase = NCase Nom ;
  npLoc : NPCase = NCase Loc ;
  NPAcc : NPCase = NCase Nom ; -- Added this so that swahili compiles. /IL2020
  npcase2case : NPCase -> Case = \nc -> case nc of {NCase c => c ; _ => Nom} ;


  mkNP : (i,my : Str) -> Gender -> Number -> Person ->  
    {s : NPCase => Str ; a : Agr} = \i,my,g,n,p -> 
    { s = table {
        NCase Nom => i ;

        NCase Loc | NPoss => my -- works for normal genitives, "whose", etc.

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

  Verb : Type      = {s, passive : Polarity => VForm => Str} ;
  VerbForms : Type = Tense => Anteriority => Polarity => Agre => Str; 
  VP : Type        = {
    s, passive : VerbForms ;
    obj : Agre => Str ;
    inf : Polarity => Agre => Str ;
    passiveInf : Polarity => Agre => Str ;
    imp : Polarity => Number => Str ;
    passiveImp : Polarity => Number => Str
    };
  SlashVP          = VP ** {c2 : Str} ;

  predV : Verb -> VP = \verb -> {
    s = \\t,ant,b,agre => 
      case t of {
        Pres  => verb.s ! b ! VPres agre.g agre.n agre.p ;
        Fut   => verb.s ! b ! VFut  agre.g agre.n  agre.p ;   --# notpresent
        Pas   => verb.s ! b ! VPast agre.g agre.n agre.p
      } ;
    passive = \\t,ant,b,agre =>
      case t of {
        Pres => verb.passive ! b ! VPres agre.g agre.n agre.p ;
        Fut  => verb.passive ! b ! VFut agre.g agre.n agre.p ;
        Pas  => verb.passive ! b ! VPast agre.g agre.n agre.p
      } ;
    obj = \\_ => [] ;
    inf = \\b,_ => verb.s ! b ! VInf ;
    passiveInf = \\b,_ => verb.passive ! b ! VInf ;
    imp = \\b,n => verb.s ! b ! VImp n ;
    passiveImp = \\b,n => verb.passive ! b ! VImp n
    };

  insertObj : (Agre => Str) -> VP -> VP = \x,vp -> vp ** {
    obj = \\a => vp.obj ! a ++ x ! a
    } ;

  insertAdV : Str -> VP -> VP = \x,vp -> vp ** {
    s = \\t,ant,b,a => x ++ vp.s ! t ! ant ! b ! a ;
    passive = \\t,ant,b,a => x ++ vp.passive ! t ! ant ! b ! a
    } ;

  -- Productive Swahili passives are formed by replacing the final -a by
  -- -wa (and by adding -wa to loan verbs ending in another vowel).  Keeping
  -- this operation on VP also preserves complements and adjuncts already
  -- accumulated in obj.
  passiveVP : VP -> VP = \vp -> vp ** {
    s = vp.passive ;
    inf = vp.passiveInf ;
    imp = vp.passiveImp
    } ;

  infVP : Polarity -> Agre -> VP -> Str = \b,a,vp ->
    vp.inf ! b ! a ++ vp.obj ! a ;

  Clause : Type = {
    s : Tense => Anteriority => Polarity => Str
    } ;

  mkClause : Str -> Agre -> VP -> Clause =
    \subj,agr,vp -> {
      s = \\t,a,b => 
        let 
          verb  = vp.s ! t ! a ! b ! agr 
        in
          subj ++ verb ++ vp.obj ! agr
    } ;

  finalComma : Str = pre {"," | "." => []; "" => SOFT_BIND ++ ","; _ => []} ;
  frontComma : Str = SOFT_BIND ++ "," ; 
}
