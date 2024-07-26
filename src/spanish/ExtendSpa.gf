--# -path=alltenses:../common:../abstract:../romance
concrete ExtendSpa of Extend = CatSpa ** ExtendRomanceFunctor -
  [
 CompVP,
 CompoundAP,
 CompoundN,
 ExistsNP,
 EmbedSSlash,
 GenRP,
 IAdvAdv,
 ICompAP,
 InOrderToVP,
 WithoutVP,
 iFem_Pron,
 theyFem_Pron,
 weFem_Pron,
 youFem_Pron,
 youPlFem_Pron,
 youPolFem_Pron,
 youPolPlFem_Pron,
 youPolPl_Pron,
 PassVPSlash, PassAgentVPSlash,
 UseComp_estar, UseComp_ser,
 ExistNP

    ]                   -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarSpa), (Syntax = SyntaxSpa), (ResRomance = ResSpa) **
  open
  GrammarSpa,
  ResSpa,
  MorphoSpa,
  Coordination,
  Prelude,
  ParadigmsSpa,
  (I=IrregSpa) in {
    -- put your own definitions here

  lin
    iFem_Pron     = agr2pron ! {g=Fem ; n=Sg ; p=P1} ;
    youFem_Pron   = agr2pron ! {g=Fem ; n=Sg ; p=P2} ;
    weFem_Pron    = agr2pron ! {g=Fem ; n=Pl ; p=P1} ;
    youPlFem_Pron = agr2pron ! {g=Fem ; n=Pl ; p=P2} ;
    theyFem_Pron  = agr2pron ! {g=Fem ; n=Pl ; p=P3} ;

    youPolFem_Pron = mkPronoun
      "usted" "la" "le" "usted"
      "su" "su" "sus" "sus"
      Fem Sg P3 ;
    youPolPl_Pron = mkPronoun
      "ustedes" "los" "les" "ustedes"
      "su" "su" "sus" "sus"
      Masc Pl P3 ;
    youPolPlFem_Pron = mkPronoun
      "ustedes" "las" "les" "ustedes"
      "su" "su" "sus" "sus"
      Fem Pl P3 ;

    ICompAP ap = {
      s =\\a => "qué tan" ++ ap.s ! (genNum2Aform a.g a.n) ;
      cop = serCopula
      } ;

    IAdvAdv adv = {
      s = "qué tan" ++ adv.s
      } ;

    EmbedSSlash s = {s = \\_ => "lo que" ++ s.s ! {g=Masc ; n=Sg} ! Indic} ;

    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV (mkV "existir"))) ;

    CompoundN noun noun2 = { -- order is different because that's needed for correct translation from english
      s = \\n => noun2.s ! n ++
                 case noun2.relType of {
                   NRelPrep p => prepCase (CPrep p) ;  -- tasa de suicidio
                   NRelNoPrep => []                    -- connessione internet = internet connection
                 } ++
                 noun.s ! Sg ;
      g = noun2.g ;
      relType = noun2.relType
      } ;

    CompoundAP noun adj = {
      s = \\af => case (aform2aagr af) of {
        {n = n} => adj.s ! genNum2Aform noun.g n ++ "de" ++ noun.s ! n
        } ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    WithoutVP vp = {
      s = "sin" ++ infStr vp
      } ;

    InOrderToVP vp = {
      s = "para" ++ infStr vp
      } ;

    --TODO: actually use ant
    CompVP ant p vp = let
      neg = negation ! p.p
      in {
        s = \\agr => ant.s ++ p.s ++ "de" ++ neg.p1 ++ infVP vp RPos agr ;
        cop = serCopula
      } ;

lin UseComp_estar comp = insertComplement comp.s (predV I.estar_V) ;
    UseComp_ser comp = insertComplement comp.s (predV copula) ;

lin PassVPSlash vps = passVPSlash vps [] ;
    PassAgentVPSlash vps np = passVPSlash 
      vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

oper
    passVPSlash : VPSlash -> Str -> VP = \vps, agent -> 
      let auxvp = predV auxPassive 
      in
      vps ** {
         s = auxvp.s ;
         agr = auxvp.agr ;
         comp  = \\a => vps.comp ! a ++ (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ agent ;
        } ;

lin AnaphPron np = agr2pron ! np.a ;

    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV (mkV "existir"))) ;


} ;
