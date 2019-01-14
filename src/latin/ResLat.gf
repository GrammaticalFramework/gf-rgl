--# -path=.:../abstract:../common:../prelude

--1 Latin auxiliary operations.

resource ResLat = ParamX ** open Prelude, Predef, (C=CommonX) in {

param
  Case = Nom | Acc | Gen | Dat | Abl | Voc ;
  Gender = Masc | Fem | Neutr ;
--  Degree = DPos | DComp | DSup ;

  oper
    consonant : pattern Str = #( "p" | "b" | "f" | "v" | "m" | "t" | "d" | "s" | "z" | "n" | "r" | "c" | "g" | "l" | "q" | "qu" | "h" );

    Noun : Type = {s : Number => Case => Str ; g : Gender } ; -- massable : Bool } ;
    NounPhrase : Type = 
      {
	s : Case => Str ; 
	g : Gender ; 
	n : Number ; 
	p : Person ;
	adv : Str ;
	preap : {s : Agr => Str } ;
	postap : {s : Agr => Str } ;
	det : Determiner 
      } ;
  param
    Order = SVO | VSO | VOS | OSV | OVS | SOV ;
    AdvPos = PreS | PreV | PreO | PreNeg | InV | InS ; -- | InO
    SAdvPos = SPreS | SPreV | SPreO | SPreNeg ;
  param
    Agr = Ag Gender Number Case ; -- Agreement for NP et al.
  oper
    Adjective : Type = {
      s : Degree => Agr => Str ; 
--      comp_adv : Str ; 
      --      super_adv : Str
      adv : Adverb ;
      } ;
    CommonNoun : Type = 
    {
      s : Number => Case => Str ; 
      g : Gender ;
      adv : Str ;
      preap : {s : Agr => Str } ;
      postap : {s : Agr => Str }
	--	massable : Bool
      } ;
    
-- nouns
  useCNasN : CommonNoun -> Noun = \cn ->
    {
      s = cn.s ;
      g = cn.g
--      massable = cn.massable;
    } ;

  pluralN : Noun -> Noun = \n ->
    { 
      s = table {
	Pl => n.s ! Pl ;
	Sg => \\_ => nonExist -- no singular forms
	};
      g = n.g ;
      preap = n.preap ;
      postap = n.postap
--      massable = n.massable ;
    };

  singularN : Noun -> Noun = \n ->
    lin N { 
      s = table {
	Sg => n.s ! Sg ;
	Pl => \\_ => nonExist -- no plural forms
	};
      g = n.g ;
      preap = n.preap ;
      postap = n.postap 
--      massable = n.massable ;
    };

  param
    AdjPos = Pre | Post ;
  oper
    addAdjToCN : AdjectivePhrase -> CommonNoun -> AdjPos -> CommonNoun = \ap,cn,pos ->
      {
	s = cn.s ;
	postap = case pos of { Pre => cn.postap ; Post => { s = \\a => ap.s ! a ++ cn.postap.s ! a } } ;
	preap = case pos of { Pre => { s = \\a => ap.s ! a ++ cn.preap.s ! a } ; Post => cn.preap } ;
	g = cn.g ;
	adv = cn.adv 
--	massable = cn.massable
      } ;
	  
  mkNoun : (n1,_,_,_,_,_,_,_,_,n10 : Str) -> Gender -> Noun =
    \sn,sa,sg,sd,sab,sv,pn,pa,pg,pd,g -> {
--  mkNoun : (n1,_,_,_,_,_,_,_,_,n10 : Str) -> Gender -> Bool -> Noun = 
    --\sn,sa,sg,sd,sab,sv,pn,pa,pg,pd,g,m -> {
      s = table {
	Sg => table {
          Nom => sn ;
          Acc => sa ;
          Gen => sg ;
          Dat => sd ;
          Abl => sab ;
          Voc => sv
          } ;
	Pl => table {
          Nom | Voc => pn ;
          Acc => pa ;
          Gen => pg ;
          Dat | Abl => pd
          }
	} ;
      g = g
--      massable = m
    } ;
  
-- to change the default gender

  nounWithGender : Gender -> Noun -> Noun = \g,n ->
    {s = n.s ; g = g } ; -- massable = n.massable ;} ;

  -- nounMassable : Bool -> Noun -> Noun = \m,n ->
  --   {s = n.s ; g = n.g ; massable = m } ;

  prefixNoun : Str -> Noun -> Noun =
    \p,n ->
    { s = \\num,cas => addPrefix p (n.s ! num ! cas) ; g = n.g };
    
    
  regNP : (_,_,_,_,_,_ : Str) -> Gender -> Number -> NounPhrase = 
    \nom,acc,gen,dat,abl,voc,g,n ->
    {
      s = table Case [ nom ; acc ; gen ; dat ; abl ; voc ] ;
      g = g ;
      n = n ;
      p = P3;
      adv = "" ;
      preap, postap = { s = \\_ => "" } ;
      det = { s = \\_,_ => "" ; sp = \\_,_ => "" ; n = n} ;
    } ;
  
  dummyNP : Str -> NounPhrase = \s -> regNP s s s s s s Masc Sg ;
	  
  emptyNP : NounPhrase = { s = \\_ => ""; g = Masc; n = Sg; p = P1 ; adv = "" ; preap, postap = { s = \\_ => "" } ; det = { s = \\_,_ => "" ; sp = \\_,_ => "" ; n = Sg } ;}; 
-- also used for adjectives and so on

-- adjectives

  AdjectivePhrase : Type = { 
	s : Agr => Str ;
--	isPre : Bool ; -- should have no use in latin because adjectives can appear variably before and after nouns
      } ;
  mkAdjective : (bonus,bona,bonum : Noun) -> 
    ( (Agr => Str) * Str ) -> 
    ( (Agr => Str) * Str ) ->
    (bono,bonius,bonissimo : Str) ->
    Adjective = 
    \bonus,bona,bonum,melior,optimus,bono,bonius,bonissimo ->
    {
      s = table {
	Posit => table {
	  Ag Masc  n c => bonus.s ! n ! c ;
	  Ag Fem   n c => bona.s  ! n ! c ;
	  Ag Neutr n c => bonum.s ! n ! c 
	  } ;
	Compar => melior.p1 ;
	Superl => optimus.p1 
	} ;
      comp_adv = melior.p2 ;
      super_adv = optimus.p2 ;
      adv = { s = table { Posit => bono ; Compar => bonius ; Superl => bonissimo } };
    } ;


  noun3adj : Str -> Str -> Gender -> Noun = \audax,audacis,g ->
    let 
      audac   = Predef.tk 2 audacis ;
      audacem = case g of {Neutr => audax ; _ => audac + "em"} ;
      audaces = case g of {Neutr => audac +"ia" ; _ => audac + "es"} ;
      audaci  = audac + "i" ;
    in
    mkNoun
      audax audacem (audac + "is") audaci audaci audax
      audaces audaces (audac + "ium") (audac + "ibus") 
      g ;


  emptyAdj : Adjective = 
    { s = \\_,_ => "" ; comp_adv = "" ; super_adv = "" ; adv = { s = \\_ => "" } } ; 

-- verbs

param 
  VActForm  = VAct VAnter VTense Number Person ;
  VPassForm = VPass VTense Number Person ; -- No anteriority because perfect forms are built using participle
  VInfForm  = VInfActPres | VInfActPerf Gender | VInfActFut Gender | VInfPassPres | VInfPassPerf Gender | VinfPassFut ;
  VImpForm  = VImp1 Number | VImp2 Number Person ;
  VGerund   = VGenAcc | VGenGen |VGenDat | VGenAbl ;
  VSupine   = VSupAcc | VSupAbl ;
  VPartForm = VActPres | VActFut | VPassPerf ;

  VAnter = VAnt | VSim ;
  VTense = VPres VMood | VImpf VMood | VFut ; 
  VMood  = VInd | VConj ;

  VQForm = VQTrue | VQFalse ; -- Question suffix should be added to the Verb
  
  oper
  VerbPhrase : Type = {
    s : VActForm => VQForm => Str ;
    part : VPartForm =>Agr => Str ;
    inf : VInfForm => Str ;
    imp : VImpForm => Str ;
    obj : Str ;
    compl : Agr => Str ; -- general complement. Agr might be ignored except for adjectives
    adv : Str
    } ;

  ObjectVerbPhrase : Type = VerbPhrase ** {c : Preposition} ;

  Verb : Type = {
    act   : VActForm => Str ;
    pass  : VPassForm => Str ;
    inf   : VInfForm => Str ;
    imp   : VImpForm => Str ;
    ger   : VGerund => Str ;
    geriv : Agr => Str ; 
    sup   : VSupine => Str ;
    part  : VPartForm =>Agr => Str ;
    } ;

  Verb2 : Type = Verb ** { c : Preposition };
  Verb3 : Type = Verb ** { c : Preposition ; c2 : Preposition };
  
  VV : Type = Verb ** { isAux : Bool } ;

  tenseToVTense : Tense -> VTense = 
    \t ->
    case t of
    {
      Pres => VPres VInd ;
      Past => VImpf VInd ;
      Fut => VFut ;
      Cond => VPres VConj -- don't know what to do
    } ;
  
  anteriorityToVAnter : Anteriority -> VAnter = 
    \a ->
    case a of
    {
      Simul => VSim ;
      Anter => VAnt
    } ;

  useVV : VV -> Verb = \vv ->
    {
      act = vv.act ;
      pass = vv.pass ;
      inf = vv.inf ;
      imp = vv.imp ;
      ger = vv.ger ;
      geriv = vv.geriv ;
      sup = vv.sup ;
      part = vv.part ;
    } ;

  useVPasV : VerbPhrase -> Verb = \vp ->
    {
      act = \\a => vp.obj ++ vp.s ! a ! VQFalse;
      pass = \\_ => nonExist ;
      --      inf = \\a => vp.obj ++ vp.inf ! a ;
      inf = vp.inf ;
      imp = vp.imp ;
      ger = \\_ => nonExist ;
      geriv = \\_ => nonExist ;
      sup = \\_ => nonExist ;
      part = \\_,_ => nonExist ;
    } ;

  mkVerb : 
    (regere,reg,regi,rega,regeba,regere,rege,regi,rex,rex,rexeri,rexera,rexisse,rexeri,rect : Str) 
    -> Verb = 
    \inf_act_pres,pres_stem,pres_ind_base,pres_conj_base,impf_ind_base,impf_conj_base,fut_I_base,imp_base,
    perf_stem,perf_ind_base,perf_conj_base,pqperf_ind_base,pqperf_conj_base,fut_II_base,part_stem -> 
    let
      fill : Str * Str * Str = case pres_stem of {
	_ + ( "a" | "e" ) => < "" , "" , "" > ;
	_ + #consonant => < "e" , "u" , "i" > ;
	_ => < "e" , "u" , "" >
	} ;
    in 
    {
      act = 
	table {
          VAct VSim (VPres VInd)  Sg P1 => -- Present Indicative
	    ( case pres_ind_base of {
		_ + "a" =>  ( init pres_ind_base ) ;
		_ => pres_ind_base
		}
	    ) + "o" ; --actPresEnding Sg P1 ;
	  VAct VSim (VPres VInd)  Pl P3 => -- Present Indicative
	    pres_ind_base + fill.p2 + actPresEnding Pl P3 ;
          VAct VSim (VPres VInd)  n  p  =>  -- Present Indicative
	    pres_ind_base + fill.p3 + actPresEnding n p ;
          VAct VSim (VPres VConj) n  p  => -- Present Conjunctive
	    pres_conj_base + actPresEnding n p ; 
          VAct VSim (VImpf VInd)  n  p  => -- Imperfect Indicative
	    impf_ind_base + actPresEnding n p ; 
          VAct VSim (VImpf VConj) n  p  => -- Imperfect Conjunctive
	    impf_conj_base + actPresEnding n p ; 
          VAct VSim VFut          Sg P1 => -- Future I
	    case fut_I_base of {
	      _ + "bi" => ( init fut_I_base ) + "o" ;
	      _  => ( init fut_I_base ) + "a" + actPresEnding Sg P1 
	    } ;
	  VAct VSim VFut          Pl P3 => -- Future I
	    ( case fut_I_base of {
		_ + "bi" => ( init fut_I_base ) + "u";
		_ => fut_I_base
		} 
	    ) + actPresEnding Pl P3 ;
          VAct VSim VFut          n  p  => -- Future I
	    fut_I_base + actPresEnding n p ; 
          VAct VAnt (VPres VInd)  n  p  => -- Prefect Indicative
	    perf_ind_base + actPerfEnding n p ; 
          VAct VAnt (VPres VConj) n  p  => -- Prefect Conjunctive
	    perf_conj_base + actPresEnding n p ; 
          VAct VAnt (VImpf VInd)  n  p  => -- Plusperfect Indicative
	    pqperf_ind_base + actPresEnding n p ; 
          VAct VAnt (VImpf VConj) n  p  => -- Plusperfect Conjunctive
	    pqperf_conj_base + actPresEnding n p ; 
          VAct VAnt VFut          Sg P1 => -- Future II 
	    ( init fut_II_base ) + "o" ; 
          VAct VAnt VFut          n  p  => -- Future II 
	    fut_II_base + actPresEnding n p 
        } ;
      pass = 
	table {
	  VPass (VPres VInd)  Sg P1 => -- Present Indicative
	    ( case pres_ind_base of
		{
		  _ + "a" => (init pres_ind_base ) ;
		  _ => pres_ind_base
		}
	    )  + "o" + passPresEnding Sg P1 ;
	  VPass (VPres VInd)  Sg P2 => -- Present Indicative
	    ( case imp_base of {
		_ + #consonant => 
		  ( case pres_ind_base of {
		      _ + "i" => ( init pres_ind_base ) ;
		      _ => pres_ind_base 
		      }
		  ) + "e" ;
		_ => pres_ind_base 
		}
	    ) + passPresEnding Sg P2 ;
	  VPass (VPres VInd)  Pl P3 => -- Present Indicative
	    pres_ind_base + fill.p2 + passPresEnding Pl P3 ;
	  VPass (VPres VInd)  n  p  => -- Present Indicative
	    pres_ind_base + fill.p3 + passPresEnding n p ;
	  VPass (VPres VConj) n  p  => -- Present Conjunctive
	    pres_conj_base + passPresEnding n p ;
	  VPass (VImpf VInd)  n  p  => -- Imperfect Indicative
	    impf_ind_base + passPresEnding n p ;
	  VPass (VImpf VConj) n  p  => -- Imperfect Conjunctive
	    impf_conj_base + passPresEnding n p ;
	  VPass VFut          Sg P1 => -- Future I
	    ( case fut_I_base of {
		_ + "bi" => ( init fut_I_base ) + "o" ;
		_ => ( init fut_I_base ) + "a"
		}
	    ) + passPresEnding Sg P1 ;
	  VPass VFut          Sg P2 => -- Future I
	    ( init fut_I_base ) + "e" + passPresEnding Sg P2 ;
	  VPass VFut          Pl P3 => -- Future I
	    ( case fut_I_base of {
		_ + "bi" => ( init fut_I_base ) + "u" ;
		_ => fut_I_base
		}
	    ) + passPresEnding Pl P3 ;
	  VPass VFut          n  p  => -- Future I
	    fut_I_base + passPresEnding n p
	} ;
      inf = 
	table {
          VInfActPres      => -- Infinitive Active Present
	    inf_act_pres ;
          VInfActPerf _    => -- Infinitive Active Perfect
	    perf_stem + "isse" ;
	  VInfActFut Masc  => -- Infinitive Active Future
	    part_stem + "urum" ;
	  VInfActFut Fem   => -- Infinitive Active Future
	    part_stem + "uram" ; 
	  VInfActFut Neutr => -- Infinitive Active Future
	    part_stem + "urum" ;
	  VInfPassPres       => -- Infinitive Present Passive
	    ( init inf_act_pres ) + "i" ;
	  VInfPassPerf Masc  => -- Infinitive Perfect Passive
	    part_stem + "um" ;
	  VInfPassPerf Fem   => -- Infinitive Perfect Passive
	    part_stem + "am" ;
	  VInfPassPerf Neutr => -- Infinitive Perfect Passive
	    part_stem + "um" ;
	  VInfPassFut        => -- Infinitive Future Passive
	    part_stem + "um"
        } ;
      imp = 
	let 
	  imp_fill : Str * Str =
	    case imp_base of {
	      _ + #consonant => < "e" , "i" > ;
	      _ => < "" , "" >
	    };
	  in
	table {
	  VImp1 Sg           => -- Imperative I
	    imp_base + imp_fill.p1 ;
	  VImp1 Pl           => -- Imperative I
	    imp_base + imp_fill.p2 + "te" ;
	VImp2 Sg ( P2 | P3 ) => -- Imperative II
	  imp_base + imp_fill.p2 + "to" ;
	VImp2 Pl P2          => -- Imperative II
	  imp_base +
	  ( case imp_base of {
	      _ + #consonant => "i" ;
	      _ => fill.p3
	      }
	  ) + "tote" ;
	VImp2 Pl P3          => -- Imperative II 
	  pres_stem + fill.p2 + "nto" ;
	_ => nonExist -- No imperative form
	} ;
      ger = 
	table {
	  VGenAcc => -- Gerund
	    pres_stem + fill.p1 + "ndum" ;
	  VGenGen => -- Gerund
	    pres_stem + fill.p1 + "ndi" ;
	  VGenDat => -- Gerund
	    pres_stem + fill.p1 + "ndo" ;
	  VGenAbl => -- Gerund
	    pres_stem + fill.p1 + "ndo" 
	} ;
      geriv = 
      	( mkAdjective
      	    ( mkNoun ( pres_stem + fill.p1 + "ndus" ) ( pres_stem + fill.p1 + "ndum" ) ( pres_stem + fill.p1 + "ndi" ) 
      		( pres_stem + fill.p1 + "ndo" ) ( pres_stem + fill.p1 + "ndo" ) ( pres_stem + fill.p1 + "nde" ) 
      		( pres_stem + fill.p1 + "ndi" ) ( pres_stem + fill.p1 + "ndos" ) ( pres_stem + fill.p1 + "ndorum" ) 
      		( pres_stem + fill.p1 + "ndis" ) 
      		Masc )
      	    ( mkNoun ( pres_stem + fill.p1 + "nda" ) ( pres_stem + fill.p1 + "ndam" ) ( pres_stem + fill.p1 + "ndae" ) 
      		( pres_stem + fill.p1 + "ndae" ) ( pres_stem + fill.p1 + "nda" ) ( pres_stem + fill.p1 + "nda" ) 
      		( pres_stem + fill.p1 + "ndae" ) ( pres_stem + fill.p1 + "ndas" ) (pres_stem + fill.p1 +"ndarum" ) 
      		( pres_stem + fill.p1 + "ndis" ) 
      		Fem )
      	    ( mkNoun ( pres_stem + fill.p1 + "ndum" ) ( pres_stem + fill.p1 + "ndum" ) ( pres_stem + fill.p1 + "ndi" ) 
      		( pres_stem + fill.p1 + "ndo" ) ( pres_stem + fill.p1 + "ndo" ) ( pres_stem + fill.p1 + "ndum" ) 
      		( pres_stem + fill.p1 + "nda" ) ( pres_stem + fill.p1 + "nda" ) ( pres_stem + fill.p1 + "ndorum" ) 
      		( pres_stem + fill.p1 + "ndis" ) 
      		Neutr )
      	    < \\_ => "" , "" > -- Comparative
      	    < \\_ => "" , "" > -- Superlative
	    "" "" "" -- Adverb part
      	).s!Posit ;
      sup = 
	table {
	  VSupAcc => -- Supin
	    part_stem + "um" ;
	  VSupAbl => -- Supin
	    part_stem + "u" 
	} ;
      part= table {
	VActPres => table {
	  Ag ( Fem | Masc ) n c => 
	    ( mkNoun ( pres_stem + fill.p1 + "ns" ) ( pres_stem + fill.p1 + "ntem" ) ( pres_stem + fill.p1 + "ntis" ) 
		( pres_stem + fill.p1 + "nti" ) ( pres_stem + fill.p1 + "nte" ) ( pres_stem + fill.p1 + "ns" ) 
		( pres_stem + fill.p1 + "ntes" ) ( pres_stem + fill.p1 + "ntes" ) ( pres_stem + fill.p1 + "ntium" ) 
		( pres_stem + fill.p1 + "ntibus" ) 
 		Masc ).s ! n ! c ;
	  Ag Neutr n c =>
	    ( mkNoun ( pres_stem + fill.p1 + "ns" ) ( pres_stem + fill.p1 + "ns" ) ( pres_stem + fill.p1 + "ntis" ) 
		( pres_stem + fill.p1 + "nti" ) ( pres_stem + fill.p1 + "nte" ) ( pres_stem + fill.p1 + "ns" ) 
		( pres_stem + fill.p1 + "ntia" ) ( pres_stem + fill.p1 + "ntia" ) ( pres_stem + fill.p1 + "ntium" ) 
		( pres_stem + fill.p1 + "ntibus" ) 
 		Masc ).s ! n ! c
	} ;
	
	VActFut => 
	  ( mkAdjective
	      ( mkNoun ( part_stem + "urus" ) ( part_stem + "urum" ) ( part_stem + "uri" ) 
		  ( part_stem + "uro" ) ( part_stem + "uro" ) ( part_stem + "ure" ) ( part_stem + "uri" ) 
		  ( part_stem + "uros" ) ( part_stem + "urorum" ) ( part_stem + "uris" ) 
		  Masc )
	      ( mkNoun ( part_stem + "ura" ) ( part_stem + "uram" ) ( part_stem + "urae" ) 
		  ( part_stem + "urae" ) ( part_stem + "ura" ) ( part_stem + "ura" )( part_stem + "urae" ) 
		  ( part_stem + "uras" ) ( part_stem +"urarum" ) ( part_stem + "uris" ) 
		  Fem )
	      ( mkNoun ( part_stem + "urum" ) ( part_stem + "urum" ) ( part_stem + "uri" ) 
		  ( part_stem + "uro" ) ( part_stem + "uro" ) ( part_stem + "urum" ) ( part_stem + "ura" ) 
		  ( part_stem + "ura" ) ( part_stem + "urorum" ) ( part_stem + "uris" ) 
		  Neutr )
	      < \\_ => "" , "" >
	      < \\_ => "" , "" >
	      "" "" ""
	  ).s!Posit ;
	VPassPerf => 
	  ( mkAdjective
	      ( mkNoun ( part_stem + "us" ) ( part_stem + "um" ) ( part_stem + "i" ) ( part_stem + "o" ) 
		  ( part_stem + "o" ) ( part_stem + "e" ) ( part_stem + "i" ) ( part_stem + "os" ) 
		  ( part_stem + "orum" ) ( part_stem + "is" ) 
		  Masc )
	      ( mkNoun ( part_stem + "a" ) ( part_stem + "am" ) ( part_stem + "ae" ) ( part_stem + "ae" ) 
		  ( part_stem + "a" ) ( part_stem + "a" ) ( part_stem + "ae" ) ( part_stem + "as" ) 
		  ( part_stem + "arum" ) ( part_stem + "is" ) 
		  Fem )
	      ( mkNoun ( part_stem + "um" ) ( part_stem + "um" ) ( part_stem + "i" ) ( part_stem + "o" ) 
		( part_stem + "o" ) ( part_stem + "um" ) ( part_stem + "a" ) ( part_stem + "a" ) 
		  ( part_stem + "orum" ) ( part_stem + "is" ) 
		  Neutr ) 
	      < \\_ => "" , "" >
	      < \\_ => "" , "" >
	      "" "" ""
	  ).s!Posit
	}
    } ;
 

  mkDeponent : ( sequi,sequ,sequi,sequa,sequeba,sequere,seque,sequi,secut : Str ) -> Verb =
    \inf_pres,pres_stem,pres_ind_base,pres_conj_base,impf_ind_base,impf_conj_base,fut_I_base,imp_base,part_stem -> 
    let fill : Str * Str =
	  case pres_ind_base of {
	    _ + ( "a" | "e" ) => < "" , "" >;
	    _ => < "u" , "e" > 
	  }
    in
    {
      act = 
	table {
          VAct VSim (VPres VInd)  Sg P1 => -- Present Indicative
	    ( case pres_ind_base of {
		_ + "a" =>  ( init pres_ind_base ) ;
		_ => pres_ind_base
		}
	    ) + "o" + passPresEnding Sg P1 ;
	  VAct VSim (VPres VInd)  Sg P2 => -- Present Indicative
	    ( case inf_pres of {
		_ + "ri" => pres_ind_base  ;
		_ => ( case pres_ind_base of {
			 _ + "i" => init pres_ind_base ;
			 _ => pres_ind_base
			 }
		  ) + "e"
		}
	    ) + passPresEnding Sg P2 ;
          VAct VSim (VPres VInd)  Pl P3 => -- Present Indicative
	    pres_ind_base + fill.p1 + passPresEnding Pl P3 ;
          VAct VSim (VPres VInd)  n  p  => -- Present Indicative
	    pres_ind_base +
	    ( case pres_ind_base of {
		_ + #consonant => "i" ;
		_ => ""
		}
	    ) + passPresEnding n p ;
          VAct VSim (VPres VConj) n  p  => -- Present Conjunctive
	    pres_conj_base + passPresEnding n p ; 
          VAct VSim (VImpf VInd)  n  p  => -- Imperfect Indicative
	    impf_ind_base + passPresEnding n p ;
          VAct VSim (VImpf VConj) n  p  => -- Imperfect Conjunctive
	    impf_conj_base + passPresEnding n p ;
          VAct VSim VFut          Sg P1 => -- Future I
	    (init fut_I_base ) + 
	    ( case fut_I_base of {
		_ + "bi" => "o" ;
		_ => "a" 
		}
	    ) + passPresEnding Sg P1 ;
	  VAct VSim VFut          Sg P2 => -- Future I
	    ( case fut_I_base of {
		_ + "bi" => ( init fut_I_base ) + "e" ;
		_ => fut_I_base
		}
	    ) + passPresEnding Sg P2 ;
	  VAct VSim VFut          Pl P3 => -- Future I
	    (init fut_I_base ) + 
	    ( case fut_I_base of {
		_ + "bi" => "u" ;
		_ => "e" 
		}
	    ) + passPresEnding Pl P3 ;

          VAct VSim VFut          n  p  => -- Future I
	    fut_I_base + passPresEnding n p ;
          VAct VAnt (VPres VInd)  n  p  => -- Prefect Indicative
	    nonExist ; -- Use participle
          VAct VAnt (VPres VConj) n  p  => -- Prefect Conjunctive
	    nonExist ; -- Use participle
          VAct VAnt (VImpf VInd)  n  p  => -- Plusperfect Indicative
	    nonExist ; -- Use participle
          VAct VAnt (VImpf VConj) n  p  => -- Plusperfect Conjunctive
	    nonExist ; -- Use participle
          VAct VAnt VFut          n  p  => -- Future II
	    nonExist -- Use participle
        } ;
      pass =
	\\_ => nonExist ; -- no passive forms
      inf =
	table {
          VInfActPres        => -- Infinitive Present Active
	    inf_pres ;
          VInfActPerf Masc   => -- Infinitive Perfect Active
	    part_stem + "um" ;
	  VInfActPerf Fem    => -- Infinitive Perfect Active
	    part_stem + "am" ;
	  VInfActPerf Neutr  => -- Infinitive Perfect Active
	    part_stem + "um" ;
	  VInfActFut Masc    => -- Infinitive Future Active
	    part_stem + "urum" ;
	  VInfActFut Fem     => -- Infinitive Perfect Active
	    part_stem + "uram" ; 
	  VInfActFut Neutr   => -- Infinitive Perfect Active
	    part_stem + "urum" ;
	  VInfPassPres       => -- Infinitive Present Passive
	    nonExist ; -- no passive form
	  VInfPassPerf _     => -- Infinitive Perfect Passive
	    nonExist ; -- no passive form
	  VInfPassFut        => -- Infinitive Future Passive
	    nonExist  -- no passive form
        } ;
      imp = 
	table {
	  VImp1 Sg             => -- Imperative I
	    ( case inf_pres of {
		_ + "ri" => imp_base ;
		_ => (init imp_base ) + "e" 
		}
	    ) + "re" ;
	  VImp1 Pl             => -- Imperative I
	    imp_base + "mini" ;
	  VImp2 Sg ( P2 | P3 ) => -- Imperative II
	    imp_base + "tor" ;
	  VImp2 Pl P2          => -- Imperative II
	    nonExist ; -- really no such form?
	  VImp2 Pl P3          => -- Imperative II
	    pres_ind_base + fill.p1 + "ntor" ;
	  _ => nonExist -- No imperative form
	} ;
      ger = 
	table {
	  VGenAcc => -- Gerund
	    pres_stem + fill.p2 + "ndum" ;
	  VGenGen => -- Gerund
	    pres_stem + fill.p2 + "ndi" ;
	  VGenDat => -- Gerund
	    pres_stem + fill.p2 + "ndo" ;
	  VGenAbl => -- Gerund
	    pres_stem + fill.p2 + "ndo" 
	} ;
      geriv =
	( mkAdjective
	    ( mkNoun ( pres_stem + fill.p2 + "ndus" ) ( pres_stem + fill.p2 + "ndum" ) 
		( pres_stem + fill.p2 + "ndi" ) ( pres_stem + fill.p2 + "ndo" ) ( pres_stem + fill.p2 + "ndo" ) 
		( pres_stem + fill.p2 + "nde" ) ( pres_stem + fill.p2 + "ndi" ) ( pres_stem + fill.p2 + "ndos" ) 
		( pres_stem + fill.p2 + "ndorum" ) ( pres_stem + fill.p2 + "ndis" ) 
		Masc )
	    ( mkNoun ( pres_stem + fill.p2 + "nda" ) ( pres_stem + fill.p2 + "ndam" ) 
		( pres_stem + fill.p2 + "ndae" ) ( pres_stem + fill.p2 + "ndae" ) ( pres_stem + fill.p2 + "nda" ) 
		      ( pres_stem + fill.p2 + "nda" ) ( pres_stem + fill.p2 + "ndae" ) ( pres_stem + fill.p2 + "ndas" ) 
		(pres_stem + fill.p2 +"ndarum" ) ( pres_stem + fill.p2 + "ndis" ) 
		Fem )
	    ( mkNoun ( pres_stem + fill.p2 + "ndum" ) ( pres_stem + fill.p2 + "ndum" ) 
		( pres_stem + fill.p2 + "ndi" ) ( pres_stem + fill.p2 + "ndo" ) ( pres_stem + fill.p2 + "ndo" ) 
		( pres_stem + fill.p2 + "ndum" ) ( pres_stem + fill.p2 + "nda" ) ( pres_stem + fill.p2 + "nda" ) 
		( pres_stem + fill.p2 + "ndorum" ) ( pres_stem + fill.p2 + "ndis" ) 
		      Neutr )
	    < \\_ => "" , "" >
	    < \\_ => "" , "" >
	    "" "" ""
	).s!Posit ;
      sup = 
	table {
	  VSupAcc => -- Supin
	    part_stem + "um" ;
	  VSupAbl => -- Supin
	    part_stem + "u" 
	} ;
      -- Bayer-Lindauer 44 1
      part = table {
	VActPres =>
	  table {
	    Ag ( Fem | Masc ) n c =>
	      ( mkNoun ( pres_stem + fill.p2 + "ns" ) ( pres_stem + fill.p2 + "ntem" ) 
		  ( pres_stem + fill.p2 + "ntis" ) ( pres_stem + fill.p2 + "nti" ) ( pres_stem + fill.p2 + "nte" ) 
		  ( pres_stem + fill.p2 + "ns" ) ( pres_stem + fill.p2 + "ntes" ) ( pres_stem + fill.p2 + "ntes" ) 
		  ( pres_stem + fill.p2 + "ntium" ) ( pres_stem + fill.p2 + "ntibus" ) 
 		  Masc ).s ! n ! c ;
	    Ag Neutr n c => 
	      ( mkNoun ( pres_stem + fill.p2 + "ns" ) ( pres_stem + fill.p2 + "ns" ) 
		( pres_stem + fill.p2 + "ntis" ) ( pres_stem + fill.p2 + "nti" ) ( pres_stem + fill.p2 + "nte" ) 
		  ( pres_stem + fill.p2 + "ns" ) ( pres_stem + fill.p2 + "ntia" ) ( pres_stem + fill.p2 + "ntia" ) 
		  ( pres_stem + fill.p2 + "ntium" ) ( pres_stem + fill.p2 + "ntibus" ) 
 		  Masc ).s ! n ! c 
	  } ;
	VActFut => 
	  ( mkAdjective
	      ( mkNoun ( part_stem + "urus" ) ( part_stem + "urum" ) ( part_stem + "uri" ) 
		  ( part_stem + "uro" ) ( part_stem + "uro" ) ( part_stem + "ure" ) ( part_stem + "uri" ) 
		  ( part_stem + "uros" ) ( part_stem + "urorum" ) ( part_stem + "uris" ) 
		  Masc )
	      ( mkNoun ( part_stem + "ura" ) ( part_stem + "uram" ) ( part_stem + "urae" ) 
		  ( part_stem + "urae" ) ( part_stem + "ura" ) ( part_stem + "ura" )( part_stem + "urae" ) 
		  ( part_stem + "uras" ) ( part_stem +"urarum" ) ( part_stem + "uris" ) 
		  Fem )
	      ( mkNoun ( part_stem + "urum" ) ( part_stem + "urum" ) ( part_stem + "uri" ) 
		  ( part_stem + "uro" ) ( part_stem + "uro" ) ( part_stem + "urum" ) ( part_stem + "ura" ) 
		  ( part_stem + "ura" ) ( part_stem + "urorum" ) ( part_stem + "uris" ) 
		  Neutr )
	      < \\_ => "" , "" >
	      < \\_ => "" , "" >
	      "" "" ""
	  ).s!Posit ;
	VPassPerf =>
	  ( mkAdjective
	      ( mkNoun ( part_stem + "us" ) ( part_stem + "um" ) ( part_stem + "i" ) 
		  ( part_stem + "o" ) ( part_stem + "o" ) ( part_stem + "e" ) 
		  ( part_stem + "i" ) ( part_stem + "os" ) ( part_stem + "orum" ) 
		  ( part_stem + "is" ) 
		  Masc )
	      ( mkNoun ( part_stem + "a" ) ( part_stem + "am" ) ( part_stem + "ae" ) 
		  ( part_stem + "ae" ) ( part_stem + "a" ) ( part_stem + "a" ) 
		  ( part_stem + "ae" ) ( part_stem + "as" ) ( part_stem + "arum" ) 
		  ( part_stem + "is" ) 
		  Fem )
	      ( mkNoun ( part_stem + "um" ) ( part_stem + "um" ) ( part_stem + "i" ) 
		  ( part_stem + "o" ) ( part_stem + "o" ) ( part_stem + "um" ) 
		  ( part_stem + "a" ) ( part_stem + "a" ) ( part_stem + "orum" ) 
		  ( part_stem + "is" ) 
		  Neutr ) 
	      < \\_ => "" , "" >
	      < \\_ => "" , "" >
	      "" "" ""
	  ).s!Posit
	}
    } ;

  -- at the moment only fills present tense
  mkImpersonal : Str -> Verb = \s ->
    {
      act = table {
	VAct VSim (VPres VInd)  Sg P1 => s ;
	_ => nonExist
	} ;
      pass = \\_ => nonExist ;
      imp = \\_ => nonExist ;
      inf = \\_ => nonExist ;
      ger = \\_ => nonExist ;
      geriv = \\_ => nonExist ;
      part = \\_,_ => nonExist ;
      sup = \\_ => nonExist ;
    } ;
  
  actPresEnding : Number -> Person -> Str = 
    useEndingTable <"m", "s", "t", "mus", "tis", "nt"> ;

  actPerfEnding : Number -> Person -> Str = 
    useEndingTable <"i", "isti", "it", "imus", "istis", "erunt"> ;

  passPresEnding : Number -> Person -> Str =
    useEndingTable <"r", "ris", "tur", "mur", "mini", "ntur"> ;

  passFutEnding : Str -> Number -> Person -> Str = 
    \lauda,n,p ->
    let endings : Str * Str * Str * Str * Str * Str = case lauda of {
	  ( _ + "a" ) | 
	    ( _ + "e" ) => < "bo" , "be" , "bi" , "bi" , "bi" , "bu" > ;
	  _             => < "a"  , "e"  , "e"  , "e"  , "e"  , "e"  >
	  }
    in
    (useEndingTable endings n p) + passPresEnding n p ;
  
  useEndingTable : (Str*Str*Str*Str*Str*Str) -> Number -> Person -> Str = 
    \es,n,p -> case n of {
      Sg => case p of {
        P1 => es.p1 ;
        P2 => es.p2 ;
        P3 => es.p3
        } ;
      Pl => case p of {
        P1 => es.p4 ;
        P2 => es.p5 ;
        P3 => es.p6
        }
      } ;


    addPrefix : Str -> Str -> Str =
      \prefix,verb ->
      case <prefix,verb> of {
	<p + "b","f"      + _ > => p + verb ;
	<p + "b", "te"    + r > => prefix + "sti" + r;
	<p + "b", "t"     + r > => prefix + "s" + verb;
	<p + "b", "iact"  + r > => prefix + "iect" + r;
	<p + "b", "iac"   + r > => prefix + "ic" + r;
	<p + "d","capt"   + r > => p + "ccept" + r ;
	<p + "d","ca"     + r > => p + "cci" + r ;
	<p      ,"spe"     + r > => p + "spi" + r ;
	<p      ,"spex"     + r > => prefix + verb;
	<p      ,"stat"     + r > => p + "stit" + r ;
	<p      ,"tex"     + r > => p + verb ;
	<p      ,"te"     + r > => p + "ti" + r ;
	<p + "d","c"      + _ >  => p + "c" + verb ;
	<p + "d","t"      + _ >  => p + "t" + verb ;
	<p + "d","l"      + _  > => p + "l" + verb ;
	_ => prefix + verb
      } ;-- TODO too simple e.g. ab+fuit = afuit

    prefixVerb2 : Str -> Verb2 -> Verb2 =
      \prefix,verb ->
      let v = { act = verb.act ; pass = verb.pass ; inf = verb.inf ; imp = verb.imp ; ger = verb.ger ; geriv = verb.geriv ; sup = verb.sup ; part = verb.part }
      in
      (prefixVerb prefix v) ** { c = verb.c };
    
    prefixVerb : Str -> Verb -> Verb =
      \prefix,verb ->
      {
	act = \\form => addPrefix prefix (verb.act ! form) ;
	pass = \\form => addPrefix prefix (verb.pass ! form) ;
	inf = \\form => addPrefix prefix (verb.inf ! form) ;
	imp = \\form => addPrefix prefix (verb.imp ! form) ;
	ger = \\form => addPrefix prefix (verb.ger ! form) ;
	geriv = \\agr => addPrefix prefix (verb.geriv ! agr) ;
	sup = \\form => addPrefix prefix (verb.sup ! form) ;
	part = \\form,agr => addPrefix prefix (verb.part ! form ! agr) ;
      } ;


    esseAux : Verb =     -- Bayer-Lindauer 93 1
    let
      pres_stem = "s" ;
      pres_ind_base = "su" ;
      pres_conj_base = "si" ;
      impf_ind_base = "era" ;
      impf_conj_base = "esse" ;
      fut_I_base = "eri" ;
      imp_base = "es" ;
      perf_stem = "fu" ;
      perf_ind_base = "fu" ;
      perf_conj_base = "fueri" ;
      pqperf_ind_base = "fuera" ;
      pqperf_conj_base = "fuisse" ;
      fut_II_base = "fueri" ;
      part_stem = "fut" ;
      verb = mkVerb "esse" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
    	imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
    in
    {
      act = 
	table {
    	  VAct VSim (VPres VInd)  n  p  => 
	    table Number [ table Person [ "sum" ; "es" ; "est" ] ;
    			   table Person [ "sumus" ; "estis" ; "sunt" ]
    	    ] ! n ! p ;
    	  a => verb.act ! a
	};
      pass =
	\\_ => nonExist ; -- no passive forms 
      inf =
	verb.inf ;
      imp =
	table {
	  VImp1 Sg => "es" ;
	  VImp1 Pl => "este" ;
	  VImp2 Pl P2 => "estote" ;
	  a => verb.imp ! a
	} ;
      sup =
	\\_ => nonExist ; -- no supin forms
      ger =
	\\_ => nonExist ; -- no gerund forms
      geriv = 
	\\_ => nonExist ; -- no gerundive forms
      part = table {
	VActFut =>
	  verb.part ! VActFut ;
	VActPres => 
	  \\_ => nonExist ; -- no such participle
	VPassPerf =>
	  \\_ => nonExist -- no such participle
	}
    } ;

  ferreAux : Verb =
    let
      pres_stem = "fer" ;
      pres_ind_base = "fer" ;
      pres_conj_base = "fera" ;
      impf_ind_base = "fereba" ;
      impf_conj_base = "ferre" ;
      fut_I_base = "fere" ;
      imp_base = "fer" ;
      perf_stem = "tul" ;
      perf_ind_base = "tul" ;
      perf_conj_base = "tuleri" ;
      pqperf_ind_base = "tulera" ;
      pqperf_conj_base = "tulisse" ;
      fut_II_base = "tuleri" ;
      part_stem = "lat" ;
      verb = mkVerb "ferre" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
	imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
    in
    {
      act =
	table {
	  VAct VSim (VPres VInd) n p => 
	    table Number [ table Person [ "fero" ; "fers" ; "fert" ] ;
			   table Person [ "ferimus" ; "fertis" ; "ferunt" ] 
	    ] ! n ! p ;
	  a => verb.act ! a
	} ;
      pass = 
	table {
	  VPass (VPres VInd) n p => 
	    table Number [ table Person [ "feror" ; "ferris" ; "fertur" ] ;
			   table Person [ "ferimur" ; "ferimini" ; "feruntur" ]
	    ] ! n ! p ;
	  a => verb.pass ! a
	} ;
      inf = 
	verb.inf ;	  
      imp =
	table {
	  VImp1 n => table Number [ "fer" ; "ferte" ] ! n ;
	  VImp2 Sg ( P2 | P3 ) => "ferto" ;
	  VImp2 Pl P2 => "fertote" ;
	  a => verb.imp ! a 
	} ; 
      sup = 
	verb.sup ;
      ger =
	verb.ger ;
      geriv =
	verb.geriv ;
      part = verb.part ;
    };

  posseAux : Verb =
    let
      pres_stem = "pos" ;
      pres_ind_base = "pos" ;
      pres_conj_base = "possi" ;
      impf_ind_base = "potera" ;
      impf_conj_base = "posse" ;
      fut_I_base = "poteri" ;
      imp_base = "" ;
      perf_stem = "potu" ;
      perf_ind_base = "potu" ;
      perf_conj_base = "potueri" ;
      pqperf_ind_base = "potuera" ;
      pqperf_conj_base = "potuisse" ;
      fut_II_base = "potueri" ;
      part_stem = "" ;
      verb = mkVerb "posse" pres_stem pres_ind_base pres_conj_base impf_ind_base impf_conj_base fut_I_base
    	imp_base perf_stem perf_ind_base perf_conj_base pqperf_ind_base pqperf_conj_base fut_II_base part_stem ;
    in
    {
      act =
    	table {
    	  VAct VSim (VPres VInd)  n  p  => 
	    table Number [ table Person [ "possum" ; "potes" ; "potest" ] ;
    			   table Person [ "possumus" ; "potestis" ; "possunt" ]
    	    ] ! n ! p ;
    	  a => verb.act ! a
    	} ;
      pass = 
    	\\_ => nonExist ; -- no passive forms
      inf = 
	table {
	  VInfActFut _ => nonExist ;
	  a => verb.inf ! a
	} ;
      imp = 
	\\_ => nonExist ;
      sup = 
	\\_ => nonExist ;
      ger =
	\\_ => nonExist ;
      geriv =
	\\_ => nonExist ;
      part = table {
	VActFut =>
    	  \\_ => nonExist ; -- no such participle
    	VActPres => 
	  \\_ => nonExist ; -- no such participle
    	VPassPerf =>
    	  \\_ => nonExist -- no such participle
	} ;
    };
  
-- pronouns

param
  PronReflForm = PronRefl | PronNonRefl ;
  PronDropForm = PronDrop | PronNonDrop;
--  PronIndefUsage = PronSubst | PronAdj ;
--  PronIndefPol = PronPos | PronNeg ;
--  PronIndefMeaning = PronSomeone | PronCertainOne | PronEvery ;
--  PronType = PronPers PronReflForm | PronPoss PronReflForm | PronDemo | PronRelat | PronInterrog | 
--    PronIndef PronIndefUsage PronIndefPol PronIndefMeaning ;

oper

  PersonalPronoun = {
    s : PronDropForm => PronReflForm => Case => Str ;
    g : Gender ;
    n : Number
    } ;

  PossessivePronoun = {
    s : PronReflForm => Agr => Str ;
    } ;
  
  -- Pronoun : Type = {
  --   pers : PronDropForm => PronReflForm => Case => Str ;
  --   poss : PronReflForm => Agr => Str ;
  --   g : Gender ;
  --   n : Number ;
  --   p : Person ;
  --   } ;

  Pronoun : Type = {
    pers : PersonalPronoun ;
    poss : PossessivePronoun ;
    p : Person
    } ;
  
  pronForms = overload {
    pronForms : (_,_,_,_,_ : Str) -> Case => Str = 
      \ego,me,mei,mihi,mee -> table Case [ego ; me ; mei ; mihi ; mee ; ego] ;
    pronForms : (_,_,_,_,_,_ : Str) -> Case => Str = 
      \meus,meum,mei,meo,meoo,mi -> table Case [meus ; meum ; mei ; meo ; meoo ; mi] ;
    };
    
  createPronouns : Gender -> Number -> Person -> ( ( PronDropForm => PronReflForm => Case => Str ) * ( PronReflForm => Agr => Str ) ) = \g,n,p ->
    case <g,n,p> of {
      <_,Sg,P1> =>
  	< 
  	table { 
  	  PronDrop    => \\_,_ => "" ;  
  	  PronNonDrop => \\_ => pronForms "ego" "me" "mei" "mihi" "me" "me"
  	},
  	\\_ => table {
  	  Ag Masc  Sg c => ( pronForms "meus" "meum" "mei" "meo" "meo" "mi" ) ! c ;
  	  Ag Masc  Pl c => ( pronForms "mei" "meos" "meorum" "meis" "meis" "mei" ) ! c ;
  	  Ag Fem   Sg c => ( pronForms "mea" "meam" "meae" "meae" "mea" "mea" ) ! c ;
   	  Ag Fem   Pl c => ( pronForms "meae" "meas" "mearum" "meis" "meis" "meae" ) ! c ;
  	  Ag Neutr Sg c => ( pronForms "meum" "meum" "mei" "meo" "meo" "meum" ) ! c ;
   	  Ag Neutr Pl c => ( pronForms "mea" "mea" "meorum" "meis" "meis" "mea" ) ! c
  	}
  	> ;
      <_,    Sg,P2> => 
      	< 
      	table {
      	  PronDrop => \\_,_ => "" ; 
      	  PronNonDrop => \\_ => pronForms "tu"  "te" "tui" "tibi" "te" "te" 
      	} ,
      	\\_ => table {
      	  Ag Masc  Sg c => ( pronForms "tuus" "tuum" "tui" "tuo" "tu" "tue" ) ! c ;
      	  Ag Masc  Pl c => ( pronForms "tui" "tuos" "tuorum" "tuis" "tuis" "tui" ) ! c ;
      	  Ag Fem   Sg c => ( pronForms "tua" "tuam" "tuae" "tuae" "tua" "tua" ) ! c ;
      	  Ag Fem   Pl c => ( pronForms "tuae" "tuas" "tuarum" "tuis" "tuis" "tuae" ) ! c ;
      	  Ag Neutr Sg c => ( pronForms "tuum" "tuum" "tui" "tuo" "tuo" "tuum" ) ! c ;
      	  Ag Neutr Pl c => ( pronForms "tua" "tua" "tuorum" "tuis" "tuis" "tua" ) ! c
      	}
      	> ;
      <_,    Pl,P1> => 
      	< 
      	table { 
      	  PronDrop => \\_,_ => "" ;
      	  PronNonDrop => \\_ => pronForms "nos" "nos" "nostri" "nobis" "nobis" --- nostrum
      	} , 
      	\\_ => table {
      	  Ag Masc  Sg c => ( pronForms "noster" "nostrum" "nostri" "nostro" "nostro" "noster" ) ! c ; 
      	  Ag Masc  Pl c => ( pronForms "nostri" "nostros" "nostrorum" "nostris" "nostris" "nostri" ) ! c ;
      	  Ag Fem   Sg c => ( pronForms "nostra" "nostram" "nostrae" "nostrae" "nostra" "nostra" ) ! c ;
      	  Ag Fem   Pl c => ( pronForms "nostrae" "nostras" "nostrarum" "nostris" "nostris" "nostrae" ) ! c ;
      	  Ag Neutr Sg c => ( pronForms "nostrum" "nostrum" "nostri" "nostro" "nostro" "nostrum" ) ! c ;
      	  Ag Neutr Pl c => ( pronForms "nostra" "nostra" "nostrorum" "nostris" "nostris" "nostra" ) ! c
      	}
      	> ; 
      <_,    Pl,P2> => 
      	< 
      	table {
      	  PronDrop => \\_,_ => "" ; 
      	  PronNonDrop => \\_ => pronForms "vos" "vos" "vestri" "vobis" "vobis"  --- vestrum
      	} ,
      	\\_ => table {
      	  Ag Masc  Sg c => ( pronForms "vester" "vestrum" "vestri" "vestro" "vestro" "vester" ) ! c ;
      	  Ag Masc  Pl c => ( pronForms "vestri" "vestros" "vestrorum" "vestris" "vestris" "vestri" ) ! c ;
      	  Ag Fem   Sg c => ( pronForms "vestra" "vestram" "vestrae" "vestrae" "vestra" "vestra" ) ! c ;
      	  Ag Fem   Pl c => ( pronForms "vestrae" "vestras" "vestrarum" "vestris" "vestris" "vestrae" ) ! c ;
      	  Ag Neutr Sg c => ( pronForms "vestrum" "vestrum" "vestri" "vestro" "vestro" "vestrum" ) ! c ;
      	  Ag Neutr Pl c => ( pronForms "vestra" "vestra" "vestrorum" "vestris" "vestris" "vestra" ) ! c
      	}
      	>; 
      <_,_ ,P3> => 
      	<
      	table {
      	  PronDrop => \\_,_ => "" ;
      	  PronNonDrop =>
      	    table { 
      	      PronNonRefl => 
      		case <g,n> of {
      		  <Masc ,Sg> => pronForms "is"  "eum" "eius"  "ei" "eo" ;
      		  <Fem  ,Sg> => pronForms "ea"  "eam" "eius"  "ei" "ea" ;
      		  <Neutr,Sg> => pronForms "id"  "id"  "eius"  "ei" "eo" ;
      		  <Masc ,Pl> => pronForms "ei"  "eos" "eorum" "eis" "eis" ;
      		  <Fem  ,Pl> => pronForms "eae" "eas" "earum" "eis" "eis" ;
      		  <Neutr,Pl> => pronForms "ea"  "ea"  "eorum" "eis" "eis"
      		} ;
      	      PronRefl => pronForms nonExist "se" "sui" "sibi" "se"
      	    }
      	} ,
      	table {
      	  PronNonRefl =>
      	    \\_ => nonExist ;
      	  PronRefl =>
      	    table {
      	      Ag Masc  Sg c => ( pronForms "suus" "suum" "sui" "suo" "suo" ) ! c ;
      	      Ag Masc  Pl c => ( pronForms "sui" "suos" "suorum" "suis" "suis" ) ! c ;
      	      Ag Fem   Sg c => ( pronForms "sua" "suam" "suae" "suae" "sua" ) ! c ;
      	      Ag Fem   Pl c => ( pronForms "suae" "suas" "suarum" "suis" "suis" ) ! c ;
      	      Ag Neutr Sg c => ( pronForms "suum" "suum" "sui" "suo" "suo" ) ! c ;
      	      Ag Neutr Pl c => ( pronForms "sua" "sua" "suorum" "suis" "suis" ) ! c
      	    }
      	}
      	> 
--	;
--      _ =>
--    	< \\_,_,_ => "######!" , \\_,_ => "######!" > -- should never be reached
    } ;

  mkPronoun : Gender -> Number -> Person -> Pronoun = \g,n,p ->
    let 
      -- Personal_Form * Possesive_Form
      prons : ( PronDropForm => PronReflForm => Case => Str ) * ( PronReflForm => Agr => Str ) =
      createPronouns g n p ;
    in
    {
     pers = { s = prons.p1 ; g = g ; n = n } ;
     poss = { s = prons.p2 } ;
     p = p
    } ;

-- prepositions

  Preposition : Type = {s : Str ; c : Case ; isPost : Bool } ;

  -- conjunctions
param Coordinator = And | Or | If | Neither | Because | Comma | Colon | Empty ;
oper
  Conjunction : Type = { s1 : Str ; s2 : Str ; n : Number ; c : Coordinator };
  mkConjunction : Str -> Str -> Number -> Coordinator -> Conjunction = \s1,s2,num,coord -> { s1 = s1; s2 = s2 ; n = num ; c = coord } ;
  
  VPSlash = VerbPhrase ** {c : Preposition} ;

  predV : Verb -> VerbPhrase = \v -> {
    s = \\a,q => v.act ! a ++ case q of { VQTrue => Prelude.BIND ++ "ne"; VQFalse => "" };
    part = v.part;
    imp = v.imp ;
    inf = v.inf ;
    obj = [] ;
    compl = \\a => [] ;
    adv = "" 
  } ;

  predV2 : Verb2 -> VPSlash = \v ->
    predV v ** {c = v.c} ;

  predV3 : Verb3 -> VPSlash = \v
    -> predV v ** {c = v.c; c2 = v.c2 } ;

  appPrep : Preposition -> (Case => Str) -> Str = \c,s -> c.s ++ s ! c.c ;

  insertObj : NounPhrase -> Preposition -> VerbPhrase -> VerbPhrase = \np,prep,vp -> {
    s = vp.s ;
    part = vp.part ;
    imp = vp.imp ;
    inf = vp.inf ;
    obj = np.det.s ! np.g ! prep.c ++ np.preap.s ! (Ag np.g np.n prep.c) ++ (appPrep prep np.s) ++ np.postap.s ! (Ag np.g np.n prep.c) ++ np.det.sp ! np.g ! prep.c ++ vp.obj ;
    compl = vp.compl ;
    adv = vp.adv ++ np.adv
  } ;

  insertObjc: NounPhrase -> VPSlash -> VPSlash = \np,vp -> {
    s = vp.s ;
    part = vp.part ;
    imp = vp.imp ;
    inf = vp.inf ;
    obj = np.det.s ! np.g ! vp.c.c ++ np.preap.s ! (Ag np.g np.n vp.c.c) ++ (appPrep vp.c np.s) ++ np.postap.s ! (Ag np.g np.n vp.c.c) ++ np.det.sp ! np.g ! vp.c.c ++ vp.obj ;
    compl = vp.compl ;
    c = vp.c ;
    adv = vp.adv ++ np.adv
    } ;
    
  insertAdj : (Agr => Str) -> VerbPhrase -> VerbPhrase = \adj,vp -> {
    s = vp.s ;
    part = vp.part ;
    imp = vp.imp ;
    inf = vp.inf ;
    obj = vp.obj ;
    compl = \\a => adj ! a ++ vp.compl ! a ;
    adv = vp.adv
  } ;

  insertAdv : Adverb -> VerbPhrase -> VerbPhrase = \a,vp -> {
    s = vp.s ;
    part = vp.part ;
    imp = vp.imp ;
    inf = vp.inf ;
    obj = vp.obj ;
    compl = vp.compl ;
    adv = vp.adv ++ (a.s ! Posit)
    } ;
  
  -- clauses
  Sentence =
    {
      s,o,v,neg : AdvPos => Str ; -- Subject, verbphrase, object and negation particle plus potential adverb
      t : C.Tense ; -- tense marker
      p : C.Pol ; -- polarity marker
      sadv : Str -- sentence adverb¡
    } ;
  
  Clause = {s,o : AdvPos => Str ; v : Tense => Anteriority => VQForm => AdvPos => Str ; neg : Polarity => AdvPos => Str ; adv : Str } ;
  QClause = {s : C.Tense => Anteriority => C.Pol => QForm => Str} ;

  mkClause : NounPhrase -> VerbPhrase -> Clause = \np,vp ->
    let
      -- combines adverbs from noun phrase and verb phrase
      adv  = np.adv ++ vp.adv ;
      -- helper functions to either place the adverb in the designated position
      -- or an empty string instead
      pres   : AdvPos -> Str = \ap -> case ap of { PreS => adv ; _ => [] } ;
      prev   : AdvPos -> Str = \ap -> case ap of { PreV => adv ; _ => [] } ;
      preo   : AdvPos -> Str = \ap -> case ap of { PreO => adv ; _ => [] } ;
      preneg : AdvPos -> Str = \ap -> case ap of { PreNeg => adv ; _ => [] } ;
      ins    : AdvPos -> Str = \ap -> case ap of { InS  => adv ; _ => [] } ;
      inv    : AdvPos -> Str = \ap -> case ap of { InV  => adv ; _ => [] }
    in
    {
      -- subject part of the clause:
      -- ap is the adverb position in the clause
      s = \\ap =>
	pres ap ++                           -- adverbs can be placed in the beginning of the clause
	np.det.s ! np.g ! Nom ++             -- the determiner, if any
	np.preap.s ! (Ag np.g np.n Nom) ++   -- adjectives which come before the subject noun, agreeing with it
	ins ap ++                            -- adverbs can be placed within the subject noun phrase
	np.s ! Nom ++                        -- the noun of the subject noun phrase in nominative
	np.postap .s ! (Ag np.g np.n Nom) ++ -- adjectives which come after the subject noun, agreeing with it
	np.det.sp ! np.g ! Nom ;             -- second part of split determiners
      -- verb part of the clause:
      -- tense and anter(ority) for the verb tense
      -- vqf is the VQForm parameter which defines if the ordinary verbform or the quistion form with suffix "-ne" will be used
      -- ap is the adverb position in the clause
      v = \\tense,anter,vqf,ap =>
	prev ap ++                           -- adverbs can be placed in the before the verb phrase
	vp.compl ! Ag np.g np.n Nom ++       -- verb phrase complement, e.g. predicative expression, agreeing with the subject 
	inv ap ++                            -- adverbs can be placed within the verb phrase
	-- verb form with conversion between different forms of tense and aspect
	vp.s ! VAct ( anteriorityToVAnter anter ) ( tenseToVTense tense ) np.n np.p ! vqf ;
      -- object part of the clause
      o = \\ap => preo ap ++ vp.obj ;
      -- optional negation particle, adverbs can be placed before the negation
      neg = \\pol,ap => preneg ap ++ negation pol ;
      adv = ""
    } ;
  
  combineClause : Clause -> C.Tense -> Anteriority -> C.Pol -> VQForm -> Sentence = \cl,tense,anter,pol,vqf ->
    { s = cl.s ;
      o =  cl.o ;
      v =  cl.v ! tense.t ! anter ! vqf ;
      neg = cl.neg ! pol.p ;
      sadv = "" ;
      t = tense ;
      p = pol
    } ;

  combineSentence : Sentence -> ( SAdvPos => AdvPos => Order => Str ) = \s ->
    let
      pres   : SAdvPos -> Str = \ap -> case ap of { SPreS =>    s.sadv ; _ => [] } ;
      prev   : SAdvPos -> Str = \ap -> case ap of { SPreV =>    s.sadv ; _ => [] } ;
      preo   : SAdvPos -> Str = \ap -> case ap of { SPreO =>    s.sadv ; _ => [] } ;
      preneg : SAdvPos -> Str = \ap -> case ap of { SPreNeg =>  s.sadv ; _ => [] } 
    in
    \\sap,ap,order  => case order of {
      SVO => s.t.s ++ s.p.s ++ pres sap ++ s.s   ! ap ++ preneg sap ++ s.neg ! ap ++ prev sap ++ s.v   ! ap ++ preo sap ++ s.o ! ap;
      VSO => s.t.s ++ s.p.s ++ preneg sap ++ s.neg ! ap ++ prev sap ++ s.v   ! ap ++ pres sap ++ s.s   ! ap ++ preo sap ++ s.o ! ap;
      VOS => s.t.s ++ s.p.s ++ preneg sap ++ s.neg ! ap ++ prev sap ++ s.v   ! ap ++ preo sap ++ s.o   ! ap ++ pres sap ++ s.s ! ap ;
      OSV => s.t.s ++ s.p.s ++ preo sap ++ s.o   ! ap ++ pres sap ++ s.s   ! ap ++ preneg sap ++ s.neg ! ap ++ prev sap ++ s.v ! ap;
      OVS => s.t.s ++ s.p.s ++ preo sap ++ s.o   ! ap ++ preneg sap ++ s.neg ! ap ++ prev sap ++ s.v   ! ap ++ pres sap ++ s.s ! ap ;
      SOV => s.t.s ++ s.p.s ++ pres sap ++ s.s   ! ap ++ preo sap ++ s.o   ! ap ++ preneg sap ++ s.neg ! ap ++ prev sap ++ s.v ! ap
      } ;


  
  -- questions
  mkQuestion : SS -> Clause -> QClause = \ss,cl -> {
     s = \\tense,anter,pol,form => case form of {
       QDir => ss.s ++ (combineSentence (combineClause cl tense anter pol VQFalse)) ! SPreS ! PreS ! OVS  ;
       QIndir => ss.s ++ (combineSentence (combineClause cl tense anter pol VQFalse)) ! SPreO ! PreO ! OSV
       }
    };
  
  negation : Polarity -> Str = \p -> case p of {
    Pos => [] ;   
    Neg => "non"
  } ;

-- determiners

  Determiner : Type = {
    s,sp : Gender => Case => Str ; -- sp for split determiners (not clear if really needed)
    n : Number
    } ;

  mkDeterminer : Adjective -> Number -> Determiner = \a,n ->
    {
      n = n ;
      s = \\g,c => a.s ! Posit ! Ag g n c ;
      sp = \\_,_ => ""
    } ;

  Quantifier : Type = {
    s,sp : Agr => Str ;
    } ;

  mkQuantifG : (_,_,_,_,_ : Str) -> (_,_,_,_ : Str) -> (_,_,_ : Str) -> 
    Gender => Case => Str = 
    \mn,ma,mg,md,mab, fno,fa,fg,fab, nn,ng,nab -> table {
      Masc  => pronForms mn  ma mg md mab ;
      Fem   => pronForms fno fa fg md fab ;
      Neutr => pronForms nn  nn ng md nab     
    } ;
      
  mkQuantifier : (sg,pl : Gender => Case => Str) -> Quantifier = \sg,pl ->
    let 
      ssp = 
	table {
	  Ag g Sg c => sg ! g ! c ; 
	  Ag g Pl c => pl ! g ! c
	}
    in 
    {
      s  = ssp ;
      sp = ssp 
    } ;

  hic_Quantifier = mkQuantifier
    (mkQuantifG 
       "hic" "hunc" "huius" "huic" "hoc"  "haec" "hanc" "huius" "hac"  "hoc" "huius" "hoc")
    (mkQuantifG 
       "hi" "hos" "horum" "his" "his"  "hae" "has" "harum" "his"  "haec" "horum" "his")
    ;

  ille_Quantifier = mkQuantifier
    (mkQuantifG 
       "ille" "illum" "illius" "illi" "illo"  
       "illa" "illam" "illius" "illa"  
       "illud" "illius" "illo")
    (mkQuantifG 
       "illi" "illos" "illorum" "illis" "illis"  
       "illae" "illas" "illarum" "illis"  
       "illa" "illorum" "illis")
    ;

  mkPreposition : Str -> Case -> Preposition  = \s,c ->  {s = s ; c = c; isPost = False} ;

  mkPostposition : Str -> Case -> Preposition = \s,c ->  {s = s ; c = c ; isPost = True } ;

  -- adverbs
  Adverb : Type = { s : Degree => Str} ;
  mkAdverb : Str -> Adverb = \adv ->
    { s = table { Posit => adv ; _ => nonExist } } ;

  mkFullAdverb : (pos,comp,sup : Str) -> Adverb = \p,c,s ->
    { s = table { Posit => p ; Compar => c ; Super => s } };
  -- numerals
  param
    CardOrd = NCard | NOrd ;
    Unit = one | ten | hundred | thousand | ten_thousand | hundred_thousand ;
  oper
    Cardinal : Type = {s : Gender => Case => Str ; n : Number};
    Ordinal : Type = { s : Gender => Number => Case => Str } ;
    Numeral : Type = { card : Cardinal ; ord : Ordinal } ;

    mkNumeral : Str -> Str -> Numeral = \c,o -> -- cardinal and ordinal form
      let
	cardFlex : Gender => Case => Str = case c of { "unus" => \\gen,cas => case <gen,cas> of {
				    <Masc, Nom | Voc> => "unus" ; <Masc, Acc> => "unum" ; <Masc, Abl> => "uno" ;
				    <Fem, Nom | Abl | Voc> => "una" ; <Fem, Acc> => "unam" ;
				    <Neutr, Nom | Acc | Voc> => "unum" ; <Neutr, Abl> => "uno" ;
				    <_, Gen> => "unius" ; <_, Dat> => "uni"
				    } ;
				  "duo" => table {
				    Masc | Neutr => table Case [ "duo" ; "duo" ; "duorum" ; "duobus" ; "duobus" ; "duo" ] ;
				    Fem => table Case [ "duae" ; "duas" ; "duarum" ; "duabus" ; "duabus" ; "duae" ] } ;
				  "tres" => \\gen,cas => case <gen,cas> of {
				    <Neutr, Nom | Acc | Voc > => "tria" ; <_, Nom | Acc | Voc > => "tres" ;
				    <_, Gen> => "trium" ; <_, Dat | Abl > => "tribus"
				    } ;
				  "milia" => table {
				    Neutr => table Case [ "milia" ; "milia" ; "milium" ; "milibus" ; "milibus" ; "milia" ] ;
				    _ => \\_ => nonExist
				    } ;
				    _ => \\_,_ => c
	  } ;
	ordFlex : Gender => Number => Case => Str =
	  case o of {
	    stem + "us" => table {
	      Masc => table Number [ table Case [ stem + "us" ; stem + "um" ; stem + "i" ; stem + "o" ; stem + "o" ; stem + "e" ] ;
				     table Case [ stem + "i" ; stem + "os" ; stem + "orum" ; stem + "is" ; stem + "is" ; stem + "i" ] ;
		];
	      Fem => table Number [ table Case [ stem + "a" ; stem + "am" ; stem + "ae" ; stem + "ae" ; stem + "a" ; stem + "a" ] ;
				    table Case [ stem + "ae" ; stem + "as" ; stem + "arum" ; stem + "is" ; stem + "is" ; stem + "ae" ] ;
		] ;
	      Neutr => table Number [ table Case [ stem + "um" ; stem + "um" ; stem + "i" ; stem + "o" ; stem + "o" ; stem + "um" ] ;
				      table Case [ stem + "a" ; stem + "a" ; stem + "orum" ; stem + "is" ; stem + "is" ; stem + "a" ] ;
		]
	      } ;
	    _ => error "unsupported ordinal form"
	  }
      in
      { card = { s = cardFlex ; n = case c of { "unus" => Sg ; _ => Pl }  } ; ord = { s = ordFlex } } ;
}
