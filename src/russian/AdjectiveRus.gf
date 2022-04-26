concrete AdjectiveRus of Adjective = CatRus ** open ResRus, Prelude, Coordination in {
lin
  -- : A -> AP ;        -- warm - тёплый
  PositA a = adjFormsAdjective a ** {isPost = a.p} ;
  -- : A2 -> AP ;       -- married
  UseA2 a = adjFormsAdjective a ** {isPost = False} ;
  -- : A -> AP ;       -- warmer - теплее
  UseComparA a = adjFormsAdjective (immutableAdjForms a.comp)
                 ** {isPost = a.p; preferShort = PrefShort} ;  -- TODO: non-qual
  -- : AP -> Adv -> AP ; -- warm by nature
  AdvAP ap adv = ap ** {
    s = \\gn,a,c => adv.s ++ ap.s ! gn ! a ! c ;
    short=\\a => adv.s ++ ap.short ! a ;
    isPost = False
    } ;

  -- : AdA -> AP -> AP ;
  AdAP ada ap = ap ** {
    s=\\gn,anim,c => ada.s ++ ap.s ! gn ! anim ! c ;
    short=\\a => ada.s ++ ap.short ! a
    } ;
  -- : CAdv -> AP -> NP -> AP ; -- as cool as John
  CAdvAP cadv ap np = ap ** {
    s = \\gn,anim,c => cadv.s ++ ap.s ! gn ! anim ! c ++ embedInCommas (cadv.p ++ np.s ! Nom) ;
    short = \\a => cadv.s ++ ap.short ! a ++ embedInCommas (cadv.p ++ np.s ! Nom)
    } ;

  -- : AP -> SC -> AP ;  -- good that she is here
  SentAP ap sc = ap ** {
    s = \\gn,anim,c => ap.s ! gn ! anim ! c ++ sc.s ;
    short = \\a => ap.short ! a ++ sc.s ;
    preferShort = PrefShort ;
    isPost = True
    } ;

  -- : A -> NP -> AP ;  -- warmer than I - теплее меня
  ComparA a np = {
    s = \\gn,anim,cas => a.comp ++ (applyPrep {s="" ; c=Gen ; neggen=True ; hasPrep=False} np) ; -- True?
    short = \\ag=>a.comp ++ (applyPrep {s="" ; c=Gen ; neggen=True ; hasPrep=False} np) ;
    isPost = False ;
    preferShort = PrefShort
    } ;

  -- : Ord -> AP ;       -- warmest
  AdjOrd ord = adjFormsAdjective ord ** {isPost = False; preferShort = PreferFull} ;

  -- : A2 -> NP -> AP ;  -- married to him - замужем за ним (NB: gender change requires different word!)
  ComplA2 a2 np = let af=adjFormsAdjective a2 in  {
    s = \\gn,anim,cas => af.s ! gn ! anim ! (a2.c.c) ++ applyPrep a2.c np ;
    short = \\a=>af.short ! a ++ applyPrep a2.c np ;
    isPost = False ;
    preferShort = a2.preferShort
    } ;

  -- : A2 -> AP ;        -- married to itself - замужем за собой
  ReflA2 a2 = let af=adjFormsAdjective a2 in {
    s = \\gn,anim,cas => af.s ! gn ! anim ! cas ++ a2.c.s ++ sebya.s ! (a2.c.c) ;
    short = \\a=>af.short ! a ++ a2.c.s ++ sebya.s ! (a2.c.c) ;
    isPost = False ;
    preferShort = a2.preferShort
    } ;
}