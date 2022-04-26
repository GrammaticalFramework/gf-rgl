concrete AdjectiveLat of Adjective = CatLat ** open ResLat, Prelude in {


  lin
--  PositA : A -> AP ; -- warm
    PositA  a = 
      { s = \\ag  => a.s ! Posit ! ag };

--  ComparA : A -> NP -> AP ; -- warmer than I
    ComparA a np = {
      s = \\ag => a.s ! Compar ! ag ++ "quam" ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! Nom ; 
      } ;

-- ComplA2 : A2 -> NP -> AP ; -- married to her
    ComplA2 a np = {
      s = \\ag => a.s ! Posit ! ag ++ a.c.s ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! a.c.c ; 
      } ;

--  ReflA2 : A2 -> AP -- married to myself
    ReflA2 a = {
      s = \\ag => a.s ! Posit ! ag ++ a.c.s ++
	table { Acc => "memet" ; _ => "egomet" } ! a.c.c;
      } ;

--  UseA2   : A2 -> AP ;        -- married
    UseA2 = PositA ;

--  UseComparA : A  -> AP ;     -- warmer
    UseComparA a =
      { s = \\ag  => a.s ! Compar ! ag };
    
--  CAdvAP  : CAdv -> AP -> NP -> AP ; -- as cool as John
    CAdvAP cadv ap np =
      { s = \\ag => cadv.s ++ ap.s ! ag ++ cadv.p ++ (combineNounPhrase np) ! PronNonDrop ! APostN ! DPreN ! Nom } ;
    
-- The superlative use is covered in $Ord$.

--  AdjOrd  : Ord -> AP ;       -- warmest
    AdjOrd a = { s = table { Ag g n c => a.s ! g ! n ! c } } ;
    
--  SentAP : AP -> SC -> AP ; -- good that she is here
    SentAP ap sc =
      { s = \\a => ap.s ! a ++ sc.s } ;

    
--  AdAP    : AdA -> AP -> AP ; -- very warm
    AdAP ada ap =
      { s = \\agr => ada.s ++ ap.s ! agr } ;

--  AdvAP   : AP -> Adv -> AP ; -- warm by nature
    AdvAP ap adv =
      { s = \\ag => ap.s ! ag ++ adv.s ! Posit} ;
}
