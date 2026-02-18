resource ResSco = ResEng - [auxBe,posneg] ** {

oper
  auxBe : Aux = {
    pres = \\b,a => case <b,a> of {
      <Pos,AgP1 Sg> => "am" ;
      <Neg,AgP1 Sg> => "amna" ;
      _ => agrVerb (posneg b "is")  (posneg b "are") a
      } ;
    contr = \\b,a => case <b,a> of {
      <Pos,AgP1 Sg> => cBind "m" ;
      <Neg,AgP1 Sg> => cBind "m not" ; --- am not I
      _ => agrVerb (posneg b (cBind "s"))  (posneg b (cBind "re")) a
      } ;
    past = \\b,a => case a of {          --# notpresent
      AgP1 Sg | AgP3Sg _ => posneg b "wis" ; --# notpresent
      _                  => posneg b "wir"   --# notpresent
      } ; --# notpresent
    inf  = "be" ;
    ppart = "been" ;
    prpart = "bein"
    } ;

  posneg : Polarity -> Str -> Str = \p,s -> case p of {
    Pos => s ;
    Neg => s + "na"
    } ;

}
