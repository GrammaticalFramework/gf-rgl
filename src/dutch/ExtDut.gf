--concrete ExtDut of ExtDutAbs = CatDut ** open ResDut in 
{
--{
----
----  lincat
----
----    Aux = {s : Polarity => Str} ;
----    
----  lin
---- 
----    PredAux np aux vp = mkClause (np.s ! Nom) np.a {
----      s = \\t,ant,b,ord,agr => 
----        let 
----          fin  = aux.s ! b ;
----          vf : Str -> Str -> {fin, inf : Str} = \x,y -> 
----            {fin = x ; inf = y} ;
----        in
----        case ant of {
----          Simul => vf fin [] ;
----          Anter => vf fin "have"
----          } ;
----      s2 = \\agr => infVP vp agr
----      } ;
----
----    can_Aux  = {s = \\p => posneg p "can"} ; ---- cannt
----    must_Aux = {s = \\p => posneg p "must"} ;
----
----}

}
