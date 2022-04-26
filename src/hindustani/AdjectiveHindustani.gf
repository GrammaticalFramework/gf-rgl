incomplete concrete AdjectiveHindustani of Adjective = CatHindustani ** open CommonHindustani,ResHindustani, Prelude in {

  lin

    PositA a = a ;
	UseComparA a = a;

    ComparA a np = {
        s = \\n,g,c,d => np.s ! NPC Obl ++ sE ++ a.s ! n ! g ! c ! d ;                       
       } ;

---- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 a np = {
      s = \\n,g,c,d => np.s ! NPC Obl ++ a.c2 ++ a.s ! n ! g ! c ! d ; 
     } ;

    ReflA2 a = {
      s = \\n,g,c,d => a.s ! n ! g ! c ! d  ++  RefPron ++ sE ; 
      } ;

    SentAP ap sc = { 
      s = \\n,g,c,d => case sc.fromVP of {
                           True => sc.s ++ kw ++ ap.s ! n ! g ! c ! d  ; 
			   False => ap.s ! n ! g ! c ! d ++ sc.s } ;
      } ;

    AdAP ada ap = {
      s = \\n,g,c,d => ada.s ++ ap.s ! n ! g ! c ! d ;
      } ;

    UseA2 a = a ;
	
	CAdvAP  cadv ap np = {
	 s = \\n,g,c,d => cadv.s ++ ap.s ! n ! g ! c ! d ++ cadv.p ++  np.s ! NPC Dir ;
	 };
    
    AdjOrd ord =  { s = \\_,_,_,_ => ord.s ; };
    
    AdvAP ap adv = {
      s = \\n,g,c,d => adv.s ! g ++ ap.s ! n ! g ! c ! d ;
      } ;

}
