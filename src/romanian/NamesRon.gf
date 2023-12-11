concrete NamesRon of Names = CatRon ** open ResRon, Prelude in {

lin GivenName, MaleSurname, FemaleSurname = \pn -> -- KA: guessed
      let
        g = pn.g ;
        n = pn.n ;
        ag = agrP3 g n ;
        hc = getClit pn.a 
      in {
        s = \\c =>  {comp = pn.s ! c ;
                      clit = \\cs => if_then_Str hc ((genCliticsCase ag c).s ! cs) [] } ;
                   
        a = ag;
        nForm = if_then_else NForm hc HasClit (HasRef False) ;
        isPronoun = False ; isPol = False;
        indForm = pn.s ! No
        } ;
        
lin FullName gn sn =  -- KA: guessed
      let 
        g = gn.g ;
        n = gn.n ;
        ag = agrP3 g n ;
        hc = getClit gn.a 
      in {
        s = \\c =>  {comp = gn.s ! No ++ sn.s ! c ;
                     clit = \\cs => if_then_Str hc ((genCliticsCase ag c).s ! cs) [] } ;
        a = ag;
        nForm = if_then_else NForm hc HasClit (HasRef False) ;
        isPronoun = False ; isPol = False;
        indForm = gn.s ! No ++ sn.s ! No
        } ;

lin UseLN pn = let 
        g = pn.g ;
        n = pn.n ;
        ag = agrP3 g n ;
        hc = getClit pn.a 
      in {
        s = \\c =>  {comp = pn.s ! c ;
                      clit = \\cs => if_then_Str hc ((genCliticsCase ag c).s ! cs) [] } ;
                   
        a = ag;
        nForm = if_then_else NForm hc HasClit (HasRef False) ;
        isPronoun = False ; isPol = False;
        indForm = pn.s ! No
        } ;

}
