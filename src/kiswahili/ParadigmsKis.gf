--# -path=.:../abstract:../../prelude:../common

instance ParadigmsKis of ParadigmsBantu = DiffKis, CommonBantu ** open  (Predef=Predef),Prelude,MorphoKis, CatKis in {

oper
  --Cgender : Type ; 
  a_wa   : Cgender ; --m-wa
  u_i    : Cgender ; --m-mi
  li_ya  : Cgender ; --ji-ma
  ki_vi  : Cgender ; --kivi
  i_zi   : Cgender ; -- nn
  u_zi   : Cgender ; --uu
  u_u    : Cgender ; --uu
  u_ya   : Cgender;
  ya_ya  : Cgender;
  i_i    : Cgender;
  ku_ku  : Cgender ; --uu
  pa_pa  : Cgender ; --uu
  mu_mu  : Cgender ; --uu

  
  Cgender =  MorphoKis.Cgender ; 
  Number =  MorphoKis.Number ;
  Case   =  MorphoKis.NPCase ;
   a_wa  = G1 ;--%
   u_i   = G2 ;
  li_ya  = G3 ;
  ki_vi  = G4 ;
  i_zi   = G5 ;
  u_zi   = G6 ;
  u_u    = G7 ;
  u_ya   = G8 ;
  ya_ya  = G9 ;
  i_i    =G10 ;
  ku_ku  = G11;
  pa_pa  =G12 ;
  mu_mu  =G13;
  
  nominative = npNom ;
  locative = npLoc ;

  --npNumber np = (nounAgr np.a).n ;


  regN = MorphoKis.regN ; 
  iregN = MorphoKis.iregN ;

  
  mkPrepof : Number => Cgender => Str = 
    table Number { Sg => table { G1 |G2|G6|G7 |G8 => "wa" ; 
                                 G3=> "la" ; 
                                 G4 => "cha" ; 
                                 G5 => "ya" ; 
                                 G11 => "pa";
                                 G12 => "kwa";
                                 G13 => "mwa";
                                  _ => ""} ; 
                                 
                   Pl => table { G1 => "wa" ; 
                                 G2|G3 |G8 |G9 |G10 => "ya" ; 
                                 G4 => "vya" ; 
                                 G5|G6 => "za" ; 
                                 _ => ""} } ;
  
                       
 



 eqNumber : Number -> Number -> Bool =  
     \n,m -> case n of { Sg => case m of { Sg => True ; _ => False } ;
                         Pl => case m of { Pl => True ; _ => False } } ;

    verb2snoun : Verb ->  Cgender -> Noun = \v,g->    
    let wp = "mu" + init(v.s ! VGen) +"ji" ;
        wpl = "wa" + init(v.s ! VGen) +"ji" in 
    iregN wp wpl g ;
 

 

 mkAV  v  = v ** { lock_AV = <>} ;
      mkAV  : A ->  AV ;
      AS, AV : Type = A ;

      mkAS  : A -> AS ; 
      mkAS  v = v ** {lock_AS = <>} ;
  
  
  iregA, regA : Str -> A = \s -> lin A (MorphoKis.regA s) ;
   cregA : Str -> A = \s -> lin A (MorphoKis.cregA s) ;
   --iregA : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoKis.iregA a b);
  mkA = overload {
    mkA : Str -> A = \a -> lin A (regA a |cregA a | iregA a);
   -- mkA : (fat,fatter : Str) -> A =\a,b -> lin A (iregA a b);
    } ;


  
  regV=MorphoKis.regV ;

 
mkV = overload {
    mkV : (cry : Str) -> V=\v-> lin V (regV v ) ; -- regular, incl. cry-cries, kiss-kisses etc
    --mkV : Str -> V -> V=\v -> lin V (regV v ) ;  -- fix compound, e.g. under+take
  }; 
mkV2 = overload {
        mkV2  : Str -> V2 = \s -> dirV2 (regV s) ;   
        mkV2  : V -> V2 = dirV2 ;
        mkV2  : V -> Prep -> V2 = prepV2  ;
  };


} 
