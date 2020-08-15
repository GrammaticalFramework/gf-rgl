--# -path=.:../abstract:../../prelude:../common
instance ParadigmsKam of ParadigmsBantu = DiffKam, CommonBantu ** open  (Predef=Predef),Prelude,MorphoKam, CatKam in {

   

oper
  --Cgender : Type ; 
  mu_a   : Cgender ; --class gender 
  mu_mi  : Cgender ;
  i_ma   : Cgender ;
  ki_i   : Cgender ;
  ka_tu  : Cgender ;
  va_ku  : Cgender ;
  n_n    : Cgender ; 
  u_ma   : Cgender ;
  u_n    : Cgender ;
  ku_ma  : Cgender ;


  Cgender =  MorphoKam.Cgender ; 
  Number =  MorphoKam.Number ;
  Case =  MorphoKam.NPCase ;
  mu_a   = G1 ;-- class gender 1/2
  mu_mi  = G2 ; -- class gender 3/4
  i_ma   = G3 ; -- class gender 5/6
  ki_i   = G4 ; -- class gender 7/8
  ka_tu  = G5 ; -- class gender 12/13
  va_ku  = G6 ; -- class gender 16/17
  n_n    = G7 ; -- class gender 9/10
  u_ma   = G8;  -- class gender 11/6
  u_n    =G9 ; -- class gender 11/10
  ku_ma  = G10 ; -- class gender 15/6
 
   nominative = npNom ;
  locative = npLoc ;
 

  regN = MorphoKam.regN ; 
  iregN = MorphoKam.iregN ;

 

  mkPrepof : Number => Cgender => Str = 
    table Number { Sg => table {  
                                 G3| G7 => "ya" ; 
                                 G4 => "kya" ; 
                                 G5 => "ka" ; 
                                 G6 => "va" ;
                                  G10 => "kwa";
                                 _ => "wa" } ; 
                                 
                   Pl => table { G1|G3| G8 | G10 => "ma" ; 
                                 G4| G7 |G9 => "sya" ; 
                                 G2 => "ya" ; 
                                 G5 => "twa" ; 
                                 G6 => "kwa"} } ;
 mkA = overload {
    mkA : Str -> A = \a -> lin A (regA a |cregA a | sregA a );
    mkA : (fat,fatter : Str) -> A =\a,b -> lin A (iregA a b| regAdj a b );
    } ; 
                       
 mkV = overload {
    mkV : (cry : Str) -> V=\v-> lin V (regV v ) ; -- regular, incl. cry-cries, kiss-kisses etc
    --mkV : Str -> V -> V=\v -> lin V (regV v ) ;  -- fix compound, e.g. under+take
  }; 
  mkV2 = overload {
        mkV2  : Str -> V2 = \s -> dirV2 (regV s |iregV s) ;   
        mkV2  : V -> V2 = dirV2 ;
        mkV2  : V -> Prep -> V2 = prepV2  ;
  };


    verb2snoun : Verb ->  Cgender -> Noun = \v,g->    
    let wp = "mu" + init(v.s ! VGen) +"i" ;
        wpl = "a" + init(v.s ! VGen) +"i" in 
    iregN wp wpl g ;
 

  regA : Str -> A = \s -> lin A (MorphoKam.regA s) ;
   cregA : Str -> A = \s -> lin A (MorphoKam.cregA s) ;
   sregA : Str -> A = \s -> lin A (MorphoKam.sregA s) ;
   iregA : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoKam.iregA a b);
   regAdj : (fat,fatter : Str) -> A =\a,b -> lin A (MorphoKam.regAdj a b);
  

  iregV=MorphoKam.iregV ;
  regV=MorphoKam.regV ; 



} 
