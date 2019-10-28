--1 Differences between Bantu languages

interface DiffBantu = open CommonBantu, Prelude in {
  flags coding=utf8 ;

param 
   Agr =  Ag  Cgender Number Person ;
  PronForm= Pers | Poss Number  Cgender;
  DetForm = Sub | Obj  Cgender ;
  IMPForm = Com | Pol ;
  VForm ;
  DForm ;
  AForm;
  VExte;


oper 
--fixclass: NPCase ->  Cgender ->  Cgender;

   Cgender : PType ;
  firstGender :  Cgender ; -- G1
  secondGender :  Cgender ; -- G2
  Noun : Type  = {s : Number => Case => Str ; g :  Cgender};
  CNoun : Type = {s : Number => Case => Str ; g :  Cgender; s2 : Number => Str};
  AAgr : Type  = {g :  Cgender ; n : Number} ;
 

oper
   clitAgr : Agr -> {n : Number ; p : Person} = \a -> case a of {
    Ag _ n p => {n = n; p = p} 
    } ;
  complAgr : Agr -> {g :  Cgender ; n : Number} = \a -> case a of {
    Ag g n _ => {g = g ; n = n} 
    } ;
  predetAgr : Agr -> {g :  Cgender} = \a -> case a of {
    Ag g _ _ => {g = g} 
    } ;
  nounAgr : Agr -> {g :  Cgender ; n : Number ; p : Person} = \a -> case a of {
    Ag g n p => {g = g ; n = n  ; p = p} 
    } ; 

  detAgr : Agr -> {g :  Cgender ; p : Person} = \a -> case a of {
    Ag g _ p => {g = g; p = p} 
    } ;

  agrG1 : Number -> Person -> Agr = \n,p ->
    Ag firstGender n p ;
  dapagr :  Cgender -> Person -> Agr = \g,p ->
    Ag g Sg p ;
  agrP3 :  Cgender -> Number -> Agr = \g,n ->
    Ag g n P3 ;

  aagr :  Cgender -> Number -> AAgr = \g,n ->
    {g = g ; n = n} ;

  ---- Conjunction Agreements----

  conjAgr : Number -> Agr -> Agr -> Agr = \n,xa,ya -> 
    let 
      x = nounAgr xa ; y = nounAgr ya
    in 
    Ag (conjGender x.g y.g) (conjNumber (conjNumber x.n y.n) n) 
       (conjPPerson x.p y.p) ;

  conjGender :  Cgender ->  Cgender ->  Cgender ;



oper
  conjThan  : Str ; --one of them in bantu
  conjThat  : Str ;
  superVery : Str ; -- one of bantu
  such : Str;
  reflPron : Agr => Str ; -- second of bantu.


oper
  ProunSgprefix :  Cgender -> Str ; 
  ProunPlprefix :  Cgender -> Str ; 
  Cardoneprefix  :  Cgender ->  Str;
 Cardtwoprefix  :  Cgender ->  Str;
 Allpredetprefix :  Cgender ->  Str;
 PrefixPlNom :  Cgender ->  Str;
 mkprefix,Ordprefix :  Cgender ->  Str;
 Cardprefix :  Cgender ->  Str ;
 --Mostpredetprefix :  Cgender ->  Str;
 --Adjpprefix :  Cgender -> Number ->   Str;
 --VowelAdjprefix:  Cgender -> Number ->   Str;
 -- ConsonantAdjprefix:  Cgender -> Number ->   Str;
}

