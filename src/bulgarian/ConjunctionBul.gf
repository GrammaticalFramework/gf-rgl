--# -coding=utf8
concrete ConjunctionBul of Conjunction = 
  CatBul ** open ResBul, Coordination, Prelude, Predef in {
  flags coding=utf8 ;


  flags optimize=all_subs ;

  lin
    ConjS conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      } ;

    ConjAdv conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      } ;

    ConjAdV conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      p = Pos
      } ;

    ConjIAdv conj ss = {
      s = \\qform => linCoord []!conj.sep ++ ss.s!qform!conj.sep ++ conj.s ++ ss.s!qform!4;
      } ;

    ConjNP conj ss = {
      s  = \\role => linCoord []!conj.sep ++ ss.s!role!conj.sep ++ conj.s ++ ss.s!role!4;
      gn = conjGenNum (gennum (AMasc NonHuman) conj.n) ss.gn;
      p  = ss.p
      } ;

    ConjAP conj ss = {
      s     = \\aform,p => linCoord []!conj.sep ++ ss.s!aform!p!conj.sep ++ conj.s ++ ss.s!aform!p!4;
      adv   =              ss.adv!conj.conj ++ conj.s ++ ss.adv!4;
      isPre = ss.isPre
      } ;

    ConjRS conj ss = {
      s = \\role => linCoord []!conj.sep ++ ss.s!role!conj.sep ++ conj.s ++ ss.s!role!4;
      } ;

    ConjCN conj ss = {
      s = \\nform => linCoord []!conj.sep ++ ss.s!nform!conj.sep ++ conj.s ++ ss.s!nform!4;
      g = ss.g
      } ;

-- These fun's are generated from the list cat's.
    BaseS x y  = {s  = table {4 => y.s;    _ => x.s}} ; 
    ConsS x xs = {s  = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;

    BaseAdv x y  = {s  = table {4 => y.s; _ => x.s}} ; 
    ConsAdv x xs = {s  = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;

    BaseAdV x y  = {s  = table {4 => y.s; _ => x.s}} ; 
    ConsAdV x xs = {s  = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;

    BaseIAdv x y  = {s  = \\qform=>table {4 => y.s!qform; _ => x.s!qform}} ; 
    ConsIAdv x xs = {s  = \\qform=>table {4 => xs.s!qform!4; t => x.s!qform++linCoord bindComma!t++xs.s!qform!t}} ;

    BaseNP x y =
      {s  = \\role=>table {4 => y.s!role; _ => x.s!role};
       gn = conjGenNum x.gn y.gn;
       p  = x.p} ;
    ConsNP x xs =
      {s  = \\role=>table {4 => xs.s!role!4; t => x.s!role++linCoord bindComma!t++xs.s!role!t};
       gn = conjGenNum xs.gn x.gn;
       p  = x.p} ;

    BaseAP x y =
      {s  = \\aform,p => table {4 => y.s!aform!p; _ => x.s!aform!p} ;
       isPre = andB x.isPre y.isPre} ; 
    ConsAP x xs =
      {s  = \\aform,p=>table {4 => xs.s!aform!p!4; t => x.s!aform!p++linCoord bindComma!t++xs.s!aform!p!t};
       isPre = andB x.isPre xs.isPre} ;

    BaseRS x y =
      {s = \\role=>table {4 => y.s!role; _ => x.s!role}} ;
    ConsRS x xs =
      {s = \\role=>table {4 => xs.s!role!4; t => x.s!role++linCoord bindComma!t++xs.s!role!t}} ;

    BaseCN x y =
      {s = \\nform=>table {4 => y.s!nform; _ => x.s!nform};
       g = x.g} ;
    ConsCN x xs =
      {s = \\nform=>table {4 => xs.s!nform!4; t => x.s!nform++linCoord bindComma!t++xs.s!nform!t};
       g = x.g} ;

  lincat
    [S]    = {s : Ints 4 => Str} ;
    [Adv]  = {s : Ints 4 => Str} ;
    [AdV]  = {s : Ints 4 => Str} ;
    [IAdv] = {s : QForm =>           Ints 4 => Str} ;
    [NP]   = {s : Role  =>           Ints 4 => Str; gn : GenNum; p : PronPerson} ;
    [AP]   = {s : AForm => Person => Ints 4 => Str; isPre : Bool} ;
    [RS]   = {s : Agr   =>           Ints 4 => Str} ;
    [CN]   = {s : NForm =>           Ints 4 => Str; g : AGender} ;

}
