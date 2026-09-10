--# -coding=utf8
concrete ConjunctionMkd of Conjunction = 
  CatMkd ** open ResMkd, Coordination, Prelude, Predef in {

  lin
    ConjS conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      } ;

    ConjAdv conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      } ;

    ConjAdV conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      } ;

    ConjIAdv conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      } ;

    ConjNP conj ss = {
      s  = \\role => linCoord []!conj.sep ++ ss.s!role!conj.sep ++ conj.s ++ ss.s!role!4;
      vocative = linCoord []!conj.sep ++ ss.vocative!conj.sep ++ conj.s ++ ss.vocative!4;
      a = {g = conjGenNum (genNum Masc conj.n) ss.gn; p  = ss.p}
      } ;

    ConjAP conj ss = {
      s     = \\sp,gn => linCoord []!conj.sep ++ ss.s!sp!gn!conj.sep ++ conj.s ++ ss.s!sp!gn!4;
      isPre = ss.isPre
      } ;

    ConjRS conj ss = {
      s = \\gn => linCoord []!conj.sep ++ ss.s!gn!conj.sep ++ conj.s ++ ss.s!gn!4;
      } ;

    ConjCN conj ss = {
      s = \\sp,n => linCoord []!conj.sep ++ ss.s!sp!n!conj.sep ++ conj.s ++ ss.s!sp!n!4;
      count_form = linCoord []!conj.sep ++ ss.count_form!conj.sep ++ conj.s ++ ss.s!Indef!Pl!4;
      vocative = \\n => linCoord []!conj.sep ++ ss.vocative!n!conj.sep ++ conj.s ++ ss.vocative!n!4;
      g = ss.g
      } ;

-- These fun's are generated from the list cat's.
    BaseS x y  = {s  = table {4 => y.s;    _ => x.s}} ; 
    ConsS x xs = {s  = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;

    BaseAdv x y  = {s  = table {4 => y.s; _ => x.s}} ; 
    ConsAdv x xs = {s  = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;

    BaseAdV x y  = {s  = table {4 => y.s; _ => x.s}} ; 
    ConsAdV x xs = {s  = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;

    BaseIAdv x y  = {s  = table {4 => y.s; _ => x.s}} ;
    ConsIAdv x xs = {s  = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;

    BaseNP x y =
      {s  = \\role=>table {4 => y.s!role; _ => x.s!role};
       vocative = table {4 => y.vocative; _ => x.vocative};
       gn = conjGenNum x.a.g y.a.g;
       p  = x.a.p} ;
    ConsNP x xs =
      {s  = \\role=>table {4 => xs.s!role!4; t => x.s!role++linCoord bindComma!t++xs.s!role!t};
       vocative = table {4 => xs.vocative!4; t => x.vocative++linCoord bindComma!t++xs.vocative!t};
       gn = conjGenNum xs.gn x.a.g;
       p  = x.a.p} ;

    BaseAP x y =
      {s  = \\sp,gn => table {4 => y.s!sp!gn; _ => x.s!sp!gn} ;
       isPre = andB x.isPre y.isPre} ;
    ConsAP x xs =
      {s  = \\sp,gn => table {4 => xs.s!sp!gn!4; t => x.s!sp!gn++linCoord bindComma!t++xs.s!sp!gn!t};
       isPre = andB x.isPre xs.isPre} ;

    BaseRS x y =
      {s = \\gn=>table {4 => y.s!gn; _ => x.s!gn}} ;
    ConsRS x xs =
      {s = \\gn=>table {4 => xs.s!gn!4; t => x.s!gn++linCoord bindComma!t++xs.s!gn!t}} ;

    BaseCN x y =
      {s = \\sp,n=>table {4 => y.s!sp!n; _ => x.s!sp!n};
       count_form = table {4 => y.count_form; t => x.count_form};
       vocative = \\n=>table {4 => y.vocative!n; t => x.vocative!n};
       g = x.g} ;
    ConsCN x xs =
      {s = \\sp,n=>table {4 => xs.s!sp!n!4; t => x.s!sp!n++linCoord bindComma!t++xs.s!sp!n!t};
       count_form = table {4 => xs.count_form!4; t => x.count_form++linCoord bindComma!t++xs.count_form!t};
       vocative = \\n=>table {4 => xs.vocative!n!4; t => x.vocative!n++linCoord bindComma!t++xs.vocative!n!t};
       g = x.g} ;

  lincat
    [S]    = {s : Ints 4 => Str} ;
    [Adv]  = {s : Ints 4 => Str} ;
    [AdV]  = {s : Ints 4 => Str} ;
    [IAdv] = {s : Ints 4 => Str} ;
    [NP]   = {s : Role  => Ints 4 => Str; vocative : Ints 4 => Str; gn : GenNum; p : Person} ;
    [AP]   = {s : Species => GenNum => Ints 4 => Str; isPre : Bool} ;
    [RS]   = {s : GenNum  =>           Ints 4 => Str} ;
    [CN]   = {s : Species => Number => Ints 4 => Str ;
              count_form : Ints 4 => Str ;
              vocative : Number => Ints 4 => Str ;
              g : Gender} ;

}
