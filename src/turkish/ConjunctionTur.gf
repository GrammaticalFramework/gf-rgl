concrete ConjunctionTur of Conjunction =
  CatTur ** open ResTur, Coordination, Prelude, Predef in {

  lin
    ConjS _ _ = variants {} ;

    ConjNP conj ss = {
      s = \\c => linCoord []!conj.sep ++ ss.s!c!conj.sep ++ conj.s ++ ss.s!c!4;
      a = conjAgr {n=conj.n; p=P3} ss.a
      } ;

    BaseNP x y =
      {s = \\c=>table {4 => y.s!c; _ => x.s!c};
       a = conjAgr x.a y.a} ;
    ConsNP x xs =
      {s = \\c=>table {4 => xs.s!c!4; t => x.s!c++linCoord bindComma!t++xs.s!c!t};
       a = conjAgr xs.a x.a} ;

    ConjAP conj ss = {
      s = \\n,c => linCoord []!conj.sep ++ ss.s!n!c!conj.sep ++ conj.s ++ ss.s!n!c!4
      } ;

    BaseAP x y =
      {s = \\n,c=>table {4 => y.s!n!c; _ => x.s!n!c}} ;
    ConsAP x xs =
      {s = \\n,c=>table {4 => xs.s!n!c!4; t => x.s!n!c++linCoord bindComma!t++xs.s!n!c!t}} ;

    ConjCN conj ss = {
      s   = \\n,c => linCoord []!conj.sep ++ ss.s!n!c!conj.sep ++ conj.s ++ ss.s!n!c!4;
      gen = \\n,a => linCoord []!conj.sep ++ ss.gen!n!a!conj.sep ++ conj.s ++ ss.gen!n!a!4
      } ;

    BaseCN x y =
      {s   = \\n,c=>table {4 => y.s!n!c; _ => x.s!n!c};
       gen = \\n,a=>table {4 => y.gen!n!a; _ => x.gen!n!a}} ;
    ConsCN x xs =
      {s   = \\n,c=>table {4 => xs.s!n!c!4; t => x.s!n!c++linCoord bindComma!t++xs.s!n!c!t};
       gen = \\n,a=>table {4 => xs.gen!n!a!4; t => x.gen!n!a++linCoord bindComma!t++xs.gen!n!a!t}} ;
 
    ConjAdV conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4
      } ;

    BaseAdV x y =
      {s = table {4 => y.s; _ => x.s}} ;
    ConsAdV x xs =
      {s = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;


    ConjAdv conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4
      } ;

    BaseAdv x y =
      {s = table {4 => y.s; _ => x.s}} ;
    ConsAdv x xs =
      {s = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t}} ;


    ConjRS _ _ = variants {} ;
    ConsRS _ _ = variants {} ;
    BaseRS _ _ = variants {} ;

    ConsS _ _ = variants {} ;
    BaseS _ _ = variants {} ;

  lincat
    [Adv] = {s : Ints 4 => Str} ;
    [AdV] = {s : Ints 4 => Str} ;
    [NP]  = {s : Case => Ints 4 => Str; a : Agr} ;
    [AP]  = {s : Number => Case => Ints 4 => Str} ;
    [CN]  = {s   : Number => Case => Ints 4 => Str;
             gen : Number => Agr  => Ints 4 => Str} ;

}
