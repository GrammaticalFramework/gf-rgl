concrete ConjunctionTur of Conjunction =
  CatTur ** open ResTur, HarmonyTur, Coordination, Prelude, Predef in {

  lin
    ConjS conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      subord = linCoord []!conj.sep ++ ss.subord!conj.sep ++ conj.s ++ ss.subord!4;
      } ;

    ConjNP conj ss = {
      s = \\c => linCoord []!conj.sep ++ ss.s!c!conj.sep ++ conj.s ++ ss.s!c!4;
      h = ss.h ;
      a = conjAgr {n=conj.n; p=P3} ss.a
      } ;

    BaseS x y  = {s      = table {4 => y.s;      _ => x.s};
                  subord = table {4 => y.subord; _ => x.subord};
                 } ;
    ConsS x xs = {s      = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t};
                  subord = table {4 => xs.subord!4; t => x.subord++linCoord bindComma!t++xs.subord!t} ;
                 } ;

    BaseNP x y =
      {s = \\c=>table {4 => y.s!c; _ => x.s!c};
       a = conjAgr x.a y.a;
       h = y.h} ;
    ConsNP x xs =
      {s = \\c=>table {4 => xs.s!c!4; t => x.s!c++linCoord bindComma!t++xs.s!c!t};
       a = conjAgr xs.a x.a;
       h = xs.h} ;

    ConjAP conj ss = {
      s = \\n,c => linCoord []!conj.sep ++ ss.s!n!c!conj.sep ++ conj.s ++ ss.s!n!c!4;
      h = ss.h
      } ;

    BaseAP x y =
      {s = \\n,c=>table {4 => y.s!n!c; _ => x.s!n!c};
       h = y.h
      } ;
    ConsAP x xs =
      {s = \\n,c=>table {4 => xs.s!n!c!4; t => x.s!n!c++linCoord bindComma!t++xs.s!n!c!t};
       h = xs.h;
      } ;

    ConjCN conj ss = {
      s   = \\n,c => linCoord []!conj.sep ++ ss.s!n!c!conj.sep ++ conj.s ++ ss.s!n!c!4;
      gen = \\n,a => linCoord []!conj.sep ++ ss.gen!n!a!conj.sep ++ conj.s ++ ss.gen!n!a!4;
      h   = ss.h
      } ;

    BaseCN x y =
      {s   = \\n,c=>table {4 => y.s!n!c; _ => x.s!n!c};
       gen = \\n,a=>table {4 => y.gen!n!a; _ => x.gen!n!a};
       h   = y.h} ;
    ConsCN x xs =
      {s   = \\n,c=>table {4 => xs.s!n!c!4; t => x.s!n!c++linCoord bindComma!t++xs.s!n!c!t};
       gen = \\n,a=>table {4 => xs.gen!n!a!4; t => x.gen!n!a++linCoord bindComma!t++xs.gen!n!a!t};
       h   = xs.h} ;
 
    ConjAdV conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      h = ss.h
      } ;

    BaseAdV x y =
      {s = table {4 => y.s; _ => x.s};
       h = y.h} ;
    ConsAdV x xs =
      {s = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t};
       h = xs.h} ;


    ConjAdv conj ss = {
      s = linCoord []!conj.sep ++ ss.s!conj.sep ++ conj.s ++ ss.s!4;
      h = ss.h
      } ;

    BaseAdv x y =
      {s = table {4 => y.s; _ => x.s};
       h = y.h} ;
    ConsAdv x xs =
      {s = table {4 => xs.s!4; t => x.s++linCoord bindComma!t++xs.s!t};
       h = xs.h} ;


    ConjRS _ _ = variants {} ;
    ConsRS _ _ = variants {} ;
    BaseRS _ _ = variants {} ;

  lincat
    [S]   = {s,subord : Ints 4 => Str} ;
    [Adv] = {s : Ints 4 => Str} ;
    [AdV] = {s : Ints 4 => Str} ;
    [NP]  = {s : Case => Ints 4 => Str; h : Harmony; a : Agr} ;
    [AP]  = {s : Number => Case => Ints 4 => Str; h : Harmony} ;
    [CN]  = {s   : Number => Case => Ints 4 => Str;
             gen : Number => Agr  => Ints 4 => Str;
             h   : Harmony} ;

}
