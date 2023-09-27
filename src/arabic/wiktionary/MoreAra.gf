resource MoreAra = CatAra ** open ParadigmsAra in {


-- temporarily moved from ParadigmsAra
-- paradigms for Wiktionary extraction
---- TODO: better usage of information in Wiktionary

oper
  wmkN = overload {
    wmkN : {sg, pl : Str ; g : Gender} -> N
      = \r -> mkN r.sg r.pl r.g nohum ;  --- hum/nohum not in Wikt
    wmkN : {sg : Str} -> N
      = \r -> smartN r.sg ; 
    wmkN : {sg : Str ; g : Gender ; root : Str} -> N
      = \r -> smartN r.sg ** {g = r.g} ; ----
    wmkN : {sg : Str; g : Gender} -> N
      = \r -> smartN r.sg ** {g = r.g} ;
    wmkN : {sg : Str; pl : Str; g : Gender; root : Str} -> N
      = \r -> mkN r.sg r.pl r.g nohum ;   --- hum/nohum not in Wikt
    wmkN : {sg : Str; pl : Str} -> N
      = \r -> mkN r.sg r.pl masc nohum ; ---- ** {g = (smartN r.sg).g} ;
    wmkN : {sg, pl : Str ; root : Str} -> N
      = \r -> mkN r.sg r.pl masc nohum ;  ---- 
    wmkN : {sg : Str; root : Str} -> N 
      = \r -> smartN r.sg ;
    } ;

  wmkA = overload {
    wmkA : {root : Str} -> A
      = \r -> mkA r.root ;
    wmkA : {masc_sg : Str; fem_pl : Str; root : Str} -> A
      = \r -> mkA r.root ;
    wmkA : {masc_sg : Str; fem_sg : Str; fem_pl : Str; root : Str} -> A
      = \r -> mkA r.root ;
    wmkA : {masc_sg, fem_sg, masc_pl, fem_pl, root, sg_patt, pl_patt : Str} -> A
      = \r -> mkA r.root r.sg_patt r.pl_patt ;
    wmkA : {masc_sg, fem_sg, masc_pl, root, sg_patt, pl_patt : Str} -> A
      = \r -> mkA r.root r.sg_patt r.pl_patt ;
    wmkA : {fem_pl : Str; fem_sg : Str; masc_sg : Str; root : Str; sg_patt : Str} -> A
      = \r -> mkA r.root r.sg_patt ;
    wmkA : {fem_pl : Str; fem_sg : Str; masc_sg, masc_pl, root, sg_patt : Str} -> A
      = \r -> mkA r.root r.sg_patt ;
    wmkA : {masc_sg, root, sg_patt : Str} -> A
      = \r -> mkA r.root r.sg_patt ;
    wmkA : {masc_sg, masc_pl, root, sg_patt : Str} -> A
      = \r -> mkA r.root r.sg_patt ;
    wmkA : {masc_sg, fem_sg, masc_pl, fem_pl, root, pl_patt : Str} -> A
      = \r -> mkA r.root ; ----
    wmkA : {masc_sg, fem_sg, masc_pl, fem_pl, root : Str} -> A
      = \r -> mkA r.root ; ----
    wmkA : {masc_sg, fem_sg, root : Str} -> A
      = \r -> mkA r.root ; ----
    wmkA : {masc_sg, fem_sg, masc_pl, fem_pl, pl_patt : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    wmkA : {masc_sg : Str; fem_sg : Str; fem_pl : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    wmkA : {masc_sg : Str; fem_sg : Str; root : Str ; sg_patt : Str} -> A
      = \r -> mkA r.root r.sg_patt ;
    wmkA : {masc_sg : Str; fem_sg : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    wmkA : {masc_sg : Str; masc_pl : Str; fem_sg : Str; fem_pl : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    wmkA : {masc_sg : Str; masc_pl : Str; fem_sg : Str; root : Str} -> A
      = \r -> mkA r.root ;
    wmkA : {masc_sg : Str; masc_pl : Str; fem_sg : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    wmkA : {masc_sg : Str; masc_pl : Str; root : Str} -> A
      = \r -> mkA r.root ;
    wmkA : {masc_sg : Str; masc_pl, pl_patt : Str; root : Str} -> A
      = \r -> mkA r.root ;
    wmkA : {masc_sg : Str; masc_pl, pl_patt, sg_patt : Str; root : Str} -> A
      = \r -> mkA r.sg_patt r.pl_patt ;
    wmkA : {masc_sg : Str; masc_pl : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    wmkA : {masc_sg : Str; masc_pl, pl_patt : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    wmkA : {masc_sg : Str; root : Str} -> A
      = \r -> mkA r.root ;
    wmkA : {masc_sg : Str} -> A
      = \r -> mkA r.masc_sg ; ----
    } ;

  wmkV = overload {
    wmkV : {perfect : Str; cls : VerbForm; root : Str} -> V
      = \r -> mkV r.root r.cls ; ----
    wmkV : {perfect : Str; cls : VerbForm} -> V
      = \r -> mkV r.perfect r.cls ; ---- expects root
    wmkV : {perfect : Str; imperfect : Str; cls : VerbForm; root : Str} -> V
      = \r -> mkV r.root r.cls ; ----
    wmkV : {perfect : Str; imperfect : Str; cls : VerbForm} -> V
      = \r -> mkV r.perfect r.cls ; ---- expects root
    wmkV : {root : Str ; cls : VerbForm} -> V
      = \r -> mkV r.root r.cls ;
    wmkV : {imperfect : Str} -> V
      = \r -> variants {} ; ---- mkV r.imperfect ; -- expects cls I
    } ;

}