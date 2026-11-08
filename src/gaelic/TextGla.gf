concrete TextGla of Text = CommonGla ** open Prelude in {
  lin
    TEmpty = {s = []} ;
    TFullStop x xs = {s = x.s ++ SOFT_BIND ++ "." ++ xs.s} ;
    TQuestMark x xs = {s = x.s ++ SOFT_BIND ++ "?" ++ xs.s} ;
    TExclMark x xs = {s = x.s ++ SOFT_BIND ++ "!" ++ xs.s} ;
}
