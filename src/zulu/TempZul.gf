concrete TempZul of TempAbs = CatZul ** open ResZul in {

  lin
    TPresTemp = { s = [] ; t = Absolute PresTense } ;
    TPerfTemp = { s = [] ; t = Absolute PerfTense } ;
    TPastTemp = { s = [] ; t = Absolute PastTense } ;
    TFutTemp = { s = [] ; t = Absolute FutTense } ;

    TPastPresTemp = { s = [] ; t = Relative PastTense PresTense } ;
    TFutPresTemp = { s = [] ; t = Relative FutTense PresTense } ;
    TPerfPerfTemp = { s = [] ; t = Relative PerfTense PerfTense } ;
    TFutPerfTemp = { s = [] ; t = Relative FutTense PerfTense } ;
    TPerfPresTemp = { s = [] ; t = Relative PerfTense PresTense } ;

}
