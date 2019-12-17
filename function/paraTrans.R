
##paramter transfromation
paraTrans <- function(ui.para){
  if(ui.para == "uaTtest.two.sided") return("two.sided")
  if(ui.para == "uaTtest.less") return("less")
  if(ui.para == "uaTtest.greater") return("greater")

  if(ui.para == "uaWilcox.two.sided") return("two.sided")
  if(ui.para == "uaWilcox.less") return("less")
  if(ui.para == "uaWilcox.greater") return("greater")

  if(ui.para == "uaTtestFDR") return("fdr")
  if(ui.para == "uaTtestHolm") return("holm")
  if(ui.para == "uaTtesthochberg") return("hochberg")
  if(ui.para == "uaTtestHommel") return("hommel")
  if(ui.para == "uaTtestBonferroni") return("bonferroni")
  if(ui.para == "uaTtestBH") return("BH")
  if(ui.para == "uaTtestBY") return("BY")
  if(ui.para == "uaTtestNoCorrection") return("none")

  if(ui.para == "uaWilcoxFDR") return("fdr")
  if(ui.para == "uaWilcoxHolm") return("holm")
  if(ui.para == "uaWilcoxhochberg") return("hochberg")
  if(ui.para == "uaWilcoxHommel") return("hommel")
  if(ui.para == "uaWilcoxBonferroni") return("bonferroni")
  if(ui.para == "uaWilcoxBH") return("BH")
  if(ui.para == "uaWilcoxBY") return("BY")
  if(ui.para == "uaWilcoxNoCorrection") return("none")

  if(ui.para == "maHCARowEuclidean") return("euclidean")
  if(ui.para == "maHCARowMaximum") return("maximum")
  if(ui.para == "maHCARowManhattan") return("manhattan")
  if(ui.para == "maHCARowCanberra") return("canberra")
  if(ui.para == "maHCARowBinary") return("binary")
  if(ui.para == "maHCARowMinkowski") return("minkowski")

  if(ui.para == "maHCAColEuclidean") return("euclidean")
  if(ui.para == "maHCAColMaximum") return("maximum")
  if(ui.para == "maHCAColManhattan") return("manhattan")
  if(ui.para == "maHCAColCanberra") return("canberra")
  if(ui.para == "maHCAColBinary") return("binary")
  if(ui.para == "maHCAColMinkowski") return("minkowski")

  if(ui.para == "maHCAward.D") return("ward.D")
  if(ui.para == "maHCAward.D2") return("ward.D2")
  if(ui.para == "maHCAsingle") return("single")
  if(ui.para == "maHCAcomplete") return("complete")
  if(ui.para == "maHCAaverage") return("average")
  if(ui.para == "maHCAmcquitty") return("mcquitty")
  if(ui.para == "maHCAmedian") return("median")
  if(ui.para == "maHCAcentroid") return("centroid")

  if(ui.para == "mi.plus.h") return("M+H")
  if(ui.para == "mi.plus.k") return("M+K")
  if(ui.para == "mi.plus.na") return("M+Na")
  if(ui.para == "mi.plus.nh4") return("M+NH4")

  if(ui.para == "mi.minus.h") return("M-H")
  if(ui.para == "mi.plus.cl") return("M+Cl")
  if(ui.para == "mi.plus.ch3coo") return("M+CH3COO")


  if(ui.para == "ms2.pos.m.plus.h") return("M+H")
  if(ui.para == "ms2.pos.m.plus.k") return("M+K")
  if(ui.para == "ms2.pos.m.plus.na") return("M+Na")
  if(ui.para == "ms2.pos.m.plus.nh4") return("M+NH4")

  if(ui.para == "ms2.pos.m.minus.h") return("M-H")
  if(ui.para == "ms2.pos.m.plus.cl") return("M+Cl")
  if(ui.para == "ms2.pos.m.plus.ch3coo") return("M+CH3COO")


  if(ui.para == "M+H") return("(M+H)+")
  if(ui.para == "M+K") return("(M+K)+")
  if(ui.para == "M+Na") return("(M+Na)+")
  if(ui.para == "M+NH4") return("(M+NH4)+")

  if(ui.para == "M-H") return("(M-H)-")
  if(ui.para == "M+Cl") return("(M+Cl)-")
  if(ui.para == "M+CH3COO") return("(M+CH3COO)-")

  if(ui.para == "mi.30") return(30)
  if(ui.para == "mi.10") return(10)
  if(ui.para == "mi.20") return(20)
  if(ui.para == "mi.40") return(40)
  if(ui.para == "mi.50") return(50)

  if(ui.para == "ms2.30") return(30)
  if(ui.para == "ms2.10") return(10)
  if(ui.para == "ms2.20") return(20)
  if(ui.para == "ms2.40") return(40)
  if(ui.para == "ms2.50") return(50)


  ##one data cleaning
  if(ui.para == "Zero value") return("zero")
  if(ui.para == "Mean") return("mean")
  if(ui.para == "Median") return("median")
  if(ui.para == "Minimum") return("minimum")
  if(ui.para == "KNN") return("knn")
  if(ui.para == "missForest") return("rf")
  if(ui.para == "BPCA") return("bpca")
  if(ui.para == "PPCA") return("ppca")
  if(ui.para == "SVD") return("svd")

  if(ui.para == "QC LOESS") return("loess")
  if(ui.para == "QC SVR (MetNormalizer)") return("svr")
  if(ui.para == "None") return("no")
  if(ui.para == "Mean") return("mean")
  if(ui.para == "Median") return("median")
  if(ui.para == "Total") return("total")
  if(ui.para == "Probabilistic Quotient Normalization(PQN)") return("pqn")
  if(ui.para == "Quantile") return("quantile")

  if(ui.para == "QC mean") return("qc.mean")
  if(ui.para == "QC median") return("qc.median")
  if(ui.para == "Subject mean") return("subject.mean")
  if(ui.para == "Subject median") return("subject.median")
  if(ui.para == "None") return("none")

  if(ui.para == "No log") return("no")
  if(ui.para == "Log 2") return("log2")
  if(ui.para == "Log e") return("loge")
  if(ui.para == "Log 10") return("log10")

  if(ui.para == "Pareto scale") return("pareto")
  if(ui.para == "Auto scale") return("auto")
  if(ui.para == "No scale") return("no")

}
