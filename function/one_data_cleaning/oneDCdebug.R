# ms1 <- readr::read_csv("ms1.peak.table.csv")
# ms2 <- readr::read_csv("ms2.csv")
# sample.info <- read.csv("sample.info.csv", stringsAsFactors = FALSE)
# idx <- which(ms2$nhits.reverse != 0 | ms2$nhits.forward != 0)
#
# ms2 <- ms2[idx,]
#
# write.csv(ms2, "ms2.csv", row.names = FALSE)
#
#
# temp.idx <- match(sample.info$sample.name, colnames(ms1))
#
# int <- apply(ms1[,temp.idx], 1, median)
# names(int) <- ms1$name
#
# int <- sort(int, decreasing = TRUE)
#
# name <- names(int)[1:3000]
#
# ms1 <- ms1[match(name, ms1$name),]
#
#
# write.csv(ms1, "ms1.peak.table.csv", row.names = FALSE)

setwd("D:/study/R/my git/metCube/version1.1/user_data/test/test/one_data_cleaning")
one.dc.param.table <- readxl::read_excel("D:/study/R/my git/metCube/version1.1/data/demo_data/parameter_table.xlsx", sheet = 1)
one.dc.param.table <- as.data.frame(one.dc.param.table)

oneDCparameter <- as.character(one.dc.param.table[,2])
names(oneDCparameter) <- as.character(one.dc.param.table[,1])
oneDCparameter <- oneDCparameter[!is.na(oneDCparameter)]
oneDCparameter <- sapply(oneDCparameter, list)
oneDCparameter[[2]] <- as.numeric(oneDCparameter[[2]])
oneDCparameter[[3]] <- as.numeric(oneDCparameter[[3]])
oneDCparameter[[4]] <- as.numeric(oneDCparameter[[4]])
oneDCparameter[[5]] <- paraTrans(ui.para = oneDCparameter[[5]])
oneDCparameter[[6]] <- as.numeric(oneDCparameter[[6]])
oneDCparameter[[7]] <- as.numeric(oneDCparameter[[7]])
oneDCparameter[[8]] <- as.numeric(oneDCparameter[[8]])
oneDCparameter[[9]] <- as.numeric(oneDCparameter[[9]])
oneDCparameter[[10]] <- as.logical(oneDCparameter[[10]])
oneDCparameter[[11]] <- as.numeric(oneDCparameter[[11]])
oneDCparameter[[12]] <- as.numeric(oneDCparameter[[12]])
oneDCparameter[[13]] <- as.character(oneDCparameter[[13]])
oneDCparameter[[14]] <- paraTrans(oneDCparameter[[14]])
oneDCparameter[[15]] <- as.logical(oneDCparameter[[15]])
oneDCparameter[[16]] <- as.logical(oneDCparameter[[16]])
oneDCparameter[[17]] <- as.numeric(strsplit(oneDCparameter[[17]], split = ",")[[1]])
# oneDCparameter[[17]] <- as.character(oneDCparameter[[17]])
oneDCparameter[[18]] <- as.numeric(oneDCparameter[[18]])
oneDCparameter[[19]] <- as.numeric(oneDCparameter[[19]])
# oneDCparameter[[20]] <- as.logical(oneDCparameter[[20]])
oneDCparameter[[21]] <- paraTrans(oneDCparameter[[21]])
oneDCparameter[[22]] <- paraTrans(oneDCparameter[[22]])
oneDCparameter[[23]] <- paraTrans(oneDCparameter[[23]])
oneDCparameter[[24]] <- as.logical(oneDCparameter[[24]])
oneDCparameter[[25]] <- as.numeric(oneDCparameter[[25]])
oneDCparameter[[26]] <- as.numeric(oneDCparameter[[26]])
oneDCparameter[[27]] <- as.logical(oneDCparameter[[27]])
oneDCparameter[[28]] <- as.logical(oneDCparameter[[28]])
##mz tolerance and rt tolerance for
oneDCparameter[[29]] <- as.numeric(oneDCparameter[[29]])
oneDCparameter[[30]] <- as.numeric(oneDCparameter[[30]])

one.user.path <- getwd()

temp.file <- dir(one.user.path)
temp.file <- setdiff(temp.file, "peak.table.after.batch.alignment.csv")

save(oneDCparameter, file = "oneDCparameter")
save(temp.file, file = "temp.file")

metcleaning.info <- try(expr = {MetCleaning::MetCleaning(
  data = grep("batch", x = temp.file, value = TRUE),
  sample.information = "sample.info.csv",
  polarity = oneDCparameter[[1]],
  hasIS = "no",
  hasQC = "yes",
  #batch alignment
  combine.mz.tol = oneDCparameter[[2]],
  combine.rt.tol = oneDCparameter[[3]],
  #MVFilter para
  mv.filter = TRUE,
  obs.mv.cutoff = 0,
  var.mv.cutoff = 1 - oneDCparameter[[3]]/100,
  #MVimputation
  imputation.method = oneDCparameter[[5]],
  k = oneDCparameter[[6]],
  rowmax = oneDCparameter[[7]],
  colmax = oneDCparameter[[8]],
  maxp = 1500,
  #ZeroFilter para
  zero.filter = TRUE,
  obs.zero.cutoff = 0,
  var.zero.cutoff = 1 - oneDCparameter[[12]]/100,
  #DataNormalization
  normalization = TRUE,
  method = oneDCparameter[[14]],
  multiple = oneDCparameter[[19]],
  threads = 2,
  #PeakIdentification
  hmdb.matching = FALSE,
  show = 5,
  mass.tolerance = 30,
  mz.tolerance = oneDCparameter[[29]],
  rt.tolerance = oneDCparameter[[30]],
  #DataOverview para
  met.plot = FALSE,
  path = one.user.path,
  # worklist.from = "manual",
  #other slection
  qc.outlier.filter = TRUE,
  subject.outlier.filter = TRUE,
  integration = TRUE)},silent = FALSE)
