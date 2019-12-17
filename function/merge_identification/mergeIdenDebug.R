setwd("C:/Users/zhulab/Desktop/cff")
temp.file <- "ms1.peak.table.csv"
merge.iden.ms1.raw <- lapply(1:length(temp.file), function(idx){
  as.data.frame(readr::read_csv(temp.file[idx],
                                col_types = readr::cols()))
})

temp.file <- "ms2.csv"
merge.iden.ms2.raw <- lapply(1:length(temp.file), function(idx){
  as.data.frame(readr::read_csv(temp.file[idx],
                                col_types = readr::cols()))
})




temp <- try({lapply(1:1, function(idx){
  mergeIden(peak.table1 = merge.iden.ms1.raw,
            peak.table2 = merge.iden.ms2.raw,
            mz.tolerance = 30,
            rt.tolerance = 180)
})[[1]]
})
