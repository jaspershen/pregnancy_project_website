checkParamTable <- function(userParamTable,
                            demoParamTable){

  if(ncol(userParamTable) != 3) {
  return("Error: No 3 columns")
  }

 if(colnames(userParamTable)[1] != "Parameter") return("Error: Column 1 is not Parameter")
 if(colnames(userParamTable)[2] != "Value") return("Error: Column 2 is not Value")
 if(nrow(userParamTable) != nrow(demoParamTable)) return("Error: Number of rows is wrong")
 if(any(as.character(userParamTable[,1]) != as.character(demoParamTable[,1]))) return("Error: Parameter names are wrong")

 ##parameter check
 #Polarity
  if(!userParamTable[2,2] %in% c("positive", "negative")) return("Error: Check your Polarity")
  #m/z tolerance (ppm)
  temp.param <- as.numeric(userParamTable[4,2])
  if(is.na(temp.param)) return("Error: Check your m/z tolerance (ppm)")
  if(temp.param <= 0) return("Error: m/z tolerance (ppm) should be larger than 0")

  #Retention tolerance (second)
  temp.param <- as.numeric(userParamTable[5,2])
    if(is.na(temp.param)) return("Error: Check your Retention tolerance (second)")
    if(temp.param <= 0) return("Error: Retention tolerance (second) should be larger than 0")


  ##Peak will be removed if MV ratio > %
  temp.param <- as.numeric(userParamTable[7,2])
    if(is.na(temp.param)) return("Error: Check your Peak will be removed if MV ratio > %")
    if(temp.param < 1 | temp.param > 50) return("Error: Peak will be removed if MV ratio > % should be between 1 and 50")

#MV imputation method
  temp.param <- as.character(userParamTable[8,2])
  if(is.na(temp.param)) return("Error: Check your MV imputation method")
  if(!temp.param %in% c("Zero value", "Mean", "Median", "Minimum", "KNN", "missForest", "BPCA", "PPCA", "SVD")) return("Error: Check your MV imputation method")

  ## Number of neighbors
  temp.param <- as.numeric(userParamTable[9,2])
  if(is.na(temp.param)) return("Error: Check your Number of neighbors")
  if(temp.param > 50 | temp.param < 2) return("Error: Number of neighbors should be between 2 and 50")

  ## The maximum percent missing data allowed in any row
  temp.param <- as.numeric(userParamTable[10,2])
  if(is.na(temp.param)) return("Error: Check The maximum percent missing data allowed in any row")
  if(temp.param > 70 | temp.param < 1) return("Error: The maximum percent missing data allowed in any row should be between 1 and 70")

  ## The maximum percent missing data allowed in any column
  temp.param <- as.numeric(userParamTable[11,2])
  if(is.na(temp.param)) return("Error: Check The maximum percent missing data allowed in any column")
  if(temp.param > 90 | temp.param < 1) return("Error: The maximum percent missing data allowed in any column should be between 1 and 90")

  ## The Number of trees to grow in each forest
  temp.param <- as.numeric(userParamTable[12,2])
  if(is.na(temp.param)) return("Error: Check Number of trees to grow in each forest")
  # if(temp.param > 90 | temp.param < 1) return("Error: Number of trees to grow in each forest should be between 1 and 90")

  #Bootstrap sampling (with replacements) is performed
  temp.param <- as.logical(userParamTable[13,2])
  if(is.na(temp.param)) return("Error: Check Bootstrap sampling (with replacements) is performed?")
  # if(temp.param > 90 | temp.param < 1) return("Error: Number of trees to grow in each forest should be between 1 and 90")

  #Number of principal components to calculate
  temp.param <- as.numeric(userParamTable[14,2])
  if(is.na(temp.param)) return("Error: Check Number of principal components to calculate")
  if(temp.param > 5 | temp.param < 1) return("Error: Number of principal components to calculate should be between 1 and 5")

  #Peak removed if zero ratio > %
  temp.param <- as.numeric(userParamTable[16,2])
  if(is.na(temp.param)) return("Error: Check Peak removed if zero ratio > %")
  if(temp.param > 100 | temp.param < 1) return("Error: Peak removed if zero ratio > % should be between 1 and 100")

  #Method based on QC sample data, normalization
  temp.param <- as.character(userParamTable[18,2])
  if(is.na(temp.param)) return("Error: Check Method based on QC sample data, normalization")
  if(!temp.param %in% c("YES", "NO")) return("Error: Method based on QC sample data, normalization should be YES or NO")

  #Normalization method
  temp.param <- as.character(userParamTable[19,2])
  if(is.na(temp.param)) return("Error: Check Normalization method")
  # if(!temp.param %in% c("QC SVR (MetNormalizer)", "QC LOESS")) return("Error: Check Normalization method")

  # KEPP dimension or not?
  temp.param <- as.logical(userParamTable[20,2])
  if(is.na(temp.param)) return("Error: Check KEPP dimension or not?")
  # if(!temp.param %in% c("QC SVR (MetNormalizer)", "QC LOESS")) return("Error: Check Normalization method")
  #

  # Optimize parameters?
  temp.param <- as.logical(userParamTable[21,2])
  if(is.na(temp.param)) return("Error: Check Optimize parameters?")
  # if(!temp.param %in% c("QC SVR (MetNormalizer)", "QC LOESS")) return("Error: Check Normalization method")

  # Beigin and End
  temp.param <- userParamTable[22,2]
  temp.param <- as.numeric(strsplit(temp.param, split = ",")[[1]])
  if(length(temp.param) != 2) return("Error: Check Beigin and End")
  if(any(is.na(temp.param))) return("Error: Check Optimize parameters?")
  if(any(temp.param > 1) | any(temp.param < 0.5)) return("Error: Check Optimize parameters?")
  if(temp.param[2] <= temp.param[1]) return("Error: Check Optimize parameters?")

  # Step of LOESS
  temp.param <- as.numeric(userParamTable[23,2])
  if(is.na(temp.param)) return("Error: Check Step of LOESS")
  if(!temp.param %in% c(0.2, 0.3, 0.4)) return("Error: Step of LOESS should be 0.2, 0.3 or 0.4")


  #How many peaks used?
  temp.param <- as.numeric(userParamTable[24,2])
  if(is.na(temp.param)) return("Error: Check How many peaks used?")
  if(!temp.param %in% c(1:10)) return("Error: Step of LOESS should be between 1 to 10")

  #Method based on QC sample data, integration
  temp.param <- as.character(userParamTable[26,2])
  if(is.na(temp.param)) return("Error: Check ethod based on QC sample data, integration")
  if(!temp.param %in% c("YES", "NO")) return("Error: Method based on QC sample data, integration should be YES or NO")


  #Integration method
  temp.param <- as.character(userParamTable[27,2])
  if(is.na(temp.param)) return("Error: Check Integration method")
  # if(!temp.param %in% c("QC SVR (MetNormalizer)", "QC LOESS")) return("Error: Check Normalization method")


  #Logarithm method
  temp.param <- as.character(userParamTable[29,2])
  if(is.na(temp.param)) return("Error: Check Logarithm method")
  if(!temp.param %in% c("No log", "Log 2", "Log e", "Log 10")) return("Error: Check Logarithm method")

  #Scale method
  temp.param <- as.character(userParamTable[30,2])
  if(is.na(temp.param)) return("Error: Check Scale method")
  if(!temp.param %in% c("Pareto scale", "Auto scale", "Log e", "No scale")) return("Error: Check Scale method")

  #Center or not
  temp.param <- as.logical(userParamTable[31,2])
  if(is.na(temp.param)) return("Error: Check Center or not")
  # if(!temp.param %in% c("Pareto scale", "Auto scale", "Log e", "No scale")) return("Error: Check Scale method")

  ##Samples will be considered as outliers outside % CI
  temp.param <- as.numeric(userParamTable[32,2])
  if(is.na(temp.param)) return("Error: Check Samples will be considered as outliers outside % CI")
  if(temp.param < 90 | temp.param > 100) return("Error: Samples will be considered as outliers outside % CI")

  ##Samples will be considered as outliers with zero value ratio > %
  temp.param <- as.numeric(userParamTable[33,2])
  if(is.na(temp.param)) return("Error: Check Samples will be considered as outliers with zero value ratio > %")
  if(temp.param < 0 | temp.param > 100) return("Error: Samples will be considered as outliers with zero value ratio > % should be between 0 and 100")


  #Filter QC outliers
  temp.param <- as.logical(userParamTable[34,2])
  if(is.na(temp.param)) return("Error: Check Filter QC outliers")
  # if(!temp.param %in% c("Pareto scale", "Auto scale", "Log e", "No scale")) return("Error: Check Scale method")

  #Filter subject outliers
  temp.param <- as.logical(userParamTable[35,2])
  if(is.na(temp.param)) return("Error: Check Filter subject outliers")
  # if(!temp.param %in% c("Pareto scale", "Auto scale", "Log e", "No scale")) return("Error: Check Scale method")
  #
  #
  return("OK")
  }