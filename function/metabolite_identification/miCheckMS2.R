
setGeneric(name = "miCheckMS2",
           def = function(ms2.data = ms2.data){
             ##check peak table
               data.record <- length(ms2.data)
             data.record <-
               cbind("MS/MS data",
                     "MS/MS spectra number",
                     data.record)
             colnames(data.record) <- c("Data.File", "Information", "Result")
             as.data.frame(data.record)
           })