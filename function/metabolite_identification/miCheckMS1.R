
setGeneric(name = "miCheckMS1",
           def = function(peak.table = peak.table){

             ##check peak table
               data.record <- NULL
               #name check
               data.col.name <- colnames(peak.table)
               if(data.col.name[1] != "name"){
                 data.record <- c(data.record, "Error: No name")
               }else{
                 # cat("OK: The first column of data is name.\n")
                 data.record <- c(data.record, "OK")
               }
               #mz check
               if(data.col.name[2] != "mz"){
                 data.record <- c(data.record, "Error: No mz")
               }else{
                 data.record <- c(data.record, "OK")
               }

               #rt check
               if(data.col.name[3] != "rt"){
                 data.record <- c(data.record, "Error: No rt")
               }else{
                 data.record <- c(data.record, "OK")
               }

             data.record <-
               cbind("MS1 peak table",
                     c("Column 1 is name", "Column 2 is mz", "Column 3 is rt"),
                     data.record)
             colnames(data.record) <- c("Data.File", "Information", "Result")
             as.data.frame(data.record)
           })