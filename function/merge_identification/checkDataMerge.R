
setGeneric(name = "checkDataMerge",
           def = function(peak.table1 = peak.table1,
                          peak.table2 = peak.table2){

             ##check peak table
             data.record1 <- lapply(peak.table1, function(data){
               temp.data.record <- NULL
               #name check
               data.col.name <- colnames(data)
               if(data.col.name[1] != "name"){
                 temp.data.record <- c(temp.data.record, "Error: No name")
               }else{
                 # cat("OK: The first column of data is name.\n")
                 temp.data.record <- c(temp.data.record, "OK")
               }
               #mz check
               if(data.col.name[2] != "mz"){
                 temp.data.record <- c(temp.data.record, "Error: No mz")
               }else{
                 temp.data.record <- c(temp.data.record, "OK")
               }

               #rt check
               if(data.col.name[3] != "rt"){
                 temp.data.record <- c(temp.data.record, "Error: No rt")
               }else{
                 temp.data.record <- c(temp.data.record, "OK")
               }

               temp.data.record

             })


             ##check peak table
             data.record2 <- lapply(peak.table2, function(data){
               temp.data.record <- NULL
               #name check
               data.col.name <- colnames(data)
               if(all(data.col.name != "name")){
                 temp.data.record <- c(temp.data.record, "Error: No name")
               }else{
                 temp.data.record <- c(temp.data.record, "OK")
               }
               #mz check
               if(all(data.col.name != "mzmed")){
                 temp.data.record <- c(temp.data.record, "Error: No mzmed")
               }else{
                 temp.data.record <- c(temp.data.record, "OK")
               }
               #rt check
               if(all(data.col.name != "rtmed")){
                 temp.data.record <- c(temp.data.record, "Error: No rtmed")
               }else{
                 temp.data.record <- c(temp.data.record, "OK")
               }

               if(all(data.col.name != "hits.reverse") &
                  all(data.col.name != "hits.reverse_zhumetlib") &
                  all(data.col.name != "hits.reverse_metlinlib")){
                 temp.data.record <- c(temp.data.record, "Error: No hits.reverse")
               }else{
                 temp.data.record <- c(temp.data.record, "OK")
               }

               if(all(data.col.name != "hits.forward") &
                  all(data.col.name != "hits.forward_zhumetlib") &
                  all(data.col.name != "hits.forward_metlinlib")){
                 temp.data.record <- c(temp.data.record, "Error: No hits.forward")
               }else{
                 temp.data.record <- c(temp.data.record, "OK")
               }

               temp.data.record

             })

             data.record1 <- lapply(1:length(data.record1), function(idx){
                 cbind(paste("MS1 peak table", idx),
                       c("Column 1 is name", "Column 2 is mz", "Column 3 is rt"),
                       data.record1[[idx]])
             })

             data.record1 <- do.call(rbind, data.record1)
             colnames(data.record1) <- c("Data.File", "Information", "Result")


             data.record2 <- lapply(1:length(data.record2), function(idx){
               cbind(paste("MS2 peak table", idx),
                     c("name", "mzmed", "rtmed",
                       "hits.reverse", "hits.forward"),
                     data.record2[[idx]])
             })

             data.record2 <- do.call(rbind, data.record2)
             colnames(data.record2) <- c("Data.File", "Information", "Result")



             check.result <- rbind(data.record1, data.record2)
             check.result <- as.data.frame(check.result)
             rownames(check.result) <- NULL
             check.result[,1] <- as.character(check.result[,1])
             check.result[,2] <- as.character(check.result[,2])
             check.result[,3] <- as.character(check.result[,3])
             check.result
           })