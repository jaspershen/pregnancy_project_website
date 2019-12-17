
setGeneric(name = "checkData",
           def = function(peak.table = peak.table,
                          sample.info = sample.info,
                          step = c("cleaning", "biomarker")){
             step <- match.arg(step)

             ##check peak table
             data.record <- lapply(peak.table, function(data){
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

               ##if is for biomarker, NA check
              if(step == "biomarker"){
                if(sum(is.na(data)) != 0){
                  temp.data.record <- c(temp.data.record, "Error: NA in MS1 peak table")
                }else{
                  temp.data.record <- c(temp.data.record, "OK")
                }
              }

               temp.data.record

             })



             sample.info.record <- NULL
             if(sum(is.na(sample.info)) > 0){
               sample.info.record <- c(sample.info.record, "Error: NA in sample.information")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }

             if(ifelse(is.na(sum(sample.info == "") > 0), FALSE, sum(sample.info == "") > 0)){
               sample.info.record <- c(sample.info.record, "Error: space in sample.information")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }


             if(colnames(sample.info)[1] != "sample.name"){
               sample.info.record <- c(sample.info.record, "Error: No sample.name")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }

             if(colnames(sample.info)[2] != "injection.order"){
               sample.info.record <- c(sample.info.record, "Error: No injection.order")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }

             if(colnames(sample.info)[3] != "class"){
               sample.info.record <- c(sample.info.record, "Error: No class")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }

             if(colnames(sample.info)[4] != "batch"){
               sample.info.record <- c(sample.info.record, "Error: No batch")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }

             if(colnames(sample.info)[5] != "group"){
               sample.info.record <- c(sample.info.record, "Error: No group")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }


             if(ncol(sample.info) > 3){
               class <-  unique(as.character(sample.info[,3]))
               if(all(c("Subject") %in% class)){
                 sample.info.record <- c(sample.info.record, "OK")
               }else{
                 sample.info.record <- c(sample.info.record, "Error: No Subject")
               }
             }else{
               sample.info.record <- c(sample.info.record, "Error: No Subject")
             }

             ## name in sample.info and data same or not

             sample.name1 <- as.character(sample.info[,1])
             sample.name2 <- unlist(lapply(peak.table, function(x){
               colnames(x)
             }))
             sample.name2 <- unique(sample.name2)
             sample.idx <- match(sample.name1, sample.name2)

             if(any(is.na(sample.idx))){
               sample.info.record <- c(sample.info.record, "Error: The sample names in sample.inforamtion and data are not same")
             }else{
               sample.info.record <- c(sample.info.record, "OK")
             }


             data.record <- lapply(1:length(data.record), function(idx){
               if(step == "biomarker"){
                 cbind(paste("MS1 peak table", idx),
                       c("Column 1 is name", "Column 2 is mz", "Column 3 is rt",
                         "NA in MS1 peak table"),
                       data.record[[idx]])
               }else{
                 cbind(paste("MS1 peak table", idx),
                       c("Column 1 is name", "Column 2 is mz", "Column 3 is rt"),
                       data.record[[idx]])
               }

             })



             data.record <- do.call(rbind, data.record)
             colnames(data.record) <- c("Data.File", "Information", "Result")

             sample.info.record <- cbind("Sample information",
                                         c("NA in sample information",
                                           "Space in sample information",
                                           "sample.name", "injection.order",
                                           "class", "batch", "group", "QC or Subject in class",
                                           "sample names in QC and subject"),
                                         sample.info.record)

             colnames(sample.info.record) <- c("Data.File", "Information", "Result")

             check.result <- rbind(data.record, sample.info.record)
             check.result <- as.data.frame(check.result)
             rownames(check.result) <- NULL
             check.result[,1] <- as.character(check.result[,1])
             check.result[,2] <- as.character(check.result[,2])
             check.result[,3] <- as.character(check.result[,3])
             check.result
           })