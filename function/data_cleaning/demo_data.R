setwd("F:/data pre-processing/demo data for web")
batch1 <- readr::read_csv("batch1.csv")
batch2 <- readr::read_csv("batch2.csv")
sample.info <- read.csv("sample.information.csv", stringsAsFactors = FALSE)
sample.info1 <- sample.info[which(sample.info$batch %in% c(1,2)),]
##
colnames(batch1) <- gsub(pattern = "-POS", replacement = "", x = colnames(batch1))
colnames(batch1) <- stringr::str_replace_all(string = colnames(batch1), pattern = "Sample[0-9]{1,3}\\-", replacement = "")
colnames(batch1) <- paste("EC", colnames(batch1), sep = "")


colnames(batch1)[1:3] <- c("name", "mz", "rt")
colnames(batch1)[grep("QC", colnames(batch1))] <- gsub(pattern = "EC", replacement = "", x = colnames(batch1)[grep("QC", colnames(batch1))])

colnames(batch2) <- gsub(pattern = "-POS", replacement = "", x = colnames(batch2))
colnames(batch2) <- stringr::str_replace_all(string = colnames(batch2), pattern = "Sample[0-9]{1,3}\\-", replacement = "")
colnames(batch2) <- paste("EC", colnames(batch2), sep = "")

colnames(batch2)[1:3] <- c("name", "mz", "rt")
colnames(batch2)[grep("QC", colnames(batch2))] <- gsub(pattern = "EC", replacement = "", x = colnames(batch2)[grep("QC", colnames(batch2))])

colnames(batch1)[which(is.na(match(colnames(batch1), sample.info$sample.name)))]
colnames(batch2)[which(is.na(match(colnames(batch2), sample.info$sample.name)))]


sample.info1$sample.name[which(is.na(match(sample.info1$sample.name, c(colnames(batch1), colnames(batch2)))))]


colnames(batch1)[which(colnames(batch1) == "ECFB123")] <- c("ECFB123_1", "ECFB123_2")
colnames(batch1)[which(colnames(batch1) == "EC6200")] <- "EC6200_1"
colnames(batch1)[which(colnames(batch1) == "EC8056")] <- "EC8056_1"

colnames(batch2)[which(colnames(batch2) == "EC0628")] <- c("EC0628_1", "EC0628_2")
colnames(batch2)[which(colnames(batch2) == "EC627")] <- "EC627_1"
colnames(batch2)[which(colnames(batch2) == "EC711")] <- "EC711_1"

sample.info1$sample.name[which(is.na(match(sample.info1$sample.name, c(colnames(batch1), colnames(batch2)))))]

write.csv(batch1, "batch1.raw.csv", row.names = FALSE)
write.csv(batch2, "batch2.raw.csv", row.names = FALSE)



int1 <- apply(batch1[,-c(1:3)], 1, function(x) mean(x, na.rm = TRUE))
temp.name <- data.frame(batch1$name, int1, stringsAsFactors = FALSE)

temp.name <- temp.name[order(temp.name$int1, decreasing = TRUE),]

temp.name <- temp.name[1:500,]

batch1 <- batch1[batch1$name %in% temp.name$batch1.name,]





int1 <- apply(batch2[,-c(1:3)], 1, function(x) mean(x, na.rm = TRUE))
temp.name <- data.frame(batch2$name, int1, stringsAsFactors = FALSE)

temp.name <- temp.name[order(temp.name$int1, decreasing = TRUE),]

temp.name <- temp.name[1:500,]

batch2 <- batch2[batch2$name %in% temp.name$batch2.name,]

write.csv(batch1, "batch1.demo.csv", row.names = FALSE)
write.csv(batch2, "batch2.demo.csv", row.names = FALSE)


write.csv(sample.info1, "sample.info.demo.csv", row.names = FALSE)
