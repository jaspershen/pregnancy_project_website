pcaAnalysis <- function(data,
                        log.method = c("no", "log2", "loge", "log10"),
                        scale.method = c("no", "pareto", "auto"),
                        center = TRUE){


data <- as.data.frame(data)
log.method <- match.arg(log.method)
scale.method <- match.arg(scale.method)

sample <- data[,-c(1:3)]
##MV should be replace by 0
sample <- apply(sample, 2, function(x){
  x[is.na(x)] <- 0
  x
})
sample <- as.data.frame(sample)
tags <- data[,c(1:3)]

##log
sample <- sxtLog(sample = sample, method = log.method)

##scale
sample <- sxtScale(sample = sample,
                          method = scale.method,
                          center = center)


##pca
pca.object <- prcomp(data.frame(t(sample)),
                        retx = TRUE,
                        center = FALSE,
                        scale = FALSE)
pca.object
}


pcaScorePlot <- function(pca.object,
                         batch.info,
                         class.info){
  if(is.null(pca.object)) return(NULL)
  #group.info has tow column, column 1 is sample name, column is group
  pov <- summary(pca.object)$importance[2,]
  sd <- summary(pca.object)$importance[1,]
  cp <- summary(pca.object)$importance[3,]
  pc <- pca.object$x

  pc1 <- round(pov[1], 2)
  pc2 <- round(pov[2], 2)

  data <- data.frame(rownames(pc), pc[,c(1:2)], stringsAsFactors = FALSE)
  batch.info <- batch.info[batch.info[,1] %in% rownames(pc),]
  class.info <- class.info[class.info[,1] %in% rownames(pc),]
  colnames(data)[1] <- "sample.name"
  data <- data[order(data$sample.name),]
  batch.info <- batch.info[order(batch.info[,1]),]
  batch.info[,2] <- as.character(batch.info[,2])

  class.info <- class.info[order(class.info[,1]),]
  class.info[,2] <- as.character(class.info[,2])

  data <- data.frame(data, batch.info[,2],class.info[,2], stringsAsFactors = FALSE)
  colnames(data)[ncol(data)-1] <- "Batch"
  colnames(data)[ncol(data)] <- "Class"
  batch_class <- paste(batch.info[,2], class.info[,2], sep = "_")
  data <- data.frame(data, batch_class, stringsAsFactors = FALSE)
  colnames(data)[ncol(data)] <- "Batch_Class"

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

   score.plot <-
    ggplot2::ggplot(data = data,
                    ggplot2::aes(x = PC1, y = PC2,
                                 colour = Batch_Class,
                                 # shape = Class,
                                 label = sample.name)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::labs(x = paste("PC1:", pc1),
                  y = paste("PC2", pc2)
                  # colour = "Batch"
                  ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20))+
    my.theme+ggplot2::ggtitle("PCA score plot")+
     ggplot2::geom_hline(yintercept = 0)+
     ggplot2::geom_vline(xintercept = 0)

   score.plot
}




pcaScorePlot2 <- function(pca.object,
                         sample.info){
  if(is.null(pca.object)) return(NULL)
  #group.info has tow column, column 1 is sample name, column is group
  pov <- summary(pca.object)$importance[2,]
  sd <- summary(pca.object)$importance[1,]
  cp <- summary(pca.object)$importance[3,]
  pc <- pca.object$x

  pc <- pc[order(rownames(pc)),]
  sample.info <- sample.info[order(sample.info$sample.name),]

  pc1 <- round(pov[1], 2)
  pc2 <- round(pov[2], 2)

  data <- data.frame(rownames(pc), pc[,c(1:2)], stringsAsFactors = FALSE)
  colnames(data)[1] <- "sample.name"
  data <- data[order(data$sample.name),]

  data <- data.frame(data, sample.info[,5], stringsAsFactors = FALSE)
  colnames(data)[ncol(data)] <- "Group"

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  score.plot <-
    ggplot2::ggplot(data = data,
                    ggplot2::aes(x = PC1, y = PC2,
                                 colour = Group,
                                 label = sample.name)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::labs(x = paste("PC1:", pc1),
                  y = paste("PC2", pc2)
                  # colour = "Batch"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20))+
    my.theme+ggplot2::ggtitle("PCA score plot")+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::geom_vline(xintercept = 0)

  score.plot
}
