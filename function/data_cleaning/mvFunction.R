mvImputation <- function(sample,
                         tags,
                         # group,
                         sample.info,
                         mv.peak.remove.tol = 50,
                         method = "KNN",
                         rowmax,
                         colmax,
                         k,
                         ntree,
                         replace,
                         bpca.npcs,
                         ppca.npcs,
                         svd.npcs
){
##sample, the columns correspond samples and the rows correspond variables
  sample <- sample[,order(colnames(sample))]
  sample.info <- sample.info[order(sample.info$sample.name),]

  sample <- apply(sample, 2, as.numeric)
  ##remove the peaks with zero ratio > tolerance
  # idx <- match(sample.info$sample.name[which(sample.info$group == group)], colnames(sample))
  ##if there are QC in data, using QC, if no QC, use all samples.

  group <- unique(sample.info$group)

  mv.ratio <- lapply(group, function(x){
    temp.idx <- which(sample.info$group == x)
    apply(sample[,temp.idx], 1, function(x) {
      sum(is.na(x))/ncol(sample[,temp.idx])
    })
  })

  mv.ratio <- do.call(cbind, mv.ratio)

  remain.idx <- which(apply(mv.ratio, 1, function(x){
   any(x <= mv.peak.remove.tol/100)
  }))


  # if(any(sample.info$class == "QC")){
  #   idx <- match(sample.info$sample.name[which(sample.info$group == "QC")], colnames(sample))
  # }else{
  #   idx <- 1:ncol(sample)
  # }
  #
  #
  # mv.ratio <- apply(sample[,idx], 1, function(x) {
  #   sum(is.na(x))/ncol(sample[,idx])
  # })
  #
  # remain.idx <- which(mv.ratio <= mv.peak.remove.tol/100)


  sample <- sample[remain.idx,,drop = FALSE]
  tags <- tags[remain.idx,,drop = FALSE]


  if(method == "Zero value"){
    sample <- apply(sample, 2, function(x){
      x[is.na(x)] <- 0
      x
    })
    sample <- as.data.frame(sample)
  }


  if(method == "Mean"){
    sample <- t(apply(sample, 1, function(x){
      x[is.na(x)] <- mean(x, na.rm = TRUE)
      x
    }))
    sample <- as.data.frame(sample)
  }

  if(method == "Median"){
    sample <- t(apply(sample, 1, function(x){
      x[is.na(x)] <- median(x, na.rm = TRUE)
      x
    }))
    sample <- as.data.frame(sample)
  }

  if(method == "Minimum"){
    sample <- t(apply(sample, 1, function(x){
      x[is.na(x)] <- min(x, na.rm = TRUE)
      x
    }))
    sample <- as.data.frame(sample)
  }


  if(method == "KNN"){
    rowmax1 <- max(apply(sample, 1, function(x){
      sum(is.na(x))/ncol(sample)
    }))

    colmax1 <- max(apply(sample, 2, function(x){
      sum(is.na(x))/nrow(sample)
    }))

    rowmax <- max(rowmax, rowmax1)
    colmax <- max(colmax, colmax1)

    sample <- impute::impute.knn(
      data = as.matrix(sample),
      k = k,
      rowmax = rowmax+0.01,
      colmax = colmax+0.01)
    sample <- sample[["data"]]
    sample <- as.data.frame(sample)
  }


  if(method == "missForest"){
    sample <- missForest::missForest(
      xmis = t(as.matrix(sample)),
      ntree = ntree,
      replace = replace, maxiter = 5)

    sample <- t(sample$ximp)
    sample <- as.data.frame(sample)
  }


  if(method == "BPCA"){

    sample <- pcaMethods::pca(t(sample),
                              method="bpca",
                              nPcs = bpca.npcs)
    sample <- t(pcaMethods::completeObs(sample))
    sample <- as.data.frame(sample)
  }

  if(method == "PPCA"){

    sample <- pcaMethods::pca(t(sample),
                              method="ppca",
                              nPcs = ppca.npcs)
    sample <- t(pcaMethods::completeObs(sample))
    sample <- as.data.frame(sample)
  }


  if(method == "SVD"){

    sample <- pcaMethods::pca(t(sample),
                              method="svdImpute",
                              nPcs = svd.npcs)
    sample <- t(pcaMethods::completeObs(sample))
    sample <- as.data.frame(sample)
  }


  sample.imputation <- cbind(tags, sample)
  return(sample.imputation)


}






# ############
# mvSamplePlot <- function(sample, tags,
#                          col = "dodgerblue",
#                          alpha = 0.5, size = 2,
#                          text = FALSE){
#   sample <- as.data.frame(sample)
#   tags <- as.data.frame(tags)
#   sample.mv.ratio <- apply(sample,2, function(x){
#     sum(is.na(x)*100/nrow(sample))
#   })
#
#   temp <- data.frame(colnames(sample), sample.mv.ratio,
#                      c(1:length(sample.mv.ratio)),stringsAsFactors = FALSE)
#   colnames(temp) <- c("Sample", "MV.ratio", "Index")
#
#   my.theme <- ggplot2::theme_bw()+
#     ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10),
#                    axis.title.y = ggplot2::element_text(size = 10)) +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
#                    axis.text.y = ggplot2::element_text(size = 10)) +
#     ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
#     ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
#
#   par(mar = c(5,5,4,2))
#   plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = MV.ratio,
#                                               label = Sample)) +
#     ggplot2::geom_point(alpha = alpha, colour = col, size = size) +
#     ggplot2::labs(x = "Sample",
#                   y = "Missing value ratio in Samples (%)") +
#     ggplot2::ggtitle("MV ratio in Samples") +
#     my.theme
#   # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
#   # ggplot2::coord_cartesian(xlim = xlim,
#   # ylim = ylim, expand = TRUE)
#
#   if(text){
#     plot <- plot+ggplot2::geom_text(aes(x = Index, y = MV.ratio, label = Sample),
#                                     check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
#   }
#
#
#   return(plot)
#
# }
#
#
#
#
#
#
# ############
# mvPeakPlot <- function(sample, tags, col = "dodgerblue",
#                        alpha = 0.5, size = 2, text = FALSE){
#   sample <- as.data.frame(sample)
#   tags <- as.data.frame(tags)
#   peak.mv.ratio <- apply(sample,1, function(x){
#     sum(is.na(x)*100/ncol(sample))
#   })
#
#   temp <- data.frame(tags[,"name"], peak.mv.ratio,
#                      c(1:length(peak.mv.ratio)),stringsAsFactors = FALSE)
#   colnames(temp) <- c("Peak", "MV.ratio", "Index")
#
#   my.theme <- ggplot2::theme_bw()+
#     ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10),
#                    axis.title.y = ggplot2::element_text(size = 10)) +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
#                    axis.text.y = ggplot2::element_text(size = 10)) +
#     ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
#     ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
#
#   par(mar = c(5,5,4,2))
#   plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = MV.ratio,
#                                               label = Peak)) +
#     ggplot2::geom_point(alpha = alpha, colour = col, size = size) +
#     ggplot2::labs(x = "Peak",
#                   y = "Missing value ratio in Peaks (%)") +
#     ggplot2::ggtitle("MV ratio in Peaks") +
#     my.theme
#   # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
#   # ggplot2::coord_cartesian(xlim = xlim,
#   # ylim = ylim, expand = TRUE)
#   if(text){
#     plot <- plot+ggplot2::geom_text(aes(x = Index, y = MV.ratio, label = Peak),
#                                     check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
#   }
#   return(plot)
# }

















