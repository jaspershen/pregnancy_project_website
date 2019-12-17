
dataIntegration <- function(sample,
                            sample.info,
                            method = c("qc.median", "qc.mean",
                                       "none",
                                       "subject.median", "subject.mean")
){
  method <- match.arg(method)
  if(method == "no"){
  return(sample)
  }

  ##only one batch or no batch information
  if(all(colnames(sample.info) != "batch")) return(sample)
  if(any(colnames(sample.info) == "batch")) {
    if(length(unique(sample.info[,"batch"])) == 1){
      return(sample)
    }
  }

  ###split batch
  return.result <- splitBatch(sample = sample, sample.info = sample.info)

  subject <- return.result[[1]]
  qc <- return.result[[2]]

  subject.mean <- lapply(subject, function(x) {
    apply(x, 1, function(x) mean(x, na.rm = TRUE))
  })

  subject.median <- lapply(subject, function(x) {
    apply(x, 1, function(x) median(x, na.rm = TRUE))
  })

  if(!is.null(qc[[1]])){
    qc.mean <- lapply(qc, function(x) {
      apply(x, 1, function(x) mean(x, na.rm = TRUE))
    })

    qc.median <- lapply(qc, function(x) {
      apply(x, 1, function(x) median(x, na.rm = TRUE))
    })

  }else{
    qc.mean <- NULL
    qc.median <- NULL
  }


  if(method == "qc.mean" & is.null(qc.mean)){
    ref <- lapply(subject.mean, function(x) {
      subject.mean[[1]] / x
    })
  }


  if(method == "qc.mean" & !is.null(qc.mean)){
    ref <- lapply(qc.mean, function(x) {
      qc.mean[[1]] / x
    })
  }


  if(method == "qc.median" & is.null(qc.median)){
    ref <- lapply(subject.median, function(x) {
      subject.median[[1]] / x
    })
  }

  if(method == "qc.median" & !is.null(qc.median)){
    ref <- lapply(qc.median, function(x) {
      qc.median[[1]] / x
    })
  }

  if(method == "subject.mean"){
    ref <-
      lapply(subject.mean, function(x) {
        subject.mean[[1]] / x
      })
  }

  if(method == "subject.median"){
    ref <-
      lapply(subject.median, function(x) {
        subject.median[[1]] / x
      })
  }


  subject2 <- as.list(rep(NA, length(subject)))
  qc2 <- as.list(rep(NA, length(subject)))


  for (i in seq_along(subject)) {
    subject2[[i]] <- subject[[i]] * ref[[i]]
  }

  if(!is.null(qc[[1]])){
    for (i in seq_along(subject)) {
      qc2[[i]] <- qc[[i]] * ref[[i]]
    }
  }else{
    qc2 <- NULL
  }



  subject3 <- do.call(cbind, subject2)
  if(is.null(qc2)){
    qc3 <- NULL
  }else{
    qc3 <- do.call(cbind, qc2)
  }


  sample <- cbind(subject3, qc2)

  sample <- sample[,match(sample.info$sample.name, colnames(sample))]
  sample[which(is.na(sample), arr.ind = TRUE)] <- 0
  sample

}



splitBatch <- function(sample, sample.info){
  batch <- unique(sample.info$batch)

  subject.idx <- which(sample.info$group != "QC")
  qc.idx <- which(sample.info$group == "QC")

  subject <- sample[,subject.idx]
  if(length(qc.idx) > 0){
    qc <- sample[,qc.idx]
  }else{
    qc <- NULL
  }

  subject.info <- sample.info[which(sample.info$group != "QC"),]
  qc.info <- sample.info[which(sample.info$group == "QC"),]

  subject1 <- lapply(batch, function(x){
    subject[,which(subject.info$batch == x)]
  })

  if(!is.null(qc)){
    qc1 <- lapply(batch, function(x){
      qc[,which(qc.info$batch == x)]
    })
  }else{
    qc1 <- NULL
  }


  return.result <- c(list(subject1), list(qc1))
  names(return.result) <- c("subject", "qc")
  return.result

}






###single peak plot for data normalization
diSinglePeak <- function(sample,
                         tags,
                         sample.info,
                         peak.idx,
                         # col = "dodgerblue",
                         alpha = 0.5, size = 2, text = FALSE,
                         title = ""){
  sample <- as.data.frame(sample)
  tags <- as.data.frame(tags)
  sample.info <- as.data.frame(sample.info)
  if(any(colnames(sample.info) == "injection.order")){
    injection.order <- as.numeric(sample.info[,"injection.order"])
  }else{
    injection.order <- c(1:nrow(sample.info))
  }

  x <- as.numeric(sample[peak.idx,])
  sample.name <- colnames(sample)
  group <- sample.info[,"group"]
  if(all(colnames(sample.info) != "batch")){
    batch <- rep(1, nrow(sample.info))
  }else{
    batch <- sample.info[,"batch"]
  }


  temp <- data.frame(sample.name, injection.order, x, batch, stringsAsFactors = FALSE)

  colnames(temp) <- c("Sample", "Injection.order", "Intensity", "Batch")
  temp$Batch <- as.factor(temp$Batch)
  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10),
  #                  axis.title.y = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
  #                  axis.text.y = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Injection.order, y = Intensity,
                                              label = Sample, label2 = Batch)) +
    ggplot2::geom_point(alpha = alpha,
                        # colour = col,
                        size = size,
                        ggplot2::aes(colour = Batch)) +
    ggplot2::labs(x = "Injection order",
                  y = "Intensity") +
    ggplot2::ggtitle(title) +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))

  if(text) {
    plot <- plot+ggplot2::geom_text(aes(x = Injection.order, y = Intensity, label = Sample),
                                    check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
  }

  return(plot)
}








#####box plot for each sample
###single peak plot for data normalization
diSampleBoxplot <- function(sample,
                            sample.info,
                            group,
                            # col = "dodgerblue",
                            # alpha = 0.5,
                            # size = 2,
                            text = FALSE,
                            title = ""){
  sample <- as.data.frame(sample)
  sample.info <- as.data.frame(sample.info)

  if(any(colnames(sample.info) == "injection.order")){
    sample.info <- sample.info[order(sample.info$injection.order),]
  }

  sample.info <- sample.info[sample.info[,"group"] %in% group,]
  sample <- sample[,match(sample.info[,1], colnames(sample))]
  sample <- sxtScale(sample = sample, method = "auto", center = TRUE)

  temp <- apply(sample, 2, list)
  temp <- lapply(temp, unlist)

  temp <- mapply(function(x, y, z){
    list(data.frame("Sample" = rep(x, nrow(sample)),
                    "Intensity" = y,
                    "Batch" = rep(z, nrow(sample)),
                    stringsAsFactors = FALSE))
  },
  x = colnames(sample),
  y = temp,
  z = as.character(sample.info[,"batch"]))

  temp <- do.call(rbind, temp)

  temp$Sample <- factor(temp$Sample,
                        levels = sample.info$sample.name,
                        ordered = TRUE)

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  temp[,"Sample"] <- as.factor(temp[,"Sample"])

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Sample, y = Intensity,
                                              color = Batch
                                              # label = Sample
  )) +
    ggplot2::geom_boxplot() +
    # ggplot2::geom_dotplot(
    #                       binaxis='y',
    #                       stackdir='center',
    #                       dotsize=0.1
    #                       ) +
    ggplot2::labs(x = "Sample",
                  y = "Intensity") +
    ggplot2::ggtitle(title) +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))


  return(plot)
}




