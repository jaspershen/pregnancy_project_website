zeroFilter <- function(sample,
                         tags,
                         # group,
                         sample.info,
                         zero.peak.remove.tol = 50
){

  sample <- sample[,order(colnames(sample))]
  sample.info <- sample.info[order(sample.info$sample.name),]

  sample <- apply(sample, 2, as.numeric)
  ##remove the peaks with zero ratio > tolerance
  # idx <- match(sample.info$sample.name[which(sample.info$group == group)], colnames(sample))
  ##if there are QC in data, using QC, if no QC, use all samples.

  group <- unique(sample.info$group)

  zero.ratio <- lapply(group, function(x){
    temp.idx <- which(sample.info$group == x)
    apply(sample[,temp.idx], 1, function(x) {
      sum(x == 0)/ncol(sample[,temp.idx])
    })
  })

  zero.ratio <- do.call(cbind, zero.ratio)

  remain.idx <- which(apply(zero.ratio, 1, function(x){
    any(x <= zero.peak.remove.tol/100)
  }))


  # if(any(sample.info$class == "QC")){
  #   idx <- match(sample.info$sample.name[which(sample.info$group == "QC")], colnames(sample))
  # }else{
  #   idx <- 1:ncol(sample)
  # }

  # zero.ratio <- apply(sample[,idx], 1, function(x) {
  #   sum(x == 0)/ncol(sample[,idx])
  # })

  # remain.idx <- which(zero.ratio <= zero.peak.remove.tol/100)


  sample <- sample[remain.idx,,drop = FALSE]
  tags <- tags[remain.idx,,drop = FALSE]


  data <- cbind(tags, sample)
  return(data)
}


