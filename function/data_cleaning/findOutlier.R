
##find outlier samples from PCA
##
pcaFindOutlier <- function(pca.object){
  loading <- summary(pca.object)$rotation
  pov <- summary(pca.object)$importance[2,]
  sd <- summary(pca.object)$importance[1,]
  cp <- summary(pca.object)$importance[3,]
  pc <- pca.object$x

  pc1 <- round(pov[1], 2)
  pc2 <- round(pov[2], 2)
  pc3 <- round(pov[3], 2)

  x <- pc[, 1]
  y <- pc[, 2]
  z <- pc[, 3]

  xmin <- 1.2 * min(x)
  xmax <- 1.2 * max(x)
  ymin <- 1.2 * min(y)
  ymax <- 1.2 * max(y)
  zmin <- 1.2 * min(z)
  zmax <- 1.2 * max(z)

  ellipse.data <-
    SXTellipse(as.numeric(cor(x, y)),
               scale = c(sd(x), sd(y)),
               centre = c(mean(x), mean(y)))
  data.for.plot <- ellipse.data[["ellipse.data"]]
  t = ellipse.data[["t"]]
  a = ellipse.data[["a"]]
  d = ellipse.data[["d"]]
  scale = ellipse.data[["scale"]]
  centre = ellipse.data[["centre"]]

  ## right point
  point1 <- c(t * scale[1] * cos(0) + centre[1], t * scale[2] *
                cos(-d) + centre[2])
  ## top point
  point2 <- c(t * scale[1] * cos(pi / 2) + centre[1], t * scale[2] *
                cos(pi / 2 - d) + centre[2])
  ## left point
  point3 <- c(t * scale[1] * cos(pi) + centre[1], t * scale[2] *
                cos(pi - d) + centre[2])

  ellipse.a <- sqrt(sum((point1 - centre) ^ 2))
  ellipse.b <- sqrt(sum((point2 - centre) ^ 2))
  ellipse.c <- sqrt(ellipse.a ^ 2 - ellipse.b ^ 2)

  ## get the focus points

  lm.reg <- lm(c(point1[2], centre[2]) ~ c(point1[1], centre[1]))
  a <- lm.reg$coefficients[[2]]
  b <- lm.reg$coefficients[[1]]

  foo.f.a <- a ^ 2 + 1
  foo.f.b <-  2 * a * (b - centre[2]) - 2 * centre[1]
  foo.f.c <- centre[1] ^ 2 + (b - centre[2]) ^ 2 - ellipse.c ^ 2

  foo.f <- function(x,
                    a = foo.f.a,
                    b = foo.f.b,
                    c = foo.f.c)
  {
    a * x ^ 2 + b * x + c
  }
  result1 <-
    uniroot(
      foo.f,
      c(0, 10000),
      a = foo.f.a,
      b = foo.f.b,
      c = foo.f.c,
      tol = 0.0001
    )
  result2 <-
    uniroot(
      foo.f,
      c(-10000, 0),
      a = foo.f.a,
      b = foo.f.b,
      c = foo.f.c,
      tol = 0.0001
    )

  p1 <- c(result1$root, foo.f(x = result1$root))
  p2 <- c(result2$root, foo.f(x = result2$root))

  x1 <- data.for.plot[, 1]
  y1 <- data.for.plot[, 2]
  ellipse.standard <-
    mean(sqrt((x1 - p1[1]) ^ 2 + (y1 - p1[2]) ^ 2) +
           sqrt((x1 - p2[1]) ^ 2 + (y1 - p2[2]) ^ 2))

  distance <-
    sqrt((x - p1[1]) ^ 2 + (y - p1[2]) ^ 2) +
    sqrt((x - p2[1]) ^ 2 + (y - p2[2]) ^ 2)

  outlier.index <- which(distance > ellipse.standard)
  if (length(outlier.index) > 0) {
    cat(names(x)[outlier.index], " are outliers.\n")
    names(outlier.index) <- names(x)[outlier.index]
  }

  return(outlier.index)
}




#-----------------------------------------------------
SXTellipse <- function(x,
                       scale = c(1, 1),
                       centre = c(0, 0),
                       level = 0.95,
                       t = sqrt(qchisq(level, 2)),
                       which = c(1, 2),
                       npoints = 100,
                       ...) {
  names <- c("x", "y")
  if (is.matrix(x)) {
    xind <- which[1]
    yind <- which[2]
    r <- x[xind, yind]
    if (missing(scale)) {
      scale <- sqrt(c(x[xind, xind], x[yind, yind]))
      if (scale[1] > 0)
        r <- r / scale[1]
      if (scale[2] > 0)
        r <- r / scale[2]
    }
    if (!is.null(dimnames(x)[[1]]))
      names <- dimnames(x)[[1]][c(xind, yind)]
  }
  else
    r <- x
  r <-
    min(max(r, -1), 1)  # clamp to -1..1, in case of rounding errors
  d <- acos(r)
  a <- seq(0, 2 * pi, len = npoints)
  ellipse.data = matrix(
    c(t * scale[1] * cos(a + d / 2) + centre[1], t * scale[2] *
        cos(a - d / 2) + centre[2]),
    npoints,
    2,
    dimnames = list(NULL,
                    names)
  )
  SXTellipseData <- list(
    ellipse.data = ellipse.data,
    t = t,
    a = a,
    d = d,
    scale = scale,
    centre = centre
  )
  class(SXTellipseData) = "SXTellipseData"
  return(SXTellipseData)
}


#####################

pcaPlot <- function(pca.object, cex = 1,
                    cex.axis = 1.5,
                    cex.lab = 1.8,
                    cex.main = 1.5,
                    col1 = "black",
                    col2 = "tomato"){
  loading <- summary(pca.object)$rotation
  pov <- summary(pca.object)$importance[2,]
  sd <- summary(pca.object)$importance[1,]
  cp <- summary(pca.object)$importance[3,]
  pc <- pca.object$x

  pc1 <- round(pov[1], 2)
  pc2 <- round(pov[2], 2)
  pc3 <- round(pov[3], 2)

  x <- pc[, 1]
  y <- pc[, 2]
  z <- pc[, 3]

  xmin <- 1.2 * min(x)
  xmax <- 1.2 * max(x)
  ymin <- 1.2 * min(y)
  ymax <- 1.2 * max(y)
  zmin <- 1.2 * min(z)
  zmax <- 1.2 * max(z)

  ellipse.data <-
    SXTellipse(as.numeric(cor(x, y)),
               scale = c(sd(x), sd(y)),
               centre = c(mean(x), mean(y)))
  data.for.plot <- ellipse.data[["ellipse.data"]]
  t = ellipse.data[["t"]]
  a = ellipse.data[["a"]]
  d = ellipse.data[["d"]]
  scale = ellipse.data[["scale"]]
  centre = ellipse.data[["centre"]]

  ## right point
  point1 <- c(t * scale[1] * cos(0) + centre[1], t * scale[2] *
                cos(-d) + centre[2])
  ## top point
  point2 <- c(t * scale[1] * cos(pi / 2) + centre[1], t * scale[2] *
                cos(pi / 2 - d) + centre[2])
  ## left point
  point3 <- c(t * scale[1] * cos(pi) + centre[1], t * scale[2] *
                cos(pi - d) + centre[2])

  ellipse.a <- sqrt(sum((point1 - centre) ^ 2))
  ellipse.b <- sqrt(sum((point2 - centre) ^ 2))
  ellipse.c <- sqrt(ellipse.a ^ 2 - ellipse.b ^ 2)

  ## get the focus points

  lm.reg <- lm(c(point1[2], centre[2]) ~ c(point1[1], centre[1]))
  a <- lm.reg$coefficients[[2]]
  b <- lm.reg$coefficients[[1]]

  foo.f.a <- a ^ 2 + 1
  foo.f.b <-  2 * a * (b - centre[2]) - 2 * centre[1]
  foo.f.c <- centre[1] ^ 2 + (b - centre[2]) ^ 2 - ellipse.c ^ 2

  foo.f <- function(x,
                    a = foo.f.a,
                    b = foo.f.b,
                    c = foo.f.c)
  {
    a * x ^ 2 + b * x + c
  }
  result1 <-
    uniroot(
      foo.f,
      c(0, 10000),
      a = foo.f.a,
      b = foo.f.b,
      c = foo.f.c,
      tol = 0.0001
    )
  result2 <-
    uniroot(
      foo.f,
      c(-10000, 0),
      a = foo.f.a,
      b = foo.f.b,
      c = foo.f.c,
      tol = 0.0001
    )

  p1 <- c(result1$root, foo.f(x = result1$root))
  p2 <- c(result2$root, foo.f(x = result2$root))

  x1 <- data.for.plot[, 1]
  y1 <- data.for.plot[, 2]
  ellipse.standard <-
    mean(sqrt((x1 - p1[1]) ^ 2 + (y1 - p1[2]) ^ 2) +
           sqrt((x1 - p2[1]) ^ 2 + (y1 - p2[2]) ^ 2))

  distance <-
    sqrt((x - p1[1]) ^ 2 + (y - p1[2]) ^ 2) +
    sqrt((x - p2[1]) ^ 2 + (y - p2[2]) ^ 2)

  outlier.index <- which(distance > ellipse.standard)
  if (length(outlier.index) > 0) {
    cat(names(x)[outlier.index], " are outliers.\n")
    colour <- rep(NA, length(x))
    colour[outlier.index] <- col2
    colour[is.na(colour)] <- col1
  }else{
    colour <- rep(col1, length(x))
  }

  par(mar = c(5,5,4,2))
  plot(
    x,
    y,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    xlab = paste("PC1:", pc1),
    ylab = paste("PC2:", pc2),
    cex = cex,
    cex.lab = cex.lab,
    cex.axis = cex.axis,
    cex.main = cex.axis,
    pch = 19,
    col = colour,
    main = "PCA score plot for outlier samples"
  )
  if (length(outlier.index) > 0) {
    text(x = x[outlier.index],
         y = y[outlier.index],
         names(x)[outlier.index],
         pos = 4)
  } else
    (text(x = x,
          y = y,
          names(x),
          pos = 4))
  abline(h = 0, lty = 2)
  abline(v = 0, lty = 2)
  lines(data.for.plot, lty = 2)
}





####PCA score plot2
pcaPlot2 <- function(pca.object,
                     sample.info,
                     cex = 1,
                     cex.axis = 1.5,
                     cex.lab = 1.8,
                     cex.main = 1.5,
                     col = c("dodgerblue", "firebrick1", "orange",
                             "springgreen2", "orchid"),
                     pch = c(19, 17, 15, 3, 4),
                     ellipse = TRUE,
                     ci = 95){
  loading <- summary(pca.object)$rotation
  pov <- summary(pca.object)$importance[2,]
  sd <- summary(pca.object)$importance[1,]
  cp <- summary(pca.object)$importance[3,]
  pc <- pca.object$x

  pc1 <- round(pov[1], 2)
  pc2 <- round(pov[2], 2)
  pc3 <- round(pov[3], 2)

  x <- pc[, 1]
  y <- pc[, 2]
  z <- pc[, 3]

  xmin <- 1.2 * min(x)
  xmax <- 1.2 * max(x)
  ymin <- 1.2 * min(y)
  ymax <- 1.2 * max(y)
  zmin <- 1.2 * min(z)
  zmax <- 1.2 * max(z)

  info <- lapply(unique(sample.info[,2]), function(x){
    sample.info[,1][which(sample.info[,2] == x)]
  })

  names(info) <- unique(sample.info[,2])

  label <- list()
  for (i in 1:length(info)) {
    label[[i]]<-match(as.character(info[[i]]),names(x))
    label[[i]]<-label[[i]][!is.na(label[[i]])]
  }


  legend<-NULL
  for (i in 1:length(label)) {
    legend[label[[i]]] <- names(info)[i]
  }


  colour<-NULL

  for (i in 1:length(label)) {
    colour[label[[i]]]<-col[i]
  }

  pcha<-NULL

  for (i in 1:length(label)) {
    pcha[label[[i]]]<-pch[i]
  }


  par(mar = c(5,5,4,2))
  plot(
    x,
    y,
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    xlab = paste("PC1:", pc1),
    ylab = paste("PC2:", pc2),
    cex = cex,
    cex.lab = cex.lab,
    cex.axis = cex.axis,
    cex.main = cex.axis,
    col = colour,
    pch = pcha,
    main = "PCA score"
  )

  abline(v=0,lty=2)
  abline(h=0,lty=2)

  lines(ellipse::ellipse(0,scale=c(sd(x),sd(y)),centre=c(mean(x),mean(y))),lty=2)

  legend("topleft",
         names(info),
         pch=pch[1:length(info)],col=colour[1:length(info)],
         bty="n",cex = cex.axis,
         pt.cex = cex)
}





zeroRatioPlot <- function(zero.ratio,
                          zero.ratio.tol = 50,
                          cex = 1,
                          cex.axis = 1.5,
                          cex.lab = 1.8,
                          cex.main = 1.5,
                          col1 = "black",
                          col2 = "tomato"){
  colour <- rep(col1, length(zero.ratio))
  idx <- which(zero.ratio > zero.ratio.tol)
  if(length(idx) > 0){
    colour[idx] <- col2
  }
  par(mar = c(5,5,4,2))
  plot(zero.ratio, xlab = "Samples",
       ylab = "Zero value ratio in amples",
       cex = cex,
       cex.axis = cex.axis,
       cex.lab = cex.lab,
       cex.main = cex.main,
       col1 = col1,
       col2 = col2,
       pch = 19
  )

  abline(h = zero.ratio.tol, lty = 2, lwd = 1)
}




#####box plot for each sample
###single peak plot for data normalization
pcaOutlierPlot <- function(pca.object,
                           outlier.idx,
                           alpha = 0.5,
                           size = 2,
                           text = FALSE,
                           title = ""){
  loading <- summary(pca.object)$rotation
  pov <- summary(pca.object)$importance[2,]
  sd <- summary(pca.object)$importance[1,]
  cp <- summary(pca.object)$importance[3,]
  pc <- pca.object$x

  pc1 <- round(pov[1], 2)
  pc2 <- round(pov[2], 2)
  pc3 <- round(pov[3], 2)

  x <- pc[, 1]
  y <- pc[, 2]
  z <- pc[, 3]

  group <- rep(NA, length(x))
  group[outlier.idx] <- "Outlier samples"
  group[is.na(group)] <- "No outlier samples"

  temp <- data.frame(names(x), x, y, z, group, stringsAsFactors = FALSE)

  colnames(temp) <- c("Sample", "PC1", "PC2", "PC3", "Group")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = PC1, y = PC2,
                                              color = Group,
                                              label = Sample)) +
    ggplot2::geom_point(alpha = alpha, size = size) +
    ggplot2::labs(x = "PC1",
                  y = "PC2") +
    ggplot2::ggtitle(title) +
    my.theme+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::geom_vline(xintercept = 0)+
    scale_color_manual(values = c("#00BFC4","#F8766D"))
    # ggplot2::stat_ellipse(mapping = ggplot2::aes(x = PC1, y = PC2), data = temp)
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
  return(plot)
}
















#####box plot for each sample
###single peak plot for data normalization
DCosZeroRatioPlot <- function(object,
                                 outlier.idx,
                                 zero.ratio.tol = 50,
                                 sample.info,
                                 alpha = 0.5,
                                 size = 2,
                                 text = FALSE,
                                 title = ""){

  group <- rep(NA, length(object))
  if(length(outlier.idx) > 0){
    group[outlier.idx] <- "Outlier samples"
    group[is.na(group)] <- "No outlier samples"
  }else{
    group[is.na(group)] <- "No outlier samples"
  }

  if(any(colnames(sample.info) == "injection.order")){
    injection.order <- sample.info[,"injection.order"]
  }else{
    injection.order <- 1:length(object)
  }

  temp <- data.frame(names(object), injection.order, object, group, stringsAsFactors = FALSE)

  colnames(temp) <- c("Sample", "Injection.order", "Zero.ratio", "Group")
  # temp$Zero.ratio <- temp$Zero.ratio * 100
  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Injection.order,
                                              y = Zero.ratio,
                                              color = Group,
                                              label = Sample)) +
    ggplot2::geom_point(alpha = alpha, size = size) +
    ggplot2::labs(x = "Samples",
                  y = "Zero value ratio (%)") +
    ggplot2::ggtitle(title) +
    my.theme+
    scale_color_manual(values = c("#00BFC4","#F8766D"))
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))


  return(plot)
}







###single peak plot for data normalization
osSingleSample <- function(sample,  sample.info,
                           sample.idx,
                           # col = "dodgerblue",
                           alpha = 0.5, size = 2,
                           title = ""){
  sample <- as.data.frame(sample)

  sample.info <- as.data.frame(sample.info)


  x <- as.numeric(sample[sample.idx,])

  group <- sample.info[,"group"]
  sample.name <- colnames(sample)

  temp <- data.frame(sample.name, x, group, stringsAsFactors = FALSE)

  colnames(temp) <- c("Sample", "Intensity", "Group")
  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Group, y = Intensity)) +
    # ggplot2::geom_point(alpha = alpha,
    #                     # colour = col,
    #                     size = size,
    #                     ggplot2::aes(colour = Group)) +
    ggplot2::geom_boxplot(ggplot2::aes(color = Group
                                       # fill = Group,
                                       # label = Sample
    ), data = temp)+
    ggplot2::labs(
      # x = "Injection order",
      y = "Intensity") +
    ggplot2::ggtitle(title) +
    my.theme +
    geom_dotplot(binaxis = 'y', stackdir='center',
                 position = position_dodge(1),
                 ggplot2::aes(fill = Group, col = Group),
                 dotsize = 0.5
    )

  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))


  return(plot)
}


