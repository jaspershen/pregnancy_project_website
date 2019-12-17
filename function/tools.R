

###function for RT correaction plot
plotrt <- function(object,
                   col = NULL,
                   ty = NULL,
                   leg = TRUE,
                   densplit = FALSE){
  samples <- sampnames(object)
  classlabel <- as.vector(unclass(sampclass(object)))
  n <- length(samples)
  rtuncor <- object@rt$raw
  rtcor <- object@rt$corrected

  ## define the colors and line types and returns a list of
  ## mypal, col and ty. Uses the original code if no colors are
  ## submitted. Supports manually selected colors (e.g. in hex)
  vals <- defineColAndTy(col, ty, classlabel)
  col <- vals$col
  mypal <- vals$mypal
  ty <- vals$ty

  rtdevsmo <- vector("list", n)

  for (i in 1:n)
    rtdevsmo[[i]] <- rtuncor[[i]] - rtcor[[i]]

  rtrange <- range(do.call(c, rtuncor))
  devrange <- range(do.call(c, rtdevsmo))

  plot(0, 0, type = "n", xlim = rtrange, ylim = devrange,
       main = "Retention Time Deviation vs. Retention Time",
       xlab = "Retention Time",
       ylab = "Retention Time Deviation")
  if (leg)
    legend(rtrange[2], devrange[2], samples, col = mypal[col], lty = ty,
           pch = ceiling(1:n/length(mypal)), xjust = 1)

  for (i in 1:n)
    points(rtuncor[[i]], rtdevsmo[[i]], type="l",
           col = mypal[col[i]],
           lty = ty[i]
    )

}


## defines the colors and ltys to be used in the plot functions...
## the code combines original code taken from the plot functions and
## adds the possibility to sumbmit colors.
## col: colors to be used for the samples
## ty: lty to be used for the samples
## classlabels: factor with class labels (group definitions), length
##              being equal to the number of samples.
defineColAndTy <- function(col=NULL, ty=NULL, classlabel){
  if(missing(classlabel))
    stop("classlabel is required")
  n <- length(classlabel)
  if(!is.factor(classlabel))
    classlabel <- factor(classlabel)
  ## want to transform the class labels to integer values, i.e. skip the levels
  classlabel <- as.numeric(classlabel)
  if(is.null(col)) {
    col <- integer(n)
    for (i in 1:max(classlabel))
      col[classlabel == i] <- 1:sum(classlabel == i)
  }else{
    ## check if we do have the same number of colors than samples
    if(length(col) != n){
      warning("Less colors than samples! Using the first color for all samples.")
      col <- rep(col[1], n)
    }
  }
  if(is.null(ty)) {
    ## allow col being not just integers...
    col.int <- as.numeric(factor(col))
    ty <- integer(n)
    for (i in 1:max(col.int))
      ty[col.int == i] <- 1:sum(col.int == i)
  }else{
    if(length(ty) != n){
      warning("Less line types than samples! Using the first type for all samples.")
      ty <- rep(ty[1], n)
    }
  }
  ## if col is a character vector (e.g. colors defined by RColorBrewer)
  if(!is.numeric(col)){
    ## define the mypal... that's the color vector as used below.
    mypal <- col
    ## col is now a numeric vector
    col <- 1:length(mypal)
  }else{
    if (length(palette()) < max(col))
      mypal <- rainbow(max(col), end = 0.85)
    else
      mypal <- palette()[1:max(col)]
  }
  return(list(col=col, ty=ty, mypal=mypal ))
}


##function for peak profile
setGeneric(name = "dataInformation",
           def = function(object){

             intensity <- apply(object[,-c(1:3)], 1, function(x) {mean(x, na.rm = TRUE)})
             mz <- as.numeric(object[,2])
             rt <- as.numeric(object[,3])
             peak.name <- as.character(object[,1])

             # my.theme <- ggplot2::theme_bw()+
             #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
             #                  axis.title.y = ggplot2::element_text(size = 18)) +
             #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
             #                  axis.text.y = ggplot2::element_text(size = 15)) +
             #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
             #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

             data <- data.frame(peak.name, mz, rt, intensity, stringsAsFactors = FALSE)



             int.log <- log(intensity + 1, 10)
             rt.mz.int <- data.frame(rt, mz, int.log, stringsAsFactors = FALSE)

             peak.profile <-
               ggplot2::ggplot(data = rt.mz.int,
                               ggplot2::aes(x = rt, y = mz, colour = int.log)) +
               ggplot2::geom_point(alpha = 0.5) +
               ggplot2::scale_color_gradient(low = "royalblue", high = "firebrick1") +
               # ggplot2::scale_color_gradient() +
               ggplot2::labs(x = "Retention time",
                             y = "Mass to charge ratio (m/z)",
                             colour = "log10(intensity)") +
               ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20))+
               my.theme+ggplot2::ggtitle("Peak profile")
             return(peak.profile)
           })



setGeneric(name = "annotateMS2",
           def = function(spec, matched.spec,
                          ppm.ms2match = 30){
             note <- rep(NA, nrow(spec))
             for(i in 1:length(note)){
               note[i] <-
                 ifelse(any(abs(as.numeric(spec[i,1]) - as.numeric(matched.spec[,1]))*10^6/ifelse(as.numeric(spec[i,1])>=400, as.numeric(spec[i,1]), 400) < ppm.ms2match), "matched", "no")
             }
             note
           })



setGeneric(name = "ms2Plot2", def = function(spectrum1,
                                            spectrum2,
                                            col1 = "dodgerblue",
                                            col2 = "firebrick1",
                                            col3 = "grey",
                                            xlim = NULL,
                                            lwd = 2,
                                            cex.lab = 1.8,
                                            cex.axis = 1.5,
                                            xlab = "m/z",
                                            ylab = "Relative intensity",
                                            real.int.cutoff = 0,
                                            ppm.ms2match = 30,
                                            bty = "o",
                                            xaxs = "i",
                                            yaxs = "i",
                                            ...){
  if(missing(spectrum2)){
    spectrum1 <- as.data.frame(spectrum1)
    spectrum1[,2] <- as.numeric(spectrum1[,2])/max(as.numeric(spectrum1[,2]))
    spectrum1 <- spectrum1[spectrum1[,2] > real.int.cutoff,]

    mz <- as.numeric(spectrum1[,1])
    int <- as.numeric(spectrum1[,2])

    par(xpd = FALSE)
    if(is.null(xlim)){
      plot(mz, int, type = "h", lwd = lwd, col = col1,
           cex.lab = cex.lab, cex.axis = cex.axis,
           xlab = xlab, ylab = ylab,...)
    }else{
      plot(mz, int, type = "h", lwd = lwd, col = col1,
           cex.lab = cex.lab, cex.axis = cex.axis, xlim = xlim,
           xlab = xlab, ylab = ylab,...)
    }


  }else{
    spectrum1 <- as.data.frame(spectrum1)
    spectrum1[,2] <- as.numeric(spectrum1[,2])/max(as.numeric(spectrum1[,2]))
    spectrum1 <- spectrum1[spectrum1[,2] > real.int.cutoff,]

    spectrum2 <- as.data.frame(spectrum2)
    spectrum2[,2] <- as.numeric(spectrum2[,2])/max(as.numeric(spectrum2[,2]))
    spectrum2 <- spectrum2[spectrum2[,2] > real.int.cutoff,]


    note <- annotateMS2(spec = spectrum1, matched.spec = spectrum2,
                        ppm.ms2match = ppm.ms2match)
    color1 <- rep(col3, length(note))
    color1[note=="matched"] <- col1
    spectrum1 <- data.frame(spectrum1, note, stringsAsFactors = FALSE)

    note <- annotateMS2(spec = spectrum2, matched.spec = spectrum1,
                        ppm.ms2match = ppm.ms2match)
    color2 <- rep(col3, length(note))
    color2[note=="matched"] <- col2
    spectrum2 <- data.frame(spectrum2, note, stringsAsFactors = FALSE)

    if(is.null(xlim)){
      xlim <- c(min(c(spectrum1[,1],spectrum2[,1])),max(c(spectrum1[,1],spectrum2[,1])))
    }

    plot(0, col = "white", xlab = xlab, ylab=ylab,
         ylim = c(-1,1),
         xlim = xlim,
         cex.lab =cex.lab, cex.axis = cex.axis, bty = bty,
         # xaxs = xaxs,
         # yaxs = yaxs,
         ...)

    abline(h = 0, lwd = 1.5)

    points(x = spectrum1[,1], spectrum1[,2], type = "h", col = color1, lwd = lwd)
    points(x = spectrum2[,1], -spectrum2[,2], type = "h", col = color2, lwd = lwd)


  }
})







####data check function
check.ms1 <- function(peak.table){
sample.number <- ncol(peak.table)
peak.number <- nrow(peak.table)
return(paste("There are ", sample.number, " samples and ", peak.number, " peaks.", sep = ""))
}

check.sample.info <- function(sample.info){
  temp <- as.data.frame(table(sample.info[,2]))
  colnames(temp) <- c("Group", "Sample number")
  temp
}

check.ms2 <- function(ms2){
  if(is.null(ms2)) return("No ms2 data")
  ms2.num <- paste("There are ", length(ms2), " MS2 data.", sep = "")
  ms2.size <- lapply(ms2, function(x) pryr::object_size(x))
  ms2.size <- unlist(lapply(ms2.size, function(x){
    round(as.numeric(x)/1000^2, 2)
  }))

  ms2.size <- paste("Total size is", sum(ms2.size), "M.")
  paste(ms2.num, ms2.size)
  }



###dnBoxplot
dnBoxplot <- function(sample, main){
  sample <- t(apply(sample, 1, function(x){
    (x-mean(x))/sd(x)
  }))
  boxplot(sample, xlab = "Samples", ylab = "Peak intensity (z-score)",
          cex.lab = 1.8, cex.axis = 1.5, main = main,cex.main = 1.8)

}



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

  # if (output.plot) {
  #   colour <- rep(NA, length(x))
  #   colour[outlier.index] <- "firebrick1"
  #   colour[is.na(colour)] <- "black"
  #   pdf(file.path(path, paste(plot.name, ".pdf", sep = "")),
  #       width = 7,
  #       height = 7)
  #   par(mar = c(5,5,4,2))
  #   plot(
  #     x,
  #     y,
  #     xlim = c(xmin, xmax),
  #     ylim = c(ymin, ymax),
  #     xlab = paste("PC1:", pc1),
  #     ylab = paste("PC2:", pc2),
  #     cex.lab = 1.3,
  #     cex.axis = 1.3,
  #     pch = 19,
  #     col = colour,
  #     main = "PCA score plot for outliers"
  #   )
  #   if (length(outlier.index) > 0) {
  #     text(x = x[outlier.index],
  #          y = y[outlier.index],
  #          names(x)[outlier.index],
  #          pos = 4)
  #   } else
  #     (text(x = x,
  #           y = y,
  #           names(x),
  #           pos = 4))
  #   abline(h = 0, lty = 2)
  #   abline(v = 0, lty = 2)
  #   lines(data.for.plot, lty = 2)
  #   dev.off()
  # }
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
       ylab = "Zero value ratios in samples",
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



####heatmap function
setGeneric(name = "heatMap",
           def = function(sample,
                          sample.info,
                          group,
                          color = c("dodgerblue", "firebrick1", "orange",
                                    "springgreen2", "orchid"),
                          int.col = c("navy", "white", "firebrick"),
                          show_rownames = TRUE,
                          show_colnames = FALSE,
                          border_color = NA,
                          fontsize = 10,
                          fontsize_row = 10,
                          fontsize_col = 10,
                          cluster_rows = TRUE,
                          cluster_cols = TRUE,
                          clustering_distance_rows = "euclidean",
                          clustering_distance_cols = "euclidean",
                          clustering_method = "complete",
                          display_numbers = FALSE,
                          legend = TRUE
           ){
             sample.info <- as.data.frame(sample.info)

             # group.idx <- lapply(group, function(x){
             #   which(sample.info$group == x)
             # })

             sample.range <- abs(range(sample))
             dif <- sample.range[1] - sample.range[2]
             if (dif < 0) {
               sample[sample > sample.range[1]] <- sample.range[1]
             }
             if (dif > 0) {
               sample[sample < -1 * sample.range[2]] <- -1 * sample.range[2]
             }

             annotation_col <- data.frame(Group = c(sample.info[,"group"]), stringsAsFactors = FALSE)


             rownames(annotation_col) <- sample.info[,1]


             # Specify colors
             # ann_col <- NULL
             # for (i in seq_along(group)) {
             #   ann_col[i] <- color[i]
             # }

             # ann_col <- c("green", "red")

             # ann_colors = list(group = c("green", "red"))

             ann_colors = list(Group = color[1:length(group)])

             names(ann_colors[[1]]) <- group

             temp <- pheatmap::pheatmap(mat = sample,
                                        color = colorRampPalette(int.col)(50),
                                annotation_col = annotation_col,
                                annotation_colors = ann_colors,
                                show_rownames = show_rownames,
                                show_colnames = show_colnames,
                                border_color = border_color,
                                fontsize = fontsize,
                                fontsize_row = fontsize_row,
                                fontsize_col = fontsize_col,
                                cluster_rows = cluster_rows,
                                cluster_cols = cluster_cols,
                                clustering_distance_rows = clustering_distance_rows,
                                clustering_distance_cols = clustering_distance_cols,
                                clustering_method = clustering_method,
                                display_numbers = display_numbers,
                                legend = legend, silent = TRUE)
             temp
           })



color.list <- c("dodgerblue", "firebrick1", "orange",
                  "springgreen2", "orchid")



SXTdummy <- function(Y) {
  dummy <- matrix(0, nrow = length(Y), ncol = length(table(Y)))
  for (i in 1:length(Y)) {
    for (j in 1:ncol(dummy)) {
      if (Y[i] == names(table(Y))[j])
        dummy[i, j] = 1
    }
  }
  return(dummy)
}




###PLS Q2 cum barplot
setGeneric(name = "plsQ2barplot",
           def = function(pls.object,
                          comps.number = 10){
             Q2cum <- pls.object$Q2[,5]
             Q2cum[is.nan(Q2cum)] <- 1

             if(is.null(comps.number)){
               if(length(Q2cum) > 10){
                 comps.number <- 10
               }
             }else{
             if(comps.number > length(Q2cum)){
               comps.number <- length(Q2cum)
             }
             }
             par(mar = c(5,5,4,2))
             par(xpd = TRUE)

             Q2cum <- data.frame(names(Q2cum), Q2cum, stringsAsFactors = FALSE)
             colnames(Q2cum) <- c("ncomp","Q2cum")
             Q2cum$ncomp <- factor(Q2cum$ncomp, levels = Q2cum$ncomp)


             # my.theme <- ggplot2::theme_bw()+
             #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
             #                  axis.title.y = ggplot2::element_text(size = 18)) +
             #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
             #                  axis.text.y = ggplot2::element_text(size = 15)) +
             #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
             #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

             if(nrow(Q2cum) < comps.number) comps.number <- nrow(Q2cum)

             plot <- ggplot2::ggplot(data = Q2cum[c(1:comps.number),],
                             aes(x = ncomp, y = Q2cum))+
               ggplot2::geom_bar(stat = "identity", colour = "dodgerblue", fill = "dodgerblue")+
               my.theme

             plot
})



###PLS Q2 R2 barplot for plsreg1
setGeneric(name = "plsQ2R2barplot",
           def = function(pls.object){

             Q2cum <- pls.object$Q2[,5]
             Q2cum[is.nan(Q2cum)] <- 1

             R2cum <- cumsum(pls.object$R2)


             Value <- c(R2cum, Q2cum)
             Type <- c(rep("R2cum", length(R2cum)), rep("Q2cum", length(Q2cum)))

             # colnames(Q2R2) <- c("ncomp", "R2cum","Q2cum")
             ncomp <- as.character(rep(c(1:length(R2cum)), 2))

             Q2R2 <- data.frame(ncomp, Value, Type, stringsAsFactors = FALSE)

             # my.theme <- ggplot2::theme_bw()+
             #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
             #                  axis.title.y = ggplot2::element_text(size = 18)) +
             #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
             #                  axis.text.y = ggplot2::element_text(size = 15)) +
             #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
             #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

             plot <- ggplot2::ggplot(data = Q2R2, aes(x = ncomp, y = Value))+
               geom_bar(stat = "identity", aes(fill = Type), position = "dodge")+
               my.theme
           })

###PLS Q2 R2 barplot for plsreg2
# setGeneric(name = "plsQ2R2barplot2",
#            def = function(pls.object){
#
#              Q2cum <- pls.object$Q2cum[,3]
#              Q2cum[is.nan(Q2cum)] <- 1
#
#              R2cum <- cumsum(pls.object$R2)
#
#
#              Value <- c(R2cum, Q2cum)
#              Type <- c(rep("R2cum", length(R2cum)), rep("Q2cum", length(Q2cum)))
#
#              # colnames(Q2R2) <- c("ncomp", "R2cum","Q2cum")
#              ncomp <- as.character(rep(c(1:length(R2cum)), 2))
#
#              Q2R2 <- data.frame(ncomp, Value, Type, stringsAsFactors = FALSE)
#
#              my.theme <- ggplot2::theme_bw()+
#                ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
#                               axis.title.y = ggplot2::element_text(size = 18)) +
#                ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
#                               axis.text.y = ggplot2::element_text(size = 15)) +
#                ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
#                ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
#
#              plot <- ggplot2::ggplot(data = Q2R2, aes(x = ncomp, y = Value))+
#                geom_bar(stat = "identity", aes(fill = Type), position = "dodge")+
#                my.theme
#            })

###PLS score plot
plsScorePlot <- function(pls.object,
                          sample.info){
  if(is.null(pls.object)) return(NULL)
  #group.info has tow column, column 1 is sample name, column is group
  x <- pls.object$x.scores[,1]
  y <- pls.object$x.scores[,2]

  # pc <- pc[order(rownames(pc)),]


  data <- data.frame(names(x), x, y, stringsAsFactors = FALSE)
  colnames(data) <- c("sample.name", "t1", "t2")
  data <- data[order(data$sample.name),]
  sample.info <- sample.info[order(sample.info$sample.name),]

  data <- data.frame(data, sample.info[,"group"], stringsAsFactors = FALSE)
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
                    ggplot2::aes(x = t1, y = t2,
                                 colour = Group,
                                 label = sample.name)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::labs(x = "t[1]",
                  y = "t[2]"
                  # colour = "Batch"
    ) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20))+
    my.theme+ggplot2::ggtitle("PLS score plot")+
    ggplot2::geom_hline(yintercept = 0)+
    ggplot2::geom_vline(xintercept = 0)

  score.plot
}



# plsPlot <- function(pls.object,
#                      sample.info,
#                      cex = 1,
#                      cex.axis = 1.5,
#                      cex.lab = 1.8,
#                      cex.main = 1.5,
#                      col = c("dodgerblue", "firebrick1", "orange",
#                              "springgreen2", "orchid"),
#                      pch = c(19, 17, 15, 3, 4),
#                      ellipse = TRUE,
#                      ci = 95){
#
#   x <- pls.object$x.scores[,1]
#   y <- pls.object$x.scores[,2]
#
#   xmin <- 1.2 * min(x)
#   xmax <- 1.2 * max(x)
#   ymin <- 1.2 * min(y)
#   ymax <- 1.2 * max(y)
#
#
#
#   info <- lapply(unique(sample.info[,"group"]), function(x){
#     sample.info[,1][which(sample.info[,"group"] == x)]
#   })
#
#   names(info) <- unique(sample.info[,"group"])
#
#   label <- list()
#   for (i in 1:length(info)) {
#     label[[i]]<-match(as.character(info[[i]]),names(x))
#     label[[i]]<-label[[i]][!is.na(label[[i]])]
#   }
#
#
#
#
#
#
#   par(mar = c(5,5,4,2))
#   plot(
#     x,
#     y,
#     xlim = c(xmin, xmax),
#     ylim = c(ymin, ymax),
#     xlab = "t[1]",
#     ylab = "t[2]",
#     cex = cex,
#     cex.lab = cex.lab,
#     cex.axis = cex.axis,
#     cex.main = cex.axis,
#     col = colour,
#     pch = pcha,
#     main = "PLS score plot",
#     bg = NA
#   )
#
#   abline(v=0,lty=2)
#   abline(h=0,lty=2)
#
#   lines(ellipse::ellipse(0,scale=c(sd(x),sd(y)),centre=c(mean(x),mean(y))),lty=2)
#
#   legend("topleft",
#          names(info),
#          pch=pch[1:length(info)],col=colour[1:length(info)],
#          bty="n",cex = cex.axis,
#          pt.cex = cex)
# }





# title SXTMTmatch
# description Match two data according to mz and RT.
# author Xiaotao Shen
# \email{shenxt@@sioc.ac.cn}
# param data1 First data for matching, first column must be mz
# and seconod column must be rt.
# param data2 Second data for matching, first column must be mz
# and seconod column must be rt.
# param mz.tol mz tol for ms1 and ms2 data matching.
# param rt.tol RT tol for ms1 and ms2 data matching.
# return Return a result which give the matching result of data1 and database.
# export

setGeneric(name = "SXTMTmatch",
           def = function(data1,
                          data2,
                          mz.tol,
                          #rt.tol is relative
                          rt.tol = 30,
                          rt.error.type = c("relative", "abs")){
             rt.error.type <- match.arg(rt.error.type)
             #
             if (nrow(data1) == 0 | nrow(data2) == 0) {
               result <- NULL
               return(result)
             }
             # mz1 <- as.numeric(data1[, 1])
             # rt1 <- as.numeric(data1[, 2])
             info1 <- data1[,c(1,2)]
             info1 <- apply(info1, 1, list)

             mz2 <- as.numeric(data2[, 1])
             rt2 <- as.numeric(data2[, 2])

             result <- pbapply::pblapply(info1, function(x) {
               temp.mz1 <- x[[1]][[1]]
               temp.rt1 <- x[[1]][[2]]
               mz.error <- abs(temp.mz1 - mz2) * 10 ^ 6 / temp.mz1
               if(rt.error.type == "relative"){
                 rt.error <- abs(temp.rt1 - rt2) * 100 / temp.rt1
               }else{
                 rt.error <- abs(temp.rt1 - rt2)
               }

               j <- which(mz.error <= mz.tol & rt.error <= rt.tol)
               if(length(j) == 0){
                 matrix(NA, ncol = 7)
               }else{
                 cbind(j, temp.mz1, mz2[j], mz.error[j], temp.rt1, rt2[j], rt.error[j])
               }
             })

             if(length(result) == 1){
               result <- cbind(1,result[[1]])
             }else{
               result <- mapply(function(x,y){list(cbind(x,y))},
                                x <- 1:length(info1),
                                y = result)
               result <- do.call(rbind, result)
             }

             result <- matrix(result[which(!apply(result,1,function(x) any(is.na(x)))),], ncol = 8)
             if(nrow(result) == 0) return(NULL)
             colnames(result) <-
               c("Index1",
                 "Index2",
                 "mz1",
                 "mz2",
                 "mz error",
                 "rt1",
                 "rt2",
                 "rt error")
             result <- result
           })






setGeneric(name = "MRImatch",
           def = function(data1,
                          data2,
                          mz.tol,
                          #rt.tol is relative
                          rt.tol = 30,
                          rt.error.type = c("relative", "abs"),
                          int.tol = 1){
             rt.error.type <- match.arg(rt.error.type)
             #
             if (nrow(data1) == 0 | nrow(data2) == 0) {
               result <- NULL
               return(result)
             }
             # mz1 <- as.numeric(data1[, 1])
             # rt1 <- as.numeric(data1[, 2])
             info1 <- data1[,c(1,2,3)]
             info1 <- apply(info1, 1, list)

             mz2 <- as.numeric(data2[, 1])
             rt2 <- as.numeric(data2[, 2])
             int2 <- as.numeric(data2[, 3])

             result <- pbapply::pblapply(info1, function(x) {
               temp.mz1 <- x[[1]][[1]]
               temp.rt1 <- x[[1]][[2]]
               temp.int1 <- x[[1]][[3]]
               mz.error <- abs(temp.mz1 - mz2) * 10 ^ 6 / temp.mz1
               if(rt.error.type == "relative"){
                 rt.error <- abs(temp.rt1 - rt2) * 100 / temp.rt1
               }else{
                 rt.error <- abs(temp.rt1 - rt2)
               }

               int.error <- abs(temp.int1 - int2)

               j <- which(mz.error <= mz.tol & rt.error <= rt.tol & int.error <= int.tol)
               if(length(j) == 0){
                 matrix(NA, ncol = 10)
               }else{
                 cbind(j, temp.mz1, mz2[j], mz.error[j], temp.rt1, rt2[j],
                       rt.error[j], temp.int1, int2[j], int.error[j])
               }
             })

             if(length(result) == 1){
               result <- cbind(1,result[[1]])
             }else{
               result <- mapply(function(x,y){list(cbind(x,y))},
                                x <- 1:length(info1),
                                y = result)
               result <- do.call(rbind, result)
             }

             result <- matrix(result[which(!apply(result,1,function(x) any(is.na(x)))),], ncol = 11)
             if(nrow(result) == 0) return(NULL)
             colnames(result) <-
               c("Index1",
                 "Index2",
                 "mz1",
                 "mz2",
                 "mz.error",
                 "rt1",
                 "rt2",
                 "rt.error",
                 "int1",
                 "int2",
                 "int.error")
             result <- result
           })




############
mvPeakPlot <- function(data,
                       col = "dodgerblue",
                       alpha = 0.5,
                       size = 2, text = FALSE){
  tags <- data[,c(1:3)]
  sample <- data[,-c(1:3)]
  sample <- as.data.frame(sample)
  tags <- as.data.frame(tags)
  peak.mv.ratio <- apply(sample,1, function(x){
    sum(is.na(x))*100/ncol(sample)
  })

  temp <- data.frame(tags[,1], peak.mv.ratio,
                     c(1:length(peak.mv.ratio)),stringsAsFactors = FALSE)
  colnames(temp) <- c("Peak", "MV.ratio", "Index")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = MV.ratio,
                                              label = Peak)) +
    ggplot2::geom_point(alpha = alpha, colour = col, size = size) +
    ggplot2::labs(x = "Peaks",
                  y = "Missing value ratio in peaks (%)") +
    ggplot2::ggtitle("Missing value ratios in peaks") +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
  # ggplot2::coord_cartesian(xlim = xlim,
  # ylim = ylim, expand = TRUE)
  if(text){
    plot <- plot+ggplot2::geom_text(aes(x = Index, y = MV.ratio, label = Peak),
                                    check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
  }
  return(plot)
}



############
mvSamplePlot <- function(data,
                       col = "dodgerblue",
                       alpha = 0.5,
                       size = 2, text = FALSE){
  tags <- data[,c(1:3)]
  sample <- data[,-c(1:3)]
  sample <- as.data.frame(sample)
  tags <- as.data.frame(tags)
  sample.mv.ratio <- apply(sample,2, function(x){
    sum(is.na(x))*100/nrow(sample)
  })

  temp <- data.frame(colnames(sample), sample.mv.ratio,
                     c(1:length(sample.mv.ratio)),stringsAsFactors = FALSE)
  colnames(temp) <- c("Sample", "MV.ratio", "Index")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = MV.ratio,
                                              label = Sample)) +
    ggplot2::geom_point(alpha = alpha, colour = col, size = size) +
    ggplot2::labs(x = "Samples",
                  y = "Missing value ratio in Samples (%)") +
    ggplot2::ggtitle("MV ratio in Samples") +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
  # ggplot2::coord_cartesian(xlim = xlim,
  # ylim = ylim, expand = TRUE)
  if(text){
    plot <- plot+ggplot2::geom_text(aes(x = Index, y = MV.ratio, label = Sample),
                                    check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
  }
  return(plot)
}




############
zeroPeakPlot <- function(data,
                       col = "dodgerblue",
                       alpha = 0.5,
                       size = 2, text = FALSE){
  tags <- data[,c(1:3)]
  sample <- data[,-c(1:3)]
  sample <- as.data.frame(sample)
  tags <- as.data.frame(tags)
  peak.zero.ratio <- apply(sample,1, function(x){
    x[is.na(x)] <- 1
    sum(x == 0)*100/ncol(sample)
  })

  temp <- data.frame(tags[,1], peak.zero.ratio,
                     c(1:length(peak.zero.ratio)),stringsAsFactors = FALSE)
  colnames(temp) <- c("Peak", "zero.ratio", "Index")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = zero.ratio,
                                              label = Peak)) +
    ggplot2::geom_point(alpha = alpha, colour = col, size = size) +
    ggplot2::labs(x = "Peaks",
                  y = "Zero value ratios in peaks (%)") +
    ggplot2::ggtitle("Zero value ratios in peaks") +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
  # ggplot2::coord_cartesian(xlim = xlim,
  # ylim = ylim, expand = TRUE)
  if(text){
    plot <- plot+ggplot2::geom_text(aes(x = Index, y = zero.ratio, label = Peak),
                                    check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
  }
  return(plot)
}



############
zeroSamplePlot <- function(data,
                         col = "dodgerblue",
                         alpha = 0.5,
                         size = 2, text = FALSE){
  tags <- data[,c(1:3)]
  sample <- data[,-c(1:3)]
  sample <- as.data.frame(sample)
  tags <- as.data.frame(tags)
  sample.zero.ratio <- apply(sample,2, function(x){
    x[is.na(x)] <- 1
    sum(x == 0)*100/nrow(sample)
  })

  temp <- data.frame(colnames(sample), sample.zero.ratio,
                     c(1:length(sample.zero.ratio)),stringsAsFactors = FALSE)
  colnames(temp) <- c("Sample", "zero.ratio", "Index")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = zero.ratio,
                                              label = Sample)) +
    ggplot2::geom_point(alpha = alpha, colour = col, size = size) +
    ggplot2::labs(x = "Samples",
                  y = "Zero value ratios in samples (%)") +
    ggplot2::ggtitle("Zero value ratios in samples") +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
  # ggplot2::coord_cartesian(xlim = xlim,
  # ylim = ylim, expand = TRUE)
  if(text){
    plot <- plot+ggplot2::geom_text(aes(x = Index, y = zero.ratio, label = Sample),
                                    check_overlap = TRUE, size = 3, hjust = 1, vjust = 1)
  }
  return(plot)
}





###RSD for different group
###single peak plot for data normalization
rsdDistribution <- function(data,
                            sample.info,
                            group,
                            # col = "dodgerblue",
                            rsd.tol = 30,
                            alpha = 0.5,
                            size = 2,
                            # text = FALSE,
                            title = ""){
  if(is.null(data) | is.null(sample.info)) return(NULL)
  sample <- data[,-c(1:3)]
  tags <- data[,c(1:3)]
  sample <- as.data.frame(sample)
  sample.info <- as.data.frame(sample.info)

  sample.info <- sample.info[sample.info[,"group"] %in% group,]
  sample <- sample[,match(sample.info[,1], colnames(sample))]

  rsd <- apply(sample, 1, function(x){
    sd(x, na.rm = TRUE)*100/mean(x, na.rm = TRUE)
  })

  rsd[is.na(rsd)] <- max(rsd, na.rm = TRUE)

  Group <- rep(NA, length(rsd))
  Group[rsd >= 30] <- ">=30"
  Group[rsd < 30] <- "<30"
  Group <- factor(x = Group, levels = c(">=30", "<30"))
  temp <- data.frame(1:length(rsd),tags[,"name"], rsd, Group, stringsAsFactors = FALSE)
  colnames(temp) <- c("Index","Peak", "RSD", "Group")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))


  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Index, y = RSD,
                                              label = Peak,
                                              color = Group)) +
    ggplot2::geom_point(alpha = alpha, size = size) +
    ggplot2::labs(x = "Peaks",
                  y = "RSD (%)", colour = "RSD value") +
    ggplot2::ggtitle(title) +
    my.theme +
    ggplot2::geom_hline(yintercept = rsd.tol)
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))

  num1 <- length(rsd)
  num2 <- sum(rsd < rsd.tol)
  text <- paste(num2, " out of ", num1, " peaks with RSDs less than ", rsd.tol, "%", sep = "")
  # plot <- plot+ggplot2::annotate(geom = "text", x = -Inf, y = Inf, vjust = 2, hjust = -0.2,
  #                                label = text)
  plot <- plot+
    ggplot2::ggtitle(label = text)
    # ggplot2::labs(fill = "RSD value")
    # scale_fill_manual("RSD value")
  return(plot)
}





sxtLog <- function(sample, method = c("no", "log2", "loge", "log10")){
  method <- match.arg(method)
  if(method == "no") sample <- sample
  if(method == "log2") sample <- log(sample + 0.00000001, 2)
  if(method == "loge") sample <- log(sample + 0.00000001)
  if(method == "log10") sample <- log(sample + 0.00000001, 10)
  sample

}


sxtScale <- function(sample,
                     method = c("no", "pareto", "auto"),
                     center = TRUE){
  method <- match.arg(method)

  if(method == 'pareto') {
    if(center == TRUE) sample <- t(apply(sample, 1, function(x) (x-mean(x))/sqrt(sd(x))))
    if(center == FALSE) sample <- t(apply(sample, 1, function(x) x/sqrt(sd(x))))
  }

  if(method == 'auto') {
    if(center == TRUE) sample <- t(apply(sample, 1, function(x) (x-mean(x))/sd(x)))
    if(center == FALSE) sample <- t(apply(sample, 1, function(x) x/sd(x)))
  }

  if(method == 'no') {
    sample <- sample
  }

  sample

}







#####QC intensity boxplot for QC samples

qcIntBoxplot <- function(data,
                         sample.info,
                         title = ""){
  data <- as.data.frame(data)
  sample.info <- as.data.frame(sample.info)
  tags <- data[,-match(sample.info$sample.name, colnames(data))]
  sample <- data[,match(sample.info$sample.name, colnames(data))]
  # tags <- data[,c(1:3)]
  sample <- as.data.frame(sample)
  sample.info <- as.data.frame(sample.info)
  if(all(sample.info[,"class"] != "QC")) return(NULL)

  if(any(colnames(sample.info) == "injection.order")){
    sample.info <- sample.info[order(sample.info$injection.order),]
  }


  ##get QC from samples
  sample.info <- sample.info[sample.info$class == "QC",]
  sample <- sample[,match(sample.info[,1], colnames(sample))]

  ##scale sample
  sample <- sxtScale(sample = sample, method = "auto")

  temp <- apply(sample, 2, list)
  temp <- lapply(temp, unlist)



  group <- sample.info[,"group"]
  if(all(colnames(sample.info) != "batch")){
    batch <- as.character(rep(1, nrow(sample.info)))
  }else{
    batch <- as.character(sample.info[,"batch"])
  }


  temp <- mapply(function(x,y,z, w){
    list(data.frame("Sample" = rep(x, nrow(sample)),
                    "Intensity" = y,
                    "Group" = rep(z, nrow(sample)),
                    "Batch" = rep(w, nrow(sample)),
                    stringsAsFactors = FALSE))
  },
  x = colnames(sample),
  y = temp,
  z = group,
  w = batch)

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
  plot <-  ggplot2::ggplot(temp, ggplot2::aes(x = Sample,
                                              y = Intensity,
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
                  y = "Intensity",
                  colour = "Batch") +
    ggplot2::ggtitle("QC sample intensity boxplot") +
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))


  return(plot)
}



###QC correlation plot
qcCorPlot <- function(data,
                      sample.info,
                      cor.method = c("pearson", "kendall",
                                           "spearman")){
if(all(sample.info$class != "QC")) return(NULL)
sample.info <- sample.info[sample.info$class == "QC",]
qc <- data[,match(sample.info$sample.name, colnames(data))]
qc <- log(qc+1, 10)

if(ncol(qc) > 100){
  qc <- qc[,1:100]
}
rm(list = c("data", "sample.info"))

qc.cor <- psych::cor2(x = qc)
colnames(qc.cor) <- rownames(qc.cor) <- colnames(qc)

# PerformanceAnalytics::chart.Correlation(R = qc,
#                                         histogram = TRUE,
#                                         method = cor.method)
# temp <- psych::pairs.panels(qc, scale=FALSE, ellipses = TRUE,
#              pch = 19, cex = 0.8, hist.col = "royalblue",
#              show.points = TRUE, cex.cor = 0.5, method = cor.method)
ggcorrplot(qc.cor, hc.order = FALSE,outline.col = "white")
}



qcCor <- function(data, sample.info,
                      cor.method = c("pearson", "kendall",
                                     "spearman")){
  if(all(sample.info$class != "QC")) return(NULL)
  sample.info <- sample.info[sample.info$class == "QC",]
  qc <- data[,match(sample.info$sample.name, colnames(data))]
  qc <- log(qc+1, 10)

  qc.cor <- psych::cor2(x = qc)
  colnames(qc.cor) <- rownames(qc.cor) <- colnames(qc)
  qc.cor
}








  ###volcanoplot
  volcanoPlot <- function(object,
                          control.group.name = "control",
                          case.group.name = "case",
                          p.adjust.method = "fdr",
                          p.cutoff = 0.05,
                          fc.cutoff = 1.3){
    p.value <- as.numeric(object[,3])
    fc <- as.numeric(object[,2])
  
    Marker <- rep(NA, length(p.value))
    Marker[which(p.value < p.cutoff & fc > fc.cutoff)] <- "Increase"
    Marker[which(p.value < p.cutoff & fc < 1/fc.cutoff)] <- "Decrease"
    Marker[is.na(Marker)] <- "No"
    marker.number <- sum(Marker != "No")
  
  
    p.value <- -log(p.value, 10)
    fc <- log(fc, 2)
  
    peak.name <- as.character(object[,1])
    temp <- data.frame(peak.name, fc, p.value, Marker, stringsAsFactors = FALSE)
  
  
    # my.theme <- ggplot2::theme_bw()+
    #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
    #                  axis.title.y = ggplot2::element_text(size = 18)) +
    #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
    #                  axis.text.y = ggplot2::element_text(size = 15)) +
    #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
    #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
  
    if(p.adjust.method == "no") ylab = "P-value (log10)"
    if(p.adjust.method == "fdr") ylab = "P-value (log10, FDR)"
    if(p.adjust.method == "holm") ylab = "P-value (log10, Holm)"
    if(p.adjust.method == "hochberg") ylab = "P-value (log10, Hochberg)"
    if(p.adjust.method == "hommel") ylab = "P-value (log10, Hommel)"
    if(p.adjust.method == "bonferroni") ylab = "P-value (log10, Bonferroni)"
    if(p.adjust.method == "BH") ylab = "P-value (log10, BH)"
    if(p.adjust.method == "BY") ylab = "P-value (log10, BY)"
  
    xlab <- paste("Fold change(log2,", case.group.name, "/",
                  control.group.name, ")",sep = "")
  
    par(mar = c(5,5,4,2))
    plot <- ggplot2::ggplot(data = temp, aes(x = fc, y = p.value, colour = Marker, label = peak.name))+
      ggplot2::geom_point()+
      my.theme+
      ggplot2::geom_hline(yintercept = -log(p.cutoff, 10))+
      ggplot2::geom_vline(xintercept = log(fc.cutoff, 2))+
      ggplot2::geom_vline(xintercept = -log(fc.cutoff, 2))+
      ggplot2::labs(x = xlab, y = ylab)+
      ggplot2::ggtitle(label = paste("Differential metabolite number:", marker.number))
      plot
  }


###3D volcanoplot
volcanoPlot3d <- function(object,
                          control.group.name = "control",
                          case.group.name = "case",
                          p.adjust.method = "fdr",
                          p.cutoff = 0.05,
                          fc.cutoff = 1.3,
                          vip.cutoff = 0){

  color <- rep(NA, nrow(object))
  temp.idx1 <- which(object[,3] < p.cutoff & object[,2] > fc.cutoff)
  temp.idx2 <- which(object[,3] < p.cutoff & object[,2] < 1/fc.cutoff)
  temp.idx3 <- which(object[,4] > vip.cutoff)
  temp.idx <- sort(unique(c(temp.idx1, temp.idx2)))
  temp.idx <- unique(intersect(temp.idx, temp.idx3))

  color[temp.idx] <- "Marker"
  color[is.na(color)] <- "No"
  color <- factor(color, levels = c("Marker", "No"))

  temp.data <- data.frame(object, color)
  temp.data$Fold.change <- log(temp.data$Fold.change, 2)
  temp.data$P.value <- -log(temp.data$P.value, 10)

  if(p.adjust.method == "no") ylab = "P-value (log10)"
  if(p.adjust.method == "fdr") ylab = "P-value (log10, FDR)"
  if(p.adjust.method == "holm") ylab = "P-value (log10, Holm)"
  if(p.adjust.method == "hochberg") ylab = "P-value (log10, Hochberg)"
  if(p.adjust.method == "hommel") ylab = "P-value (log10, Hommel)"
  if(p.adjust.method == "bonferroni") ylab = "P-value (log10, Bonferroni)"
  if(p.adjust.method == "BH") ylab = "P-value (log10, BH)"
  if(p.adjust.method == "BY") ylab = "P-value (log10, BY)"

  xlab <- paste("Fold change(log2,", case.group.name, "/",
                control.group.name, ")",sep = "")


  p <- plotly::plot_ly(temp.data, x = ~Fold.change, y = ~P.value,
                       z = ~VIP, color = ~color,
                       text = ~name,
                       colors = c('firebrick1', 'black')) %>%
    plotly::add_markers() %>%
    plotly::layout(scene = list(xaxis = list(title = xlab),
                        yaxis = list(title = ylab),
                        zaxis = list(title = 'VIP')))

  p

}




ROCplot <- function(roc.object){
if(is.null(roc.object)) return(NULL)
  sensitivities <- roc.object$sensitivities
  specificities <- roc.object$specificities

  temp.data <- data.frame(specificities, 1 - specificities, sensitivities, stringsAsFactors = FALSE)
  colnames(temp.data)[2] <- "specificities2"

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
  #                  axis.title.y = ggplot2::element_text(size = 18)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
  #                  axis.text.y = ggplot2::element_text(size = 15)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  auc <- paste("AUC:",round(roc.object$auc, 3),
               ";95%CI:", round(roc.object$ci[1], 3),"-",
               round(roc.object$ci[2], 3), sep = "")

  temp.idx <- which.max(sensitivities+specificities)
  sen <- round(sensitivities[temp.idx], 3)
  spe <- round(specificities[temp.idx], 3)
  cutoff <- round(roc.object$thresholds[temp.idx], 3)

  point <- paste("Cutoff:", cutoff, sep = "")



  plot <- ggplot2::ggplot(data = temp.data, aes(x = 1-specificities, y = sensitivities))+
    ggplot2::geom_path(size = 1) +
    my.theme +
    ggplot2::labs(x = "1-Specificity", y = "Sensitivity")+
    ggplot2::annotate(geom = "text", x = 0.5, y = 0.25,
                      label = auc, size = 5)+
    ggplot2::geom_point(aes(x=1-spe, y=sen), colour="firebrick1", size = 2)+
    ggplot2::annotate(geom = "text", x = 1 - spe, y = sen, vjust = 1, hjust = -0.2,
                      label = point, size = 5, colour = "firebrick1")

  return(plot)
}








#####differential metabolite discovery
DAuaParam <- function(
  da.ua.control,
  da.ua.case,
  daUAlog,
  # daUAscale,
  # daUAcenter,
  daUAfcWhich,
  daUAhypothesisTesting,
  daUAalternative,
  daUApaired,
  daUAadjust
){

  da.ua.param <- data.frame(
    "Parameter" = c("Control group",
                    "Case group",
                    "Logarithm method",
                    # "Scale method",
                    # "Center or not",
                    "Use what to calcuate fold change",
                    "Hypothesis testing method",
                    "Alternative",
                    "Paired",
                    "Correction method"),
    "Setting" = c(  da.ua.control,
                    da.ua.case,
                    daUAlog,
                    # daUAscale, daUAcenter,
                    daUAfcWhich,
                    daUAhypothesisTesting,
                    daUAalternative,
                    daUApaired, daUAadjust),
    stringsAsFactors = FALSE)

  if(da.ua.param[3,2] == "no") da.ua.param[3,2] <- "No log"
  if(da.ua.param[3,2] == "log2") da.ua.param[3,2] <- "Log 2"
  if(da.ua.param[3,2] == "loge") da.ua.param[3,2] <- "Log e"
  if(da.ua.param[3,2] == "log10") da.ua.param[3,2] <- "Log 10"

  # if(da.ua.param[4,2] == "pareto") da.ua.param[4,2] <- "Pareto scale"
  # if(da.ua.param[4,2] == "auto") da.ua.param[4,2] <- "Auto scale"
  # if(da.ua.param[4,2] == "no") da.ua.param[4,2] <- "No scale"

  if(da.ua.param[4,2] == "median") da.ua.param[4,2] <- "Median"
  if(da.ua.param[4,2] == "mean") da.ua.param[4,2] <- "Mean"

  if(da.ua.param[5,2] == "t") da.ua.param[5,2] <- "Student's t test"
  if(da.ua.param[5,2] == "w") da.ua.param[5,2] <- "Wilcoxon test"

  if(da.ua.param[6,2] == "two.sided") da.ua.param[6,2] <- "Two sided"
  if(da.ua.param[6,2] == "less") da.ua.param[6,2] <- "Less"
  if(da.ua.param[6,2] == "greater") da.ua.param[6,2] <- "Greater"

  if(da.ua.param[8,2] == "fdr") da.ua.param[8,2] <- "False discovery ratio (FDR)"
  if(da.ua.param[8,2] == "holm") da.ua.param[8,2] <- "Holm"
  if(da.ua.param[8,2] == "hochberg") da.ua.param[8,2] <- "Hochberg"
  if(da.ua.param[8,2] == "hommel") da.ua.param[8,2] <- "Hommel"
  if(da.ua.param[8,2] == "bonferroni") da.ua.param[8,2] <- "Bonferroni"
  if(da.ua.param[8,2] == "BH") da.ua.param[8,2] <- "BH"
  if(da.ua.param[8,2] == "BY") da.ua.param[8,2] <- "BY"
  if(da.ua.param[8,2] == "no") da.ua.param[8,2] <- "No correction"

  da.ua.param

}




DAmaParam <- function(
  da.ma.control,
  da.ma.case,
  daMAlog,
  daMAscale,
  daMAcenter,
  daMAncomp,
  daMAhcaClusteringDistanceRows,
  daMAhcaClusteringDistanceCols,
  daMAhcaClusteringMethod

){

  da.ma.param <- data.frame(
    "Parameter" = c("Control group", "Case group", "Logarithm method",
                    "Scale method", "Center or not",
                    "Number of component", "Distance measure used in clustering rows", "Distance measure used in clustering columns",
                    "Clustering method"),
    "Setting" = c(da.ma.control, da.ma.case, daMAlog,
                  daMAscale, daMAcenter,
                  daMAncomp, daMAhcaClusteringDistanceRows, daMAhcaClusteringDistanceCols,
                  daMAhcaClusteringMethod),
    stringsAsFactors = FALSE)

  if(da.ma.param[3,2] == "no") da.ma.param[3,2] <- "No log"
  if(da.ma.param[3,2] == "log2") da.ma.param[3,2] <- "Log 2"
  if(da.ma.param[3,2] == "loge") da.ma.param[3,2] <- "Log e"
  if(da.ma.param[3,2] == "log10") da.ma.param[3,2] <- "Log 10"

  if(da.ma.param[4,2] == "pareto") da.ma.param[4,2] <- "Pareto scale"
  if(da.ma.param[4,2] == "auto") da.ma.param[4,2] <- "Auto scale"
  if(da.ma.param[4,2] == "no") da.ma.param[4,2] <- "No scale"

  if(da.ma.param[7,2] == "euclidean") da.ma.param[7,2] <- "Euclidean"
  if(da.ma.param[7,2] == "maximum") da.ma.param[7,2] <- "Maximum"
  if(da.ma.param[7,2] == "manhattan") da.ma.param[7,2] <- "Manhattan"
  if(da.ma.param[7,2] == "canberra") da.ma.param[7,2] <- "Canberra"
  if(da.ma.param[7,2] == "binary") da.ma.param[7,2] <- "Binary"
  if(da.ma.param[7,2] == "minkowski") da.ma.param[7,2] <- "Minkowski"

  if(da.ma.param[8,2] == "euclidean") da.ma.param[8,2] <- "Euclidean"
  if(da.ma.param[8,2] == "maximum") da.ma.param[8,2] <- "Maximum"
  if(da.ma.param[8,2] == "manhattan") da.ma.param[8,2] <- "Manhattan"
  if(da.ma.param[8,2] == "canberra") da.ma.param[8,2] <- "Canberra"
  if(da.ma.param[8,2] == "binary") da.ma.param[8,2] <- "Binary"
  if(da.ma.param[8,2] == "minkowski") da.ma.param[8,2] <- "Minkowski"

  if(da.ma.param[9,2] == "ward.D") da.ma.param[9,2] <- "Ward.D"
  if(da.ma.param[9,2] == "ward.D2") da.ma.param[9,2] <- "Ward.D2"
  if(da.ma.param[9,2] == "single") da.ma.param[9,2] <- "Single"
  if(da.ma.param[9,2] == "complete") da.ma.param[9,2] <- "Complete"
  if(da.ma.param[9,2] == "average") da.ma.param[9,2] <- "Average"
  if(da.ma.param[9,2] == "mcquitty") da.ma.param[9,2] <- "Mcquitty"
  if(da.ma.param[9,2] == "median") da.ma.param[9,2] <- "Median"
  if(da.ma.param[9,2] == "centroid") da.ma.param[9,2] <- "Centroid"
  da.ma.param
}



DAvalidationParam <- function(
  daValidationPredictionModel
){

  da.validation.param <- data.frame(
    "Parameter" = c("Prediction model"),
    "Setting" = c(daValidationPredictionModel),
    stringsAsFactors = FALSE)

  if(da.validation.param[1,2] == "pls") da.validation.param[1,2] <- "PLS"
  if(da.validation.param[1,2] == "Support Vector Machine") da.validation.param[1,2] <- "svm"
  if(da.validation.param[1,2] == "Logistic regression") da.validation.param[1,2] <- "lr"
  if(da.validation.param[1,2] == "Random Forest") da.validation.param[1,2] <- "rf"
  da.validation.param
}




DAdmParam <- function(
  daDMpCutoff,
  daDMfcCutoff,
  daDMvipCutoff
){

  da.dm.param <- data.frame(
    "Parameter" = c("P-value cutoff", "Fold-change cutoff", "VIP cutoff"),
    "Setting" = c(daDMpCutoff, daDMfcCutoff, daDMvipCutoff),
    stringsAsFactors = FALSE)
  da.dm.param
}






#####data cleaning
DCbaParam <- function(
  dc.ba.mz.tol,
  dc.ba.rt.tol
){

  da.ba.param <- data.frame(
    "Parameter" = c("m/z tolerance (ppm)", "Retention time tolerance"),
    "Setting" = c(dc.ba.mz.tol, dc.ba.rt.tol),
    stringsAsFactors = FALSE)
  da.ba.param
}




DCmvParam <- function(dc.mv.peak.remove.tol,
                      DCimputationMethod,
                      k,
                      mvNtree,
                      mvReplace,
                      bpca.nPcs,
                      ppca.nPcs,
                      svd.nPcs,
                      rowmax,
                      colmax){

  dc.mv.params <- cbind("Parameter" = c("Peaks will be removed if MV ratio more than %",
                                        "Imputation method"),
                        "Setting" = c(dc.mv.peak.remove.tol, DCimputationMethod)
  )
  if(DCimputationMethod == "KNN"){
    dc.mv.params1 <- cbind("Parameter" = c("Number of neighbors",
                                           "The maximum percent missing data allowed in any row",
                                           "The maximum percent missing data allowed in any column"),
                           "Setting" = c(k, rowmax, colmax)
    )

    dc.mv.params <- rbind(dc.mv.params, dc.mv.params1)
  }

  if(DCimputationMethod == "missForest"){
    dc.mv.params1 <- cbind("Parameter" = c("Number of trees to grow in each forest",
                                           "Bootstrap sampling (with replacements) is performed"),
                           "Setting" = c(mvNtree, mvReplace)
    )

    dc.mv.params <- rbind(dc.mv.params, dc.mv.params1)
  }

  if(DCimputationMethod == "BPCA"){
    dc.mv.params1 <- cbind("Parameter" = c("Number of principal components to calculate"),
                           "Setting" = c(bpca.nPcs)
    )
    dc.mv.params <- rbind(dc.mv.params, dc.mv.params1)
  }

  if(DCimputationMethod == "PPCA"){
    dc.mv.params1 <- cbind("Parameter" = c("Number of principal components to calculate"),
                           "Setting" = c(ppca.nPcs)
    )

    dc.mv.params <- rbind(dc.mv.params, dc.mv.params1)
  }

  if(DCimputationMethod == "SVD"){

    dc.mv.params1 <- cbind("Parameter" = c("Number of principal components to calculate"),
                           "Setting" = c(svd.nPcs)
    )
    dc.mv.params <- rbind(dc.mv.params, dc.mv.params1)
  }

  dc.mv.params

}






DCdnParam <- function(
  DCdnHasQC,
  normalizationMethod1,
  normalizationMethod2,
  loess.kepp.dimension,
  parameter.optimization,
  begin.end,
  loess.step,
  svr.kepp.dimension,
  svr.multiple
){

  dc.dn.param <- cbind(
    "Parameter" = "QC sample-based methods",
    "Setting" = DCdnHasQC
  )

  if(DCdnHasQC == "hasQC"){
    dc.dn.param[1,2] <- "YES"
    if(normalizationMethod1 == "loess"){

      dc.dn.param1 <- cbind("Parameter" = c("Normalization method",
                                            "KEPP dimension or not?",
                                            "Optimize parameters?",
                                            "Beigin and End",
                                            "Step of LOESS"),
                            "Setting" = c("QC LOESS",
                                          loess.kepp.dimension,
                                          parameter.optimization,
                                          paste(begin.end, collapse = ";"),
                                          loess.step)
      )

      dc.dn.param <- rbind(dc.dn.param, dc.dn.param1)
    }

    if(normalizationMethod1 == "svr"){

      dc.dn.param1 <- cbind("Parameter" = c("Normalization method",
                                            "KEPP dimension or not?",
                                            "How many peaks used?"),
                            "Setting" = c("QC SVR (MetNormalizer)",
                                          svr.kepp.dimension,
                                          svr.multiple)
      )

      dc.dn.param <- rbind(dc.dn.param, dc.dn.param1)
    }
  }


  if(DCdnHasQC == "noQC"){
    dc.dn.param[1,2] <- "NO"
    dc.dn.param1 <- cbind("Parameter" = c("Normalization method"),
                          "Setting" = c(normalizationMethod2))
    if(dc.dn.param1[1,2] == "no") {dc.dn.param1[1,2] == "None"}
    if(dc.dn.param1[1,2] == "mean") {dc.dn.param1[1,2] == "Mean"}
    if(dc.dn.param1[1,2] == "median") {dc.dn.param1[1,2] == "Median"}
    if(dc.dn.param1[1,2] == "total") {dc.dn.param1[1,2] == "Total"}
    dc.dn.param <- rbind(dc.dn.param, dc.dn.param1)
  }

  dc.dn.param

}



DCdiParam <- function(
  DCdiHasQC,
  integrationMethod1,
  integrationMethod2

){

  dc.di.param <- cbind(
    "Parameter" = "QC sample-based methods", "Setting" = DCdiHasQC
  )

  if(DCdiHasQC == "hasQC"){
    dc.di.param[1,2] <- "YES"
    dc.di.param1 <- cbind("Parameter" = "Integration method",
                          "Setting" = integrationMethod1)
    if(dc.di.param1[1,2] == "qc.mean") {dc.di.param1[1,2] <- "QC mean"}
    if(dc.di.param1[1,2] == "qc.median") {dc.di.param1[1,2] <- "QC median"}
    if(dc.di.param1[1,2] == "no") {dc.di.param1[1,2] <- "None"}
    dc.di.param <- rbind(dc.di.param, dc.di.param1)
  }

  if(DCdiHasQC == "noQC"){
    dc.di.param[1,2] <- "NO"
    dc.di.param1 <- cbind("Parameter" = "Integration method",
                          "Setting" = integrationMethod2)
    if(dc.di.param1[1,2] == "subject.mean") {dc.di.param1[1,2] <- "Subject mean"}
    if(dc.di.param1[1,2] == "subject.median") {dc.di.param1[1,2] <- "Subject median"}
    if(dc.di.param1[1,2] == "no") {dc.di.param1[1,2] <- "None"}
    dc.di.param <- rbind(dc.di.param, dc.di.param1)
  }

  dc.di.param

}




DCosParam <- function(
  dc.os.pca.log,
  dc.os.pca.scale,
  dc.os.pca.center,
  dc.os.pca.ci.tol,
  dc.os.zero.tol

){

  # dc.os.param <- cbind(
  #   "Logarithm method" = dc.os.pca.log,
  #   "Scale method" = dc.os.pca.scale,
  #   "Center or not" = dc.os.pca.center,
  #   "Samples will be considered as outliers outside % CI" = dc.os.pca.ci.tol,
  #   "Samples will be considered as outliers with zero value ratio > %" = dc.os.zero.tol,
  #   stringsAsFactors = FALSE
  # )

  dc.os.param <- cbind(
    "Parameter" = c("Logarithm method",
                    "Scale method",
                    "Center or not",
                    "Samples will be considered as outliers outside % CI",
                    "Samples will be considered as outliers with zero value ratio > %"),
    "Setting" = c(dc.os.pca.log,
                  dc.os.pca.scale,
                  dc.os.pca.center,
                  dc.os.pca.ci.tol,
                  dc.os.zero.tol)
  )

  if(dc.os.param[1,2] == "no") {dc.os.param[1,2] == "No log"}
  if(dc.os.param[1,2] == "log2") {dc.os.param[1,2] == "No log"}
  if(dc.os.param[1,2] == "log3") {dc.os.param[1,2] == "No log"}
  if(dc.os.param[1,2] == "log10") {dc.os.param[1,2] == "No log"}

  if(dc.os.param[2,2] == "no") {dc.os.param[2,2] == "No scale"}
  if(dc.os.param[2,2] == "auto") {dc.os.param[2,2] == "Auto scale"}
  if(dc.os.param[2,2] == "pareto") {dc.os.param[2,2] == "Pareto scale"}

  dc.os.param
}




SXTpaste<-function(x,sep=" ") {
  y<-NULL
  for (i in seq_along(x)) {
    if (i==1) {y<-paste(y,x[i],sep="")}
    else {y<-paste(y,x[i],sep=sep)}
  }
  y
}





#####pathway enrichment analysis
paParam <- function(
  paMetaboliteType,
  paMZpolarity,
  pa.mz.pos.adduct,
  pa.mz.neg.adduct,
  pa.mz.match.mz.tol,
  pa.mz.match.library,
  pa.pathway.library,
  pa.algorithm
){

  pa.param <- data.frame(
    "Parameter" = c("Metabolite type",
                    "Ionization polarity",
                    "Adduct type",
                    "m/z macth tolerance (ppm)",
                    "Database for metabolite identification",
                    "Pathway library",
                    "Pathway analysis algorithm"),
    "Setting" = c(paMetaboliteType,
                  paMZpolarity,
                  NA,
                  pa.mz.match.mz.tol,
                  pa.mz.match.library,
                  pa.pathway.library,
                  pa.algorithm),
    stringsAsFactors = FALSE)

  if(paMZpolarity == "positive"){
    pa.param[3, 2] <- paste(pa.mz.pos.adduct, collapse = ";")
  }else{
    pa.param[3, 2] <- paste(pa.mz.neg.adduct, collapse = ";")
  }

  if(pa.param[1,2] == "kegg.id") pa.param[1,2] <- "KEGG ID"
  if(pa.param[1,2] == "peak.mz") pa.param[1,2] <- "Peak (m/z)"

  if(pa.param[2,2] == "positive") pa.param[2,2] <- "Positive"
  if(pa.param[2,2] == "negative") pa.param[2,2] <- "Negative"

  if(pa.param[5,2] == "kegg") pa.param[5,2] <- "KEGG"

  if(pa.param[6,2] == "hsa") pa.param[6,2] <- "Homo sapiens (human)"
  if(pa.param[6,2] == "mmu") pa.param[6,2] <- "Mus musculus (mouse)"
  if(pa.param[6,2] == "rat") pa.param[6,2] <- "Rattus norvegicus (rat)"
  if(pa.param[6,2] == "bta") pa.param[6,2] <- "Bos taurus (cow)"
  if(pa.param[6,2] == "gga") pa.param[6,2] <- "Gallus gallus (chicken)"
  if(pa.param[6,2] == "dre") pa.param[6,2] <- "Danio rerio (zebrafish)"
  if(pa.param[6,2] == "dme") pa.param[6,2] <- "Drosophila melanogaster (fruit fly)"
  if(pa.param[6,2] == "cel") pa.param[6,2] <- "Caenorhabditis elegans (nematode)"
  if(pa.param[6,2] == "sce") pa.param[6,2] <- "Saccharomyces cerevisiae (yeast)"
  if(pa.param[6,2] == "ath") pa.param[6,2] <- "Arabidopsis thaliana (thale cress)"
  if(pa.param[6,2] == "smm") pa.param[6,2] <- "Schistosoma mansoni"
  if(pa.param[6,2] == "pfa") pa.param[6,2] <- "Plasmodum falciparum 3D7 (Malaria)"
  if(pa.param[6,2] == "tbr") pa.param[6,2] <- "Trypanosoma brucei"
  if(pa.param[6,2] == "eco") pa.param[6,2] <- "Escherichia coli K-12 MG1655"
  if(pa.param[6,2] == "ppu") pa.param[6,2] <- "Pseudomonas putida KT2440"
  if(pa.param[6,2] == "syf") pa.param[6,2] <- "Synechococcus elongatus"

  if(pa.param[7,2] == "hypergeometric") pa.param[7,2] <- "Hypergeometric Test"
  if(pa.param[7,2] == "fisher") pa.param[7,2] <- "Fisher's Exact Test"

  pa.param

}





getKeggLibrary <- function(species = c("hsa","dme", "mmu", "rat", "bta", "gga",
                                       "dre", "cel", "sce", "ath", "smm", "pfa",
                                       "tbr", "eco", "ppu", "syf")){

  switch(species,
         "hsa" = {load("data/database/hsa.kegg.pathway.rda", envir = environment())
           return(hsa.kegg.pathway)
           },
         "dme" = {load("data/database/dme.kegg.pathway.rda", envir = environment())
             return(dme.kegg.pathway)
           },
         "mmu" = {load("data/database/mmu.kegg.pathway.rda", envir = environment())
           return(mmu.kegg.pathway)
           },
         "rat" = {load("data/database/rat.kegg.pathway.rda", envir = environment())
           return(rat.kegg.pathway)
                  },
         "bta" = {load("data/database/bta.kegg.pathway.rda", envir = environment())
           return(bta.kegg.pathway)
                  },
         "gga" = {load("data/database/gga.kegg.pathway.rda", envir = environment())
           return(gga.kegg.pathway)
           },
         "dre" = {load("data/database/dre.kegg.pathway.rda", envir = environment())
           return(dre.kegg.pathway)
           },
         "cel" = {load("data/database/cel.kegg.pathway.rda", envir = environment())
           return(cel.kegg.pathway)
           },
         "sce" = {load("data/database/sce.kegg.pathway.rda", envir = environment())
           return(sce.kegg.pathway)
           },
         "ath" = {load("data/database/ath.kegg.pathway.rda", envir = environment())
           return(ath.kegg.pathway)
           },
         "smm" = {load("data/database/smm.kegg.pathway.rda", envir = environment())
           return(smm.kegg.pathway)
           },
         "pfa" = {load("data/database/pfa.kegg.pathway.rda", envir = environment())
           return(pfa.kegg.pathway)
           },
         "tbr" = {load("data/database/tbr.kegg.pathway.rda", envir = environment())
           return(tbr.kegg.pathway)
           },
         "eco" = {load("data/database/eco.kegg.pathway.rda", envir = environment())
           return(eco.kegg.pathway)
           },
         "ppu" = {load("data/database/ppu.kegg.pathway.rda", envir = environment())
           return(ppu.kegg.pathway)
           },
         "syf" = {load("data/database/syf.kegg.pathway.rda", envir = environment())
           return(syf.kegg.pathway)
           }
  )

}