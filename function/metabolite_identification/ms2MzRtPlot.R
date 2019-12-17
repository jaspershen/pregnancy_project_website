ms2MzRtPlot <- function(object){
  temp.mz <- as.numeric(unlist(lapply(object, function(x) x[[1]][2,1])))
  temp.rt <- as.numeric(unlist(lapply(object, function(x) x[[1]][3,1])))
  temp.name <- unlist(lapply(object, function(x) x[[1]][1,1]))

  object <- data.frame(temp.mz, temp.rt, temp.name, stringsAsFactors = FALSE)
  colnames(object) <- c("mz", "rt", "name")

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10),
  #                  axis.title.y = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
  #                  axis.text.y = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(object, ggplot2::aes(x = rt, y = mz,
                                                label = name)) +
    ggplot2::geom_point(alpha = 0.3, colour = "dodgerblue") +
    ggplot2::labs(x = "Retention time (RT, second)",
                  y = "Mass to charge ratio (m/z)")+
    ggplot2::ggtitle(paste(length(temp.mz), "spectra in total"))+
    my.theme

  return(plot)

}