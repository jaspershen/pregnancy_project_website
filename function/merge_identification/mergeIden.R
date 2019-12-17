

### PeakIdentification
mergeIden <- function(peak.table1,
                      peak.table2,
                      mz.tolerance = 30,
                      rt.tolerance = 180) {
  options(warn = -1)

  subject <- peak.table1[[1]][,-c(1:3)]
  tags <- peak.table1[[1]][,c(1:3)]

  peak.number <- nrow(tags)


  peak.number <- as.numeric(peak.number)

  peak.mz <- list()
  peak.mz[[peak.number + 1]] <- NA
  peak.rt <- list()
  peak.rt[[peak.number + 1]] <- NA
  peak.mzerror <- list()
  peak.mzerror[[peak.number + 1]] <- NA
  peak.rterror <- list()
  peak.rterror[[peak.number + 1]] <- NA
  peak.forward <- list()
  peak.forward[[peak.number + 1]] <- NA
  peak.reverse <- list()
  peak.reverse[[peak.number + 1]] <- NA
  peak.name <- list()
  peak.name[[peak.number + 1]] <- NA
  which.file <- list()
  which.file[[peak.number + 1]] <- NA

  ms1 <- tags
  peak <- ms1[, c("mz", "rt", "name")]


  ##begin match
    cat("Reading ms2 data...")
    ms2 <- peak.table2
    rm(list = c("peak.table2"))
    ms2 <- lapply(ms2, as.data.frame)

    for (i in seq_along(ms2)) {
      msms <- ms2[[i]]
      temp.idx.forward1 <- which(colnames(msms) == "hits.forward_zhumetlib")
      temp.idx.forward2 <- which(colnames(msms) == "hits.forward_metlinlib")
      temp.idx.forward3 <- which(colnames(msms) == "hits.forward")

      if(all(length(temp.idx.forward1) == 0 & length(temp.idx.forward2) == 0 & length(temp.idx.forward3) == 0)){
        stop("There are no identifiecation result!")
      }


      temp.idx.reverse1 <- which(colnames(msms) == "hits.reverse_zhumetlib")
      temp.idx.reverse2 <- which(colnames(msms) == "hits.reverse_metlinlib")
      temp.idx.reverse3 <- which(colnames(msms) == "hits.reverse")

      if(all(length(temp.idx.reverse1) == 0 & length(temp.idx.reverse2) == 0 & length(temp.idx.reverse3) == 0)){
        stop("There are no identifiecation result!")
      }

      temp.idx.forward <- c(temp.idx.forward1, temp.idx.forward2, temp.idx.forward3)
      temp.idx.reverse <- c(temp.idx.reverse1, temp.idx.reverse2, temp.idx.reverse3)


      forward <- msms[,c(temp.idx.forward), drop = FALSE]
      reverse <- msms[,c(temp.idx.reverse), drop = FALSE]

      forward[is.na(forward)] <- ""
      reverse[is.na(reverse)] <- ""

      forward <- apply(forward, 1, list)
      forward <- lapply(forward, unlist)

      reverse <- apply(reverse, 1, list)
      reverse <- lapply(reverse, unlist)

      forward <- lapply(forward, function(x) {paste(x, collapse = ";")})
      reverse <- lapply(reverse, function(x) {paste(x, collapse = ";")})

      forward <- unlist(forward)
      reverse <- unlist(reverse)


      forward[forward == ";"] <- ""
      reverse[reverse == ";"] <- ""

      #remove the unidentified feature
      temp.idx <- which(forward != '' | reverse != '')
      forward <- forward[temp.idx]
      reverse <- reverse[temp.idx]
      msms <- msms[temp.idx,]
      msmsinfo <- msms[, c("mzmed", "rtmed")]
      #get the information of metabolite
      # forward <- as.character(msms[, "hits.forward"])
      # reverse <- as.character(msms[, "hits.reverse"])
      forward[is.na(forward)] <- ""
      reverse[is.na(reverse)] <- ""
      name <- as.character(msms[, "name"])
      mz <- as.numeric(msms[, "mzmed"])
      rt <- as.numeric(msms[, "rtmed"])
      # adduct <- as.character(msms[, "adduct"])
      # isotopes <- as.character(msms[, "isotopes"])

      file <- rep(paste("MS2_peak_table", i, sep = ""), length(forward))

      #begin ms1 and ms2 matching
      cat("\n")
      cat(paste("Begin MS2_", i, "matching...\n"))

      result <-
        SXTMTmatch(data1 = peak,
                   data2 = msmsinfo,
                   mz.tol =mz.tolerance,
                   rt.tol = rt.tolerance)

      if (is.null(result)) {
        next
        }else{
        #mz and rt information
        mzerror <- result[, "mz error"]
        rterror <- result[, "rt error"]

        index1 <- result[, "Index1"]
        index2 <- result[, "Index2"]

        for (i in seq_along(index1)) {
          peak.forward[[index1[i]]] <-
            c(peak.forward[[index1[i]]], forward[index2[i]])
          peak.reverse[[index1[i]]] <-
            c(peak.reverse[[index1[i]]], reverse[index2[i]])
          peak.name[[index1[i]]] <-
            c(peak.name[[index1[i]]], name[index2[i]])
          peak.mz[[index1[i]]] <-
            c(peak.mz[[index1[i]]], mz[index2[i]])
          peak.rt[[index1[i]]] <-
            c(peak.rt[[index1[i]]], rt[index2[i]])
          peak.mzerror[[index1[i]]] <-
            c(peak.mzerror[[index1[i]]], mzerror[i])
          peak.rterror[[index1[i]]] <-
            c(peak.rterror[[index1[i]]], rterror[i])
          # peak.isotopes[[index1[i]]] <-
          #   c(peak.isotopes[[index1[i]]], isotopes[index2[i]])
          # peak.adduct[[index1[i]]] <-
          #   c(peak.adduct[[index1[i]]], adduct[index2[i]])
          which.file[[index1[i]]] <-
            c(which.file[[index1[i]]], file[index2[i]])
        }
      }
    }


  ms1name <- as.character(ms1[, "name"])


  for (i in seq_len(peak.number)) {
    if (is.null(peak.name[[i]]) | length(peak.name[[i]]) == 1) {
      next
    }else {
      index <-
        which(as.numeric(peak.mzerror[[i]]) - min(as.numeric(peak.mzerror[[i]])) <= 5)
      index <-
        match(min(as.numeric(peak.rterror[[i]])[index]), as.numeric(peak.rterror[[i]]))
      peak.forward[[i]] <- peak.forward[[i]][index]
      peak.reverse[[i]] <- peak.reverse[[i]][index]
      peak.name[[i]] <- peak.name[[i]][index]
      peak.mz[[i]] <- peak.mz[[i]][index]
      peak.rt[[i]] <- peak.rt[[i]][index]
      peak.mzerror[[i]] <- peak.mzerror[[i]][index]
      peak.rterror[[i]] <- peak.rterror[[i]][index]
      # peak.adduct[[i]] <- peak.adduct[[i]][index]
      # peak.isotopes[[i]] <- peak.isotopes[[i]][index]
      which.file[[i]] <- which.file[[i]][index]

    }
  }

  index <- NULL
  for (i in seq_len(peak.number)) {
    if (is.null(peak.name[[i]])) {
      index <- index
    }
    else {
      index <- c(index, i)
    }
  }


  ms1name <- as.character(peak[, "name"])
  forward <- rep(NA, peak.number)
  reverse <- rep(NA, peak.number)
  ms2name <- rep(NA, peak.number)
  ms2mz <- rep(NA, peak.number)
  ms2rt <- rep(NA, peak.number)
  # ms2isotopes <- rep(NA, peak.number)
  # ms2adduct <- rep(NA, peak.number)
  mzerror <- rep(NA, peak.number)
  rterror <- rep(NA, peak.number)
  from.file <- rep(NA, peak.number)

  for (i in index) {
    if (is.null(peak.forward[[i]])) {
      forward[i] <- NA
    }
    else {
      forward[i] <- SXTpaste(peak.forward[[i]], sep = "|")
    }

    if (is.null(peak.reverse[[i]])) {
      reverse[i] <- NA
    }
    else {
      reverse[i] <- SXTpaste(peak.reverse[[i]], sep = "|")
    }

    ms2name[i] <- SXTpaste(peak.name[[i]], sep = "|")
    ms2mz[i] <- SXTpaste(peak.mz[[i]], sep = "|")
    # ms2isotopes[i] <- SXTpaste(peak.isotopes[[i]], sep = "|")
    # ms2adduct[i] <- SXTpaste(peak.adduct[[i]], sep = "|")
    ms2rt[i] <- SXTpaste(peak.rt[[i]], sep = "|")
    mzerror[i] <- SXTpaste(peak.mzerror[[i]], sep = "|")
    rterror[i] <- SXTpaste(peak.rterror[[i]], sep = "|")
    from.file[i] <- SXTpaste(which.file[[i]], sep = "|")

  }


  forward <-
    sapply(forward, function(x) {
      if (is.na(x)) {
        x
      } else {
        ifelse(x == "", NA, x)
      }
    })
  reverse <-
    sapply(reverse, function(x) {
      if (is.na(x)) {
        x
      } else {
        ifelse(x == "", NA, x)
      }
    })
  # ms2isotopes <-
  #   sapply(ms2isotopes, function(x) {
  #     if (is.na(x)) {
  #       x
  #     } else {
  #       ifelse(x == "", NA, x)
  #     }
  #   })
  # ms2adduct <-
  #   sapply(ms2adduct, function(x) {
  #     if (is.na(x)) {
  #       x
  #     } else {
  #       ifelse(x == "", NA, x)
  #     }
  #   })
  names(forward) <-
    names(reverse) <- NULL
  lib <- rep(NA, peak.number)
  identification <- rep(NA, peak.number)
  for (i in seq_along(forward)) {
    # cat(i);cat(" ")
    if (!is.na(forward[i]) | !is.na(reverse[i])) {
      if (!is.na(forward[i])) {
        compound <- forward[i]
      }
      else {
        compound <- reverse[i]
      }
      if (regexpr("\\{", compound)[[1]] < 0) {
        lib[i] <- "MetDDA"
        identification[i] <-
          substr(compound, 1, regexpr("Score", compound)[[1]] - 2)
      }
      else {
        lib[i] <- "zhulab"
        identification[i] <-
          substr(compound,
                 gregexpr("\\{", compound)[[1]][3] + 1,
                 gregexpr("\\}", compound)[[1]][3] - 1)
      }
    }
    else
      (next)
  }

  ide.idx <- which(!is.na(identification))
  ide <- identification[ide.idx]
  dup.ide <- unique(ide[duplicated(ide)])

  if (length(dup.ide) != 0) {
    for (k in seq_along(dup.ide)) {
      temp.idx <- grep(dup.ide[k], ide)
      ide[temp.idx] <-
        paste(dup.ide[k], c(seq_along(temp.idx)), sep = "_")
    }

    identification[ide.idx] <- ide
  }
  peak.identification <-
    data.frame(
      ms1name,
      peak,
      ms2name,
      ms2mz,
      ms2rt,
      mzerror,
      rterror,
      # ms2isotopes,
      # ms2adduct,
      forward,
      reverse,
      identification,
      lib,
      from.file,
      stringsAsFactors = FALSE
    )


  marker <-
    peak.identification[!is.na(peak.identification[, "ms2name"]),]
  marker.ms2name <- as.character(marker[, "ms2name"])
  marker.ms2name <- unique(ms2name[!is.na(ms2name)])
  new.marker <- matrix(ncol = ncol(peak.identification) + 1)
  colnames(new.marker) <- c(colnames(marker), "remain")

  for (i in seq_along(marker.ms2name)) {
    temp <-
      marker[marker[, "ms2name"] == marker.ms2name[i], , drop = FALSE]
    if (nrow(temp) == 1) {
      temp <- cbind(temp, TRUE)
      colnames(temp)[ncol(new.marker)] <- "remain"
      new.marker <- rbind(new.marker, temp)
    }
    else {
      rterror <-
        as.numeric(as.character(temp[, "rterror"]))
      mzerror <- as.numeric(as.character(temp[, "mzerror"]))
      index <- which(mzerror - min(mzerror) <= 5)
      index <- match(min((rterror)[index]), rterror)
      need <- rep(FALSE, nrow(temp))
      need[index] <- TRUE
      temp <- cbind(temp, need)
      colnames(temp)[ncol(new.marker)] <- "remain"
      new.marker <- rbind(new.marker, temp)
    }
  }

  new.marker <- new.marker[-1, ]

  remain <- new.marker[, "remain"]
  for (i in seq_along(remain)) {
    if (remain[i]) {
      next
    }
    else {
      new.marker[i, c(5:15)] <- rep(NA, 11)
    }
  }



  peak.name <- as.character(peak.identification[, "ms1name"])
  marker.name <- as.character(new.marker[, "ms1name"])


  peak.identification[match(marker.name, peak.name), 1:14] <-
    new.marker[, 1:14]


  num <- sum(!is.na(peak.identification[,"identification"]))

  # peak.identification$ms1name <- as.character(peak.identification$ms1name)
  # peak.identification$name <- as.character(peak.identification$name)
  # peak.identification$ms2name <- as.character(peak.identification$ms2name)
  # peak.identification$ms2mz <- as.numeric(peak.identification$ms2mz)
  # peak.identification$ms2rt <- as.numeric(peak.identification$ms2rt)
  # peak.identification$mzerror <- as.numeric(peak.identification$mzerror)
  # peak.identification$rterror <- as.numeric(peak.identification$rterror)
  # peak.identification$forward <- as.character(peak.identification$forward)
  # peak.identification$reverse <- as.character(peak.identification$reverse)
  # peak.identification$identification <- as.character(peak.identification$identification)
  # peak.identification$lib <- as.character(peak.identification$lib)
  # peak.identification$from.file <- as.character(peak.identification$from.file)
  # peak.identification$mz <- as.numeric(peak.identification$mz)
  # peak.identification$rt <- as.numeric(peak.identification$rt)

  peak.identification <- peak.identification[,c("ms1name", "mz", "rt", "ms2name", "mzerror",
                                                "rterror", "forward", "reverse", "identification",
                                                "lib", "from.file")]
  colnames(peak.identification)[1] <- "name"
  colnames(peak.identification)[4] <- "MS2.name"
  colnames(peak.identification)[5] <- "mz.error"
  colnames(peak.identification)[6] <- "rt.error"
  return(peak.identification)
}


