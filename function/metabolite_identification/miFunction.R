#------------------------------------------------------------------------------
# title combineMS1MS2
# description Combine MS1 and MS2 data.
# author Xiaotao Shen
# \email{shenxt@@sioc.ac.cn}
# param ms1.file The name of ms1 peak table. Column 1 is "name", Column 2 is
# "mz" and column 3 is "rt".
# param ms2.file The vector of names of ms2 files. MS2 file must be mzXML.
# param mz.tol mz tol for ms1 and ms2 data matching.
# param rt.tol RT tol for ms1 and ms2 data matching.
# param path Directory.
# param ms2.type The type of MS2 file, default is mzXML.
# return Return ms1 and ms2 data.


setGeneric(name = "combineMS1MS2",
           function(ms1.peak.table,
                    ms2.data,
                    mz.tol = 25,
                    rt.tol = 10,
                    ms2.type = c("mgf", "mzXML", "msp")){
             #
             ms1.name <- ms1.peak.table$name
             ms1.mz <- ms1.peak.table$mz
             ms1.rt <- ms1.peak.table$rt
             ms1.info <- data.frame(ms1.mz, ms1.rt, ms1.name,
                                    stringsAsFactors = FALSE)

             ms2.info <- lapply(ms2.data, function(x) x[[1]])
             ms2.info <- do.call(rbind, ms2.info)
             ms2.info <- as.data.frame(ms2.info)

             ##if ms2.info is from metAnalyzer
             if(length(grep("[A-Ma-z]", ms2.info[1,2])) > 0){
               if(length(grep("POS|NEG", ms1.info[1,3]))>0){
                 ms1.name <- gsub("[_POS|_NEG]", "", ms1.info[,3])
               }
               temp.idx <- match(ms2.info[,2], ms1.name)
               ms2.info[,2] <- ms1.info$ms1.rt[temp.idx]
               ms2.info[,1] <- ms1.info$ms1.mz[temp.idx]
               ms2.info$rt[is.na(ms2.info$rt)] <- 0
               ms2.info$mz[is.na(ms2.info$mz)] <- 0

               ##
               temp <- apply(ms2.info, 1, list)
               temp <- lapply(temp, unlist)

               ms2.data <- mapply(function(x, y){
                 x[[1]] <- y
                 list(x)
               },
               x = ms2.data,
               y = temp)
             }

             if(max(ms2.info[,2], na.rm = TRUE) < 60){
               ms2.info[,2] <- ms2.info[,2] * 60
             }

             ##match ms1 and ms2
             match.result <- SXTMTmatch(data1 = ms2.info,
                                        data2 = ms1.info,
                                        mz.tol = mz.tol, rt.tol = rt.tol,
                                        rt.error.type = "abs")


             ms2.name <- rep(NA, nrow(ms2.info))
             ms2.name[match.result[,"Index1"]] <- ms1.info[match.result[,"Index2"],"ms1.name"]
             ms2.name <- as.character(ms2.name)

             ##remove no matched MS2
             remove.idx <- which(is.na(ms2.name))
             if(length(remove.idx) > 0){
               ms2.name <- ms2.name[-remove.idx]
               ms2.data <- ms2.data[-remove.idx]
             }

             ###remove duplicated MS2 spectrum, if one peak has more than 1 ms2 spectrum,
             ###select the biggest of the sum intensity of top 10 fragments.
             unique.name <- unique(ms2.name)
             cat("\n")
             cat("Select the most intense MS2 spectrum for one peak.\n")
             remain.idx <- pbapply::pblapply(unique.name, function(name){
               temp.idx <- which(ms2.name == name)
               if(length(temp.idx) == 1) return(temp.idx)
               temp.ms2 <- ms2.data[temp.idx]
               temp.ms2 <- lapply(temp.ms2, function(x) x[[2]])
               temp.int <- lapply(temp.ms2, function(x) {
                 x <- as.data.frame(x)
                 x <- x[order(x[,2], decreasing = TRUE),]
                 if(nrow(x) >= 10) {sum(x[1:10,2])}else{sum(x[,2])}
               })
               temp.int <- unlist(temp.int)
               temp.idx <- temp.idx[which.max(temp.int)]
               temp.idx
             })

             remain.idx <- sort(unlist(remain.idx))

             ms2.data <- ms2.data[remain.idx]
             ms2.name <- ms2.name[remain.idx]


             ms2.data <- mapply(function(info, name){
               NAME <- name
               PRECURSORMZ <- info[[1]][1]
               PRECURSORRT <- info[[1]][2]

               info[[1]] <- data.frame(NAME, PRECURSORMZ, PRECURSORRT, stringsAsFactors = FALSE)
               info[[1]] <- t(info[[1]])
               # info[[1]] <- matrix(c(NAME, PRECURSORMZ, PRECURSORRT), ncol = 1)
               rownames(info[[1]]) <- c('NAME', 'PRECURSORMZ', 'PRECURSORRT')
               list(info)
             },
             info = ms2.data,
             name = as.character(ms2.name))
             ms2.data
           })






#-----------------------------------------------------------------------------
# title readMGF
# description Read mgf data.
# author Xiaotao Shen, Yandong Yin
#  \email{shenxt@@sioc.ac.cn}
# param file The vector of names of ms2 files. MS2 file must be mgf.
# return Return ms2 data.
# export

setGeneric(name = "readMGF",
           def = function(file){
             pbapply::pboptions(style = 1)
             # cat("Reading MS2 data (step1)\n")
             # mgf.data.list <- pbapply::pblapply(file, ListMGF)
             ms2 <- pbapply::pblapply(file, function(mgf.data) {
               mgf.data <- ListMGF(mgf.data)
               # nl.spec <- grep('^\\d', mgf.data)
               nl.spec <- lapply(mgf.data, function(x) grep('^\\d', x))
               info.mz <- lapply(mgf.data, function(x) grep('^PEPMASS', x, value = T))
               info.rt <- lapply(mgf.data, function(x) grep('^RTINSECONDS', x, value = T))

               info.mz <- unlist(info.mz)
               #for orbitrap data, the intensity of precursor ion should be removed
               info.mz <- unlist(lapply(strsplit(x = info.mz, split = " "), function(x) x[1]))
               info.mz <- as.numeric(gsub(pattern = "\\w+=", "", info.mz))
               info.rt <- unlist(info.rt)
               info.rt <- as.numeric(gsub(pattern = "\\w+=", "", info.rt))

               spec <- mapply(function(x, y){
                 do.call(rbind, strsplit(x[y], split = " "))
               },
               x = mgf.data,
               y = nl.spec)

               spec <- lapply(spec, function(x){
                 temp <- cbind(as.numeric(x[,1]),as.numeric(x[,2]))
                 temp <- matrix(temp, ncol = 2)
                 if(nrow(temp) > 0) temp <- temp[temp[,2] >= max(temp[,2])*0.01,]
                 temp <- matrix(temp, ncol = 2)
                 colnames(temp) <- c("mz", "intensity")
                 temp
               })

               ms2 <- mapply(function(x,y,z){
                 info <- c(y, z)
                 names(info) <- c("mz", "rt")
                 spectrum <- as.matrix(x)
                 temp <- list(info, spectrum)
                 names(temp) <- c("info", "spec")
                 list(temp)
               },
               x = spec,
               y = info.mz,
               z = info.rt)

               ms2

             })


             spec.info <- ms2[[1]]
             if(length(ms2) > 1){
               for(i in 2:length(ms2)){
                 spec.info <- c(spec.info, ms2[[i]])
               }
             }

             remove.idx <- which(unlist(lapply(spec.info, function(x) nrow(x[[2]]))) == 0)
             if(length(remove.idx) != 0) spec.info <- spec.info[-remove.idx]
             # ##remove noise
             # cat("\n")
             # cat("Remove noise of MS/MS spectra...\n")
             # spec.info <- pbapply::pblapply(spec.info, function(x){
             #   temp.spec <- x[[2]]
             #   temp.spec <- removeNoise(temp.spec)
             #   x[[2]] <- temp.spec
             #   x
             # })

             spec.info <- spec.info
           })


#----------------------------------------------------------------------------
setGeneric(name = "ListMGF",
           def = function(file){
             mgf.data <- readLines(file)
             nl.rec.new <- 1
             idx.rec <- 1
             rec.list <- list()
             for(nl in 1:length(mgf.data))
             {
               if(mgf.data[nl]=="END IONS")
               {
                 rec.list[idx.rec] <- list(Compound = mgf.data[nl.rec.new : nl])
                 nl.rec.new <- nl + 1
                 idx.rec <- idx.rec + 1
               }
             }
             rec.list
           })

setGeneric(name = "CheckInRange", def = function(targets, range){
  targets >= range[1] & targets <= range[2]
})










plotMS2 <- function(ms2.data){
  ms2.info <- ms2.data[[1]]
  ms2 <- ms2.data[[2]]
  par(xpd = FALSE)
  par(mar = c(5,5,4,2))
  plot(as.numeric(ms2[,1]),
       as.numeric(ms2[,2])*100/max(as.numeric(ms2[,2])),
       cex.lab = 1.3,
       cex.axis = 1.3,
       xlab = "m/z",
       ylab = "Relative intensity",
       type = "h", main = paste("m/z:", round(ms2.info[1], 4),
                                "; RT:", round(ms2.info[2]), 1))
}

















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



setGeneric(name = "mzMatch",
           def = function(mz,
                          ms1.database,
                          mz.tol = 25#ppm
                          ){
             pbapply::pblapply(mz, function(x){
               mz.error <- abs(x - ms1.database$Accurate.mass)*10^6/ms1.database$Accurate.mass
               idx <- which(mz.error < mz.tol)
               if(length(idx) > 0){
                 mz.error <- mz.error[idx]
                 result <- data.frame(ms1.database[idx,,drop = FALSE],
                                      mz.error, stringsAsFactors = FALSE)
                 # result <- result[,c("ID", "Charge", "Adduct", "mz.error")]
               }else{
                 return(NULL)
               }
             })
})







#---------------------------------------------------------------------------
RemoveRingEffect <- function(spec, mz.diff.thr = 0.3, int.rel.thr = 0.2) {
  spec <- spec[order(spec[, 'mz']), , drop = FALSE]
  nr.ring <- nrow(spec) + 1
  mz <- spec[, 'mz']

  mz.diff <- diff(mz)
  idx.mzdiff <- which(mz.diff <= mz.diff.thr)
  if (length(idx.mzdiff) == 0) {
    return(spec)
  }

  nr.ring.possible <- unique(c(idx.mzdiff, idx.mzdiff + 1))
  while (TRUE) {
    idx.int.max <- which.max(spec[nr.ring.possible, 2])
    nr.int.max <- nr.ring.possible[idx.int.max]
    int.thr <- spec[nr.int.max, 2] * int.rel.thr

    mz.diff <- abs(mz[nr.ring.possible[-idx.int.max]] - mz[nr.int.max])
    int <- spec[nr.ring.possible[-idx.int.max], 2]
    nr.ring <- append(nr.ring, nr.ring.possible[-idx.int.max][which(mz.diff <= mz.diff.thr & int <= int.thr)])
    nr.ring.possible <- nr.ring.possible[!nr.ring.possible %in% c(nr.ring, nr.int.max)]
    if (length(nr.ring.possible) == 0) {
      break
    }
  }

  return(spec[-nr.ring, , drop = FALSE])
}


#---------------------------------------------------------------------------
MergeFragments <- function(ms2.all, ppm = 30, mz.bin.min = 0.004) {
  spec.all <- do.call(rbind, ms2.all)
  spec.all <- spec.all[order(spec.all[, 'mz']), , drop = FALSE]

  idx.left <- seq(nrow(spec.all))

  spec.merged <- {}
  while (length(idx.left) > 0 ) {
    idx <- tail(idx.left, 1)
    mz <- spec.all[idx, 'mz']
    mz.range <- c(-1, 0) * max(prod(mz, ppm, 1e-6), mz.bin.min) + mz
    idx.range <- idx.left[spec.all[idx.left, 'mz'] >= mz.range[1] &
                            spec.all[idx.left, 'mz'] <= mz.range[2]]
    spec.tmp <- sapply(c('mz', 'intensity'), function(x) {
      quantile(spec.all[idx.left[idx.range], x], 0.5)
    })
    spec.merged <- rbind(spec.merged, spec.tmp)
    idx.left <- idx.left[-idx.range]
  }
  colnames(spec.merged) <- c('mz', 'intensity')
  rownames(spec.merged) <- NULL
  spec.merged <- spec.merged[order(spec.merged[, 'mz']), , drop = FALSE]

  return(spec.merged)
}



#---------------------------------------------------------------------------
VoteSpectra <- function(ms2.merged, ms2.all, num.smp,
                        ms2.find.method = c('concensus', 'combined'),
                        minfrac.vote = 0.25,
                        ppm = 30,
                        mz.bin.min = 0.004) {
  ms2.find.method <- match.arg(ms2.find.method)

  num.contained <- sapply(ms2.merged[, 'mz'], function(mz) {
    mz.range <- mz + max(prod(mz, ppm, 1e-6), mz.bin.min) * c(-1, 1)
    is.contained <- sapply(ms2.all, function(spec) {
      any(spec[, 'mz'] >= mz.range[1] & spec[, 'mz'] <= mz.range[2])
    })
    sum(is.contained)
  })
  if (ms2.find.method == 'combined') {
    ms2.merged[num.contained/num.smp >= minfrac.vote, , drop = FALSE]
  } else {
    ms2.merged[num.contained/length(ms2.all) >= minfrac.vote, , drop = FALSE]
  }
}







#---------------------------------------------------------------------------
setGeneric(name = "MetID", def = function(info,
                                          spec,
                                          pol, ce = '30',
                                          lib,
                                          dp.cutoff = 0.8,
                                          adduct,
                                          d.out,
                                          lc = 'HILIC',
                                          ms2.match.plot = TRUE){


  options(warn = -1)

  rerun.ms2 <- FALSE
  is.plotMSMS <- FALSE
  dt.peaktable <- info
  # colnames(dt.peaktable) <- c('mzmed', colnames(info)[-1])
  colnames(dt.peaktable)[2] <- "mzmed"
  dt.peaktable$mzmed <- as.numeric(dt.peaktable$mzmed)
  # dt.peaktable <- cbind('name' = seq(nrow(dt.peaktable)), dt.peaktable)
  # fn.lib <- 'zhuMetlib.RData'
  # fn.lib <- 'zhuMetlib.RData'
  lc <- 'HILIC'
  # fn.adduct.lc <- switch(pol,
  #                        'pos' = paste0(lc, '_POS.csv'),
  #                        'neg' = paste0(lc, '_NEG.csv'))
  # fp.adduct.lc <- file.path('.', fn.adduct.lc)
  # fp.adduct.lc <- adduct.table.hilic
  # lib <- LoadData(fn.lib)
  lib.meta <- lib$meta[[pol]][[ce]]
  lib.name <- apply(lib.meta, 1, function(info) {
    nr <- which(lib$meta$compound[, 'labid'] == as.character(info['labid']))
    lib$meta$compound[nr, 'name']
  })
  lib.meta$name <- lib.name

  lib.spec <- lib$compound[[pol]]

  # adduct <- read.csv(fp.adduct.lc,
  #                    stringsAsFactors = FALSE)
  lib.mz <- t(sapply(lib.meta[, 'mz'], function(mz) {
    mz <- as.numeric(mz)
    apply(adduct, 1, function(info.adduct) {
      x <- gsub('\\(', '',
                gsub('M.*', '', info.adduct['adduct']))
      xm <- ifelse(x == '', 1, as.numeric(x))
      xm * mz + as.numeric(info.adduct['mz'])
    })
  }))
  colnames(lib.mz) <- adduct$adduct
  idx.peak.match <- unname(which(!sapply(spec, is.null)))
  # ShowSeperateLines('Identifying metabolites with library ...')
  cat("\n")
  cat('Identify metabolites with MS/MS library.\n')
  # match library reverse and forward
  for (direction in c('reverse', 'forward')) {
    cat("\n")
    cat('Search ', direction, '.\n', sep = "")
    # fn.skip <- paste0('Search_', direction, '.Rda')
    #
    pbapply::pboptions(style = 1)
    id.peaks.list <- pbapply::pblapply(idx.peak.match, ParIdentifyFeatureMet,
                                       dt.peaktable,
                                       # spec[idx.peak.match],
                                       spec,
                                       lib.meta, lib.spec, lib.mz,
                                       ce,
                                       search.scope = 'ms1.based',
                                       ppm.ms1match = 25,
                                       ppm.ms2match = 35,
                                       cutoff = dp.cutoff,
                                       top = 10, # report top 10 satisfactories
                                       top.below = 5, # if no satisfactory, report top 5
                                       is.tune.ms2.exp = TRUE,
                                       is.tune.ms2.lib = FALSE,
                                       mz.range.ms2 = NULL,
                                       is.include.precursor = ifelse(pol == 'neg', TRUE, FALSE),
                                       weight.mz = 0,
                                       weight.int = 1,
                                       int.ms2.min.relative = 0.01,
                                       is.apply.ms2.min.relative = TRUE,
                                       noise.ms2 = 10,
                                       snthr = 3,
                                       ppm.sanity.check = 100,
                                       is.sanity.check = FALSE,
                                       direction = direction)
    # save(id.peaks.list, file = fn.skip)


    num.hits <- rep(0, nrow(dt.peaktable))
    num.hits[idx.peak.match] <- sapply(id.peaks.list, function(id) {
      if (all(is.na(id))) {
        0
      } else {
        nrow(id)
      }
    })

    info.hits <- rep('', nrow(dt.peaktable))
    info.hits[idx.peak.match] <- sapply(id.peaks.list, function(id){
      if (!all(is.na(id)) & length(id) > 0) {
        paste(paste('score{', round(id$score, 6), '}',
                    'adduct{', id$adduct, '}',
                    'name{', id$name, '}',
                    'labid{', id$labid, '}', sep = ''),
              collapse = ';')
      } else {
        ''
      }
    })
    if (all(num.hits == 0)) {
      return()
    }

    # if (is.plotMSMS) {
    # d.spec <- file.path('MetLibMatch/MSMSfigures', d.out)
    d.spec <- d.out
    dir.create(d.spec, recursive = T)

    info.peak.plot <- data.frame('nr.peaktable' = idx.peak.match[which(num.hits[idx.peak.match] > 0)],
                                 'idx.peaklist' = which(num.hits[idx.peak.match] > 0))

    if(ms2.match.plot){
      cat("\n")
      cat('Plot spectra match results.\n')
      PlotIDResults(id.peaks.list, info.peak.plot,
                    spec, dt.peaktable,
                    lib.spec,
                    lib.meta,
                    ce,
                    polarity = pol,
                    direction = direction,
                    d.spec = d.spec,
                    width = 20, height = 7,
                    is.include.precursor = FALSE,
                    is.tune.ms2.exp = TRUE,
                    is.tune.ms2.lib = FALSE,
                    mz.range.ms2 = NULL)
    }
    # }
    assign(paste0('id.peaks.info.', direction),
           cbind('nhits' = num.hits, 'hits' = info.hits))
  }

  id.peaks.info <- cbind(id.peaks.info.reverse, id.peaks.info.forward)
  colnames(id.peaks.info) <- c('nhits.reverse', 'hits.reverse', 'nhits.forward', 'hits.forward')
  # if(!file.exists('MetLibMatch')) dir.create('MetLibMatch')
  # fn <- file.path('MetLibMatch', paste0(gsub('/', '_', d.out), '.csv'))
  # write.csv(cbind(dt.peaktable, id.peaks.info), fn, row.names = FALSE)
  annotation.result <- data.frame(dt.peaktable, id.peaks.info,
                                  stringsAsFactors = FALSE)
  return(annotation.result)
})


#---------------------------------------------------------------------------
setGeneric(name = "ParIdentifyFeatureMet",
           def = function(idx.pk,
                          dt.peaktable,
                          ms.assigned,
                          lib.meta,
                          lib.spec,
                          lib.mz,
                          ce,
                          search.scope,
                          ppm.ms1match = 30,
                          ppm.ms2match = 30,
                          cutoff = 0.6,
                          top = 10, # report top 10 satisfactories
                          top.below = 5, # if no satisfactory, report top 5
                          is.tune.ms2.exp = FALSE,
                          is.include.precursor = is.include.precursor,
                          ...){
             # ...:
             # is.tune.ms2.lib = is.tune.ms2.lib,
             # weight.mz = weight.mz,
             # weight.int = weight.int,
             # int.ms2.min.relative = int.ms2.min.relative,
             # is.apply.ms2.min.relative = is.apply.ms2.min.relative,
             # noise.ms2 = noise.ms2,
             # snthr = 3,
             # ppm.sanity.check = 100,
             # is.sanity.check = FALSE,
             # direction = direction
             # suppressMessages(require(MetMatch))
             # cat(idx.pk, '\n')
             pk.precursor <- dt.peaktable[idx.pk, ]
             pk.mz <- pk.precursor$mzmed
             pk.spec <- ms.assigned[[idx.pk]]

             if (is.tune.ms2.exp) {
               pk.spec <- TuneMS2(pk.spec, pk.mz, is.include.precursor = is.include.precursor,...)
             }
             if (length(pk.spec) == 0) {
               return(NA)
             }
             pk.mz.range <- GetRangePPM(pk.mz, ppm.ms1match)

             switch(search.scope,
                    'ms1.based' = {
                      idx.lib.match <- apply(lib.mz, 2, function(mz.col) {
                        which(mz.col >= pk.mz.range[1] & mz.col <= pk.mz.range[2])
                      })

                      idx.remove <- which(sapply(idx.lib.match, length) == 0)
                      if (length(idx.remove) > 0) {
                        idx.lib.match <- idx.lib.match[-idx.remove]
                      }
                    },
                    'all' = {
                      idx.lib.match <- list(seq(nrow(lib.meta)))
                    })

             if (length(idx.lib.match) == 0) {
               return(NA)
             }
             #
             id.list.adduct <- lapply(seq(idx.lib.match), function(idx) {
               idx.match <- idx.lib.match[[idx]]
               ids <- lib.meta[idx.match, 'labid']
               mz.lib <- lib.meta[idx.match, 'mz']
               id.list <- lapply(seq_along(ids), function(idx) {
                 id <- ids[idx]
                 lib.spec.match <- lib.spec[[id]][[ce]]
                 if (!is.include.precursor) {
                   mz.cutoff <- GetRangePPM(mz.lib[idx], 20)[2]
                   lib.spec.match <- lib.spec.match[lib.spec.match[, 'mz'] <= mz.cutoff, ,
                                                    drop = FALSE]
                 }
                 IdentifyFeature(pk.spec, lib.spec.match,
                                 ppm.ms2match = ppm.ms2match,
                                 is.include.precursor = is.include.precursor,...)
               })
               #

               sc <- do.call(c, id.list)
               sc[is.nan(sc) | is.na(sc)] <- 0
               id.result <- data.frame(sc, lib.meta[idx.match, c(1, 5), drop = FALSE], stringsAsFactors = FALSE)
               id.result$adduct <- rep(names(idx.lib.match[idx]), length(ids))
               id.result
             })
             id.result <- do.call(rbind, id.list.adduct)
             switch(as.character(ncol(id.result)),
                    '3' = {
                      colnames(id.result) <- c('score', 'labid', 'name')
                    },
                    '4' = {
                      colnames(id.result) <- c('score', 'labid', 'name', 'adduct')
                    })

             id.result.cutoff <- id.result[id.result[, 'score'] >= cutoff, , drop = FALSE]
             id.result.cutoff <- id.result.cutoff[order(id.result.cutoff[, 'score'], decreasing = TRUE), , drop = FALSE]
             id.result.cutoff[, 'score'] <- round(id.result.cutoff[, 'score'], 4)

             if (length(id.result.cutoff) > 0) {
               id.result.output <- head(id.result.cutoff, top)
             } else {
               id.result.output <- head(id.result, top.below)
             }

             if (nrow(id.result.output) == 0) {
               id.result.output <- NA
             }

             return(id.result.output)
           })



#---------------------------------------------------------------------------

setGeneric(name = "PlotIDResults",
           def = function(id.peaks.list, info.peak.plot, ms.assigned, dt.peaktable,
                          lib.spec, lib.meta, ce, polarity = c("positive", "negative"),
                          direction = c("reverse", "forward"), d.spec = "ms2Result/MSMSfigures",
                          width = 20, height = 7, is.include.precursor = TRUE, is.tune.ms2.exp = TRUE,
                          is.tune.ms2.lib = FALSE,
                          ...){
             polarity <- match.arg(polarity)
             direction <- match.arg(direction)
             col.plot <- c(lib = "salmon", exp = "lightseagreen", filtered = "gray")
             for (i in rev(seq(nrow(info.peak.plot)))) {
               cat(i, " ")
               apply(id.peaks.list[[info.peak.plot$idx.peaklist[i]]],
                     1, function(r.id) {
                       id <- as.character(r.id["labid"])
                       score <- round(as.numeric(r.id["score"]), 3)
                       spec.lib <- spec.lib.all <- lib.spec[[id]][[ce]]
                       if (!is.include.precursor) {
                         mz.precursor <- as.numeric(lib.meta[lib.meta[,
                                                                      "labid"] == id, "mz"])
                         nr.remove <- which(spec.lib[, "mz"] == mz.precursor)
                         if (length(nr.remove) > 0) {
                           spec.lib <- spec.lib[-nr.remove, , drop = FALSE]
                         }
                       }
                       if (is.tune.ms2.lib) {
                         spec.lib <- TuneMS2(spec.lib, mz.precursor,
                                             is.include.precursor = is.include.precursor,
                                             ...)
                       }
                       spec.lib.filtered <- spec.lib.all[which(!spec.lib.all[,
                                                                             "mz"] %in% spec.lib[, "mz"]), , drop = FALSE]
                       nr.peaktable <- info.peak.plot$nr.peaktable[i]
                       spec.exp <- spec.exp.all <- ms.assigned[[nr.peaktable]]
                       if (is.tune.ms2.exp) {
                         spec.exp <- TuneMS2(spec.exp, dt.peaktable[nr.peaktable,
                                                                    "mzmed"], is.include.precursor = is.include.precursor,
                                             ...)
                       }
                       spec.exp.filtered <- spec.exp.all[which(!spec.exp.all[,
                                                                             "mz"] %in% spec.exp[, "mz"]), , drop = FALSE]
                       spec2match <- GetSpec2Match(spec.exp, spec.lib,
                                                   direction = direction)
                       nr.matched <- which(spec2match$exp[, "intensity"] >
                                             0 & spec2match$lib[, "intensity"] > 0)
                       spec.matched <- lapply(spec2match, function(spec) {
                         spec[nr.matched, , drop = FALSE]
                       })
                       d.plot <- file.path(d.spec, paste(dt.peaktable[nr.peaktable,
                                                                      "name"], direction, sep = "_"))
                       dir.create(d.plot)
                       cmpd.replaced <- paste(sapply(strsplit(r.id["name"],
                                                              "")[[1]], function(x) {
                                                                switch(x, `:` = "_", `/` = "-", x)
                                                              }), collapse = "")
                       fn.plot <- switch(as.character(length(r.id)),
                                         `3` = file.path(d.plot, paste(score, ",",
                                                                       cmpd.replaced, ".pdf", sep = "")), `4` = file.path(d.plot,
                                                                                                                          paste(score, ",", cmpd.replaced, ",", r.id["adduct"],
                                                                                                                                ".pdf", sep = "")))
                       range.mz <- range(c(spec.lib.all[, "mz"], spec.exp.all[,
                                                                              "mz"]))
                       range.int <- c(-1, 1)
                       pdf(file = fn.plot, height = 7, width = 20,
                           family = "mono")

                       par(mar = c(5,5,4,2))

                       plot(range.mz, range.int, type = "n", main = r.id["name"],
                            xlab = "m/z", ylab = "Relative intensity",  cex.lab = 1.5,
                            cex.axis = 1.3, cex.main = 1.5)

                       abline(h = 0, col = "black")
                       ref.lib <- max(spec.lib.all[, "intensity"])
                       points(NormalizeSpec(spec.lib, ref.lib, "down"),
                              type = "h", col = col.plot["lib"])
                       if (nrow(spec.lib.filtered) > 0) {
                         points(NormalizeSpec(spec = spec.lib.filtered,
                                              ref = ref.lib, pos = "down"), type = "h",
                                col = col.plot["filtered"])
                       }
                       ref.exp <- max(spec.exp.all[, "intensity"])
                       points(NormalizeSpec(spec.exp, ref.exp, "top"),
                              type = "h", col = col.plot["exp"])
                       if (nrow(spec.exp.filtered) > 0) {
                         points(NormalizeSpec(spec = spec.exp.filtered,
                                              ref = ref.exp, pos = "top"), type = "h",
                                col = col.plot["filtered"])
                       }
                       points(NormalizeSpec(spec.matched$lib, ref.lib,
                                            "down"), type = "p", pch = 20, col = col.plot["lib"])
                       points(NormalizeSpec(spec.matched$exp, ref.exp,
                                            "top"), type = "p", pch = 20, col = col.plot["exp"])
                       if (ce == "spec") {
                         legend("bottomleft", legend = c(paste("Name:",
                                                               r.id["name"]), paste("Polarity:", polarity)),
                                pch = NA, bty = "n")
                       }
                       else {
                         legend("bottomleft", legend = c(paste("LabID:",
                                                               r.id["labid"]), paste("Polarity:", polarity),
                                                         paste("CE:", ce)), pch = NA, bty = "n", cex = 1.3)
                       }
                       legend("topleft", cex = 1.3, legend = c(paste("Score:",
                                                                     score), paste("Matched peaks:", c("data",
                                                                                                       "library"))), col = c("white", col.plot[c("exp",
                                                                                                                                                 "lib")]), pch = list(NA, 20, 20), bty = "n")
                       dev.off()
                     })
             }
           })




#-----------------------------------------------------------------------------
setGeneric(name = "removeNoise",
           def = function(spec, mz.tol = 30){
             spec <- matrix(spec, ncol = 2)
             colnames(spec) <- c("mz", "intensity")
             if(nrow(spec) == 1) return(spec)
             spec <- spec[order(spec[,1]),]
             mz <- as.numeric(spec[,1])

             new.spec <- NULL
             i = 1
             while(i < length(mz)){
               # cat(i); cat(" ")
               temp.mz <- mz[i]
               mz.error <- abs(temp.mz - mz)*10^6/ifelse(temp.mz >= 400, temp.mz, 400)
               temp.idx <- which(mz.error <= mz.tol)
               temp.spec <- spec[temp.idx,]
               if(length(temp.idx) == 1) {
                 new.spec <- rbind(new.spec, temp.spec)
                 i <- max(temp.idx) + 1
                 next()
               }
               temp.mz <- median(temp.spec[,1])
               temp.int <- max(temp.spec[,2])
               new.spec <- rbind(new.spec, c(temp.mz, temp.int))
               # spec <- spec[-temp.idx,]
               i <- max(temp.idx) + 1
             }

             row.names(new.spec) <- NULL
             colnames(new.spec) <- c("mz", "intensity")
             return(new.spec)
           })




#-----------------------------------------------------------------------------
# title readMGF
# description Read mgf data.
# author Xiaotao Shen, Yandong Yin
#  \email{shenxt@@sioc.ac.cn}
# param file The vector of names of ms2 files. MS2 file must be mgf.
# return Return ms2 data.
# export

setGeneric(name = "readMGF",
           def = function(file){
             pbapply::pboptions(style = 1)
             # cat("Reading MS2 data (step1)\n")
             # mgf.data.list <- pbapply::pblapply(file, ListMGF)
             ms2 <- pbapply::pblapply(file, function(mgf.data) {
               mgf.data <- ListMGF(mgf.data)
               # nl.spec <- grep('^\\d', mgf.data)
               nl.spec <- lapply(mgf.data, function(x) grep('^\\d', x))
               info.mz <- lapply(mgf.data, function(x) grep('^PEPMASS', x, value = T))
               info.rt <- lapply(mgf.data, function(x) grep('^RTINSECONDS', x, value = T))

               info.mz <- unlist(info.mz)
               #for orbitrap data, the intensity of precursor ion should be removed
               info.mz <- unlist(lapply(strsplit(x = info.mz, split = " "), function(x) x[1]))
               info.mz <- as.numeric(gsub(pattern = "\\w+=", "", info.mz))
               info.rt <- unlist(info.rt)
               info.rt <- as.numeric(gsub(pattern = "\\w+=", "", info.rt))

               spec <- mapply(function(x, y){
                 do.call(rbind, strsplit(x[y], split = " "))
               },
               x = mgf.data,
               y = nl.spec)

               spec <- lapply(spec, function(x){
                 temp <- cbind(as.numeric(x[,1]),as.numeric(x[,2]))
                 temp <- matrix(temp, ncol = 2)
                 if(nrow(temp) > 0) temp <- temp[temp[,2] >= max(temp[,2])*0.01,]
                 temp <- matrix(temp, ncol = 2)
                 colnames(temp) <- c("mz", "intensity")
                 temp
               })

               ms2 <- mapply(function(x,y,z){
                 info <- c(y, z)
                 names(info) <- c("mz", "rt")
                 spectrum <- as.matrix(x)
                 temp <- list(info, spectrum)
                 names(temp) <- c("info", "spec")
                 list(temp)
               },
               x = spec,
               y = info.mz,
               z = info.rt)

               ms2

             })


             spec.info <- ms2[[1]]
             if(length(ms2) > 1){
               for(i in 2:length(ms2)){
                 spec.info <- c(spec.info, ms2[[i]])
               }
             }

             remove.idx <- which(unlist(lapply(spec.info, function(x) nrow(x[[2]]))) == 0)
             if(length(remove.idx) != 0) spec.info <- spec.info[-remove.idx]
             # ##remove noise
             # cat("\n")
             # cat("Remove noise of MS/MS spectra...\n")
             # spec.info <- pbapply::pblapply(spec.info, function(x){
             #   temp.spec <- x[[2]]
             #   temp.spec <- removeNoise(temp.spec)
             #   x[[2]] <- temp.spec
             #   x
             # })

             spec.info <- spec.info
           })


#----------------------------------------------------------------------------
setGeneric(name = "ListMGF",
           def = function(file){
             mgf.data <- readLines(file)
             nl.rec.new <- 1
             idx.rec <- 1
             rec.list <- list()
             for(nl in 1:length(mgf.data))
             {
               if(mgf.data[nl]=="END IONS")
               {
                 rec.list[idx.rec] <- list(Compound = mgf.data[nl.rec.new : nl])
                 nl.rec.new <- nl + 1
                 idx.rec <- idx.rec + 1
               }
             }
             rec.list
           })

setGeneric(name = "CheckInRange", def = function(targets, range){
  targets >= range[1] & targets <= range[2]
})





#-----------------------------------------------------------------------------
# title readMZXML
# description Read mzXML data.
# author Xiaotao Shen
#  \email{shenxt@@sioc.ac.cn}
# param file The vector of names of ms2 files. MS2 file must be mzXML.
# return Return ms2 data.
# export

setGeneric(name = "readMZXML", function(file){
  # cat("Open MS2 file.\n")
  mzxml.data <- lapply(file, function(x) {
    mzR::openMSfile(x)
  })
  # cat("Extract MS2 information\n")
  mzxml.info <- lapply(mzxml.data, function(x){
    mzR::header(x)
  })
  # cat("Extract MS2 spectrum\n")
  pbapply::pboptions(type = "timer", style = 1)
  mzxml.peak <- pbapply::pblapply(mzxml.data, function(x){
    mzR::peaks(x)
  })


  ms2 <- mapply(function(info, peak){
    ms2.idx <- which(info$msLevel == 2)
    ms2.info <- info[ms2.idx, c("precursorMZ", "retentionTime")]
    ms2.info <- apply(ms2.info, 1, list)
    ms2.spec <- peak[ms2.idx]

    temp.ms2 <- mapply(function(x, y){
      names(x[[1]]) <- c("mz", "rt")
      colnames(y) <- c("mz", "intensity")
      if(nrow(y) > 0) y <- y[y[,2] >= max(y[,2])*0.01,]
      y <- matrix(y, ncol = 2)
      temp <- list(x[[1]], y)
      names(temp) <- c("info", "spec")
      list(temp)
    }, x = ms2.info,
    y = ms2.spec)

    list(temp.ms2)
  },
  info = mzxml.info,
  peak = mzxml.peak)

  spec.info <- ms2[[1]]
  if(length(ms2) > 1){
    for(i in 2:length(ms2)){
      spec.info <- c(spec.info, ms2[[i]])
    }
  }


  remove.idx <- which(unlist(lapply(spec.info, function(x) nrow(x[[2]]))) == 0)
  if(length(remove.idx) != 0) spec.info <- spec.info[-remove.idx]

  # ##remove noise
  # cat("\n")
  # cat("Remove noise of MS/MS spectra...\n")
  # spec.info <- pbapply::pblapply(spec.info, function(x){
  # temp.spec <- x[[2]]
  # temp.spec <- removeNoise(temp.spec)
  # x[[2]] <- temp.spec
  # x
  # })

  spec.info <- spec.info
})



#---------------------------------------------------------------------------
#title ReadMSP
#aliases MSP file reader
#description  Read a MSP file and return a list of spectra for all feature
# with feature information
#param file path of the msp file
setGeneric('readMSP', function(file) {
  msp.data <- readLines(file)
  # n.tot <- length(msp.data)
  n.null <- which(msp.data == '')

  temp.idx1 <- c(1, n.null[-length(n.null)])
  temp.idx2 <- n.null - 1

  temp.idx <- data.frame(temp.idx1, temp.idx2, stringsAsFactors = FALSE)
  temp.idx <- apply(temp.idx, 1, list)

  temp.idx <- lapply(temp.idx, unlist)

  # n.spec <- which(grepl('^\\d', msp.data))
  # n.info <- seq(n.tot)[-c(n.spec, n.null)]

  pbapply::pboptions(style = 1)
  info.spec <- pbapply::pblapply(temp.idx, function(idx) {

    temp.msp.data <- msp.data[idx[1]:idx[2]]

    temp.msp.data <- temp.msp.data[temp.msp.data != ""]
    info.idx <- grep("[A-Za-z]", temp.msp.data)
    temp.info <- temp.msp.data[info.idx]
    temp.info <- strsplit(temp.info, split = ":")
    temp.info <- do.call(rbind, temp.info)
    temp.info <- data.frame(temp.info, stringsAsFactors = FALSE)
    temp.info[,2] <- stringr::str_trim(temp.info[,2])
    colnames(temp.info) <- rownames(temp.info) <- NULL
    rownames(temp.info) <- temp.info[,1]
    temp.info <- temp.info[,-1,drop = FALSE]

    temp.spec <- temp.msp.data[-info.idx]

    if(length(temp.spec) != 0){
      if(length(grep(" ", temp.spec[1])) == 1){
        temp.spec <- strsplit(temp.spec, split = ' ')
      }

      if(length(grep("\t", temp.spec[1])) == 1){
        temp.spec <- strsplit(x = temp.spec, split = "\t")
      }

      temp.spec <- do.call(rbind, temp.spec)
      temp.spec <- data.frame(temp.spec, stringsAsFactors = FALSE)
      colnames(temp.spec) <- c('mz', 'intensity')
      rownames(temp.spec) <- NULL
      temp.spec$mz <- as.numeric(as.character(temp.spec$mz))
      temp.spec$intensity <- as.numeric(temp.spec$intensity)
      temp.spec <- temp.spec[temp.spec$intensity != 0,]
    }else{
      temp.spec <- NULL
    }

    list('info' = temp.info,
         'spec' = temp.spec)
  })

  mz.idx <- grep("[Mm][Zz]", rownames(info.spec[[1]][[1]]))
  rt.idx <- grep("Time|TIME|time|RT|rt|Rt", rownames(info.spec[[1]][[1]]))

  ##fix bug in msp data from metAnalyzer
  if(length(rt.idx)==0){
    cat("The msp data are from MetAnalyzer software.\n")
    rt.idx <- grep("NAME|Name|name", rownames(info.spec[[1]][[1]]))
    ##rt.idx is the name of peak
    info.spec <- lapply(info.spec, function(x){
      info <- x[[1]]
      mz <- as.numeric(info[mz.idx, 1])
      rt <- as.character(info[rt.idx, 1])
      info <- c(mz, rt)
      names(info) <- c("mz", "rt")
      x[[1]] <- info
      x
    })
  }else{
    info.spec <- lapply(info.spec, function(x){
      info <- x[[1]]
      mz <- as.numeric(info[mz.idx, 1])
      rt <- as.numeric(info[rt.idx, 1])
      info <- c(mz, rt)
      names(info) <- c("mz", "rt")
      x[[1]] <- info
      x
    })
  }


  remove.idx <- which(unlist(lapply(info.spec, function(x) is.null(x[[2]]))))
  if(length(remove.idx) > 0){
    info.spec <- info.spec[-remove.idx]
  }

  info.spec <- info.spec
})



#------------------------------------------------------------------------------
setGeneric(name = "WriteMSP",
           def = function(info, fn.pre, spec.all){
             fn.save <- paste0(fn.pre, '_spectra.msp')
             #

             sink(fn.save)
             for (idx in seq(nrow(info))) {
               if (!is.null(spec.all[[idx]])) {
                 if (nrow(spec.all[[idx]]) > 0) {
                   mz <- info[idx, 'Mass']
                   spec <- spec.all[[idx]]
                   cat('IDX: ', idx, '\n', sep = '')
                   cat('PRECURSORMZ: ', mz, '\n', sep = '')
                   cat('Num Peaks: ', nrow(spec), '\n', sep = '')
                   for (nr in seq(nrow(spec))) {
                     cat(paste(spec[nr, ], collapse = ' '), '\n', sep = '')
                   }
                   cat('\n')
                 }
               }
             }
             sink()
           })


#------------------------------------------------------------------------------
setGeneric(name = "GetPpmRange",
           def = function(ref, tol){
             ref + c(-1, 1) * ref * tol * 1e-6
           })



setGeneric(name = "LoadData", def = function(file, keep.name = FALSE, env){
  if (missing(env))
    env <- new.env()
  b <- load(file, envir = env)
  if (keep.name | length(b) > 1) {
    r <- lapply(b, function(b1) env[[b1]])
    names(r) <- b
    r
  }
  else {
    env[[b]]
  }
})






###IdentifyFeature
#'@title IdentifyFeature
#'@description IdentifyFeature
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param spec.exp spec.exp.
#'@param spec.lib spec.lib.
#'@param is.include.precursor is.include.precursor.
#'@param is.tune.ms2.lib is.tune.ms2.lib.
#'@param direction direction.
#'@param ... other parameters
#'@export

IdentifyFeature <- function(spec.exp,
                            spec.lib,
                            is.include.precursor = FALSE,
                            is.tune.ms2.lib = FALSE,
                            direction = "forward",
                            ...) {
  # ()
  if (is.tune.ms2.lib) {
    spec.lib <- TuneMS2(spec.lib, is.include.precursor = is.include.precursor, ...)
  }
  if (length(spec.lib) == 0) {
    return(NA)
  }


  return(GetMatchResult(spec.exp, spec.lib, direction = direction, ...))
}

#-----------------------------------------------------------------------------
###
###GetMatchResult
#'@title GetMatchResult
#'@description GetMatchResult
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param spec.exp spec.exp
#'@param spec.lib spec.lib
#'@param weight.int weight.int
#'@param weight.mz weight.mz
#'@param ppm.ms2match ppm.ms2match
#'@param mz.ppm.thr mz.ppm.thr
#'@param ppm.sanity.check ppm.sanity.check
#'@param is.sanity.check is.sanity.check
#'@param direction direction
#'@param ... other parameters
#'@export
#'

GetMatchResult <- function(spec.exp, spec.lib,
                           weight.int = 1,
                           weight.mz = 0,
                           ppm.ms2match = 30,
                           mz.ppm.thr = 400,
                           ppm.sanity.check = 100,
                           is.sanity.check = FALSE,
                           direction = direction,
                           ...) {
  # ()
  if (is.sanity.check) {
    switch(direction,
           'forward' = {
             if (any(c(GetDiffMZppm(spec.exp[, 'mz']), GetDiffMZppm(spec.lib[, 'mz'])) <= ppm.sanity.check)) {
               stop('Difference between m/z is too small!!')
             }
           },
           'reverse' = {
             if (any(GetDiffMZppm(spec.lib) <= ppm.sanity.check)) {
               stop('Difference between m/z is too small!!')
             }
           },
           stop('Error setup for parameter: direction!!!'))
  }
  #
  spec2match <- GetSpec2Match(spec.exp, spec.lib,
                              ppm.ms2match = ppm.ms2match,
                              mz.ppm.thr = mz.ppm.thr,
                              direction = direction)
  int.weighted.pk  <- GetWeightedInt(spec2match$exp, weight.mz, weight.int)
  int.weighted.lib <- GetWeightedInt(spec2match$lib, weight.mz, weight.int)
  match.score <- GetDotProduct(int.weighted.pk, int.weighted.lib)
  attr(match.score, 'spec') <- spec2match
  attr(match.score, 'spec.compared') <- cbind('exp' = int.weighted.pk,
                                              'lib' = int.weighted.lib)
  return(match.score)
}



#-----------------------------------------------------------------------------
###GetSpec2Match
#'@title GetSpec2Match
#'@description GetSpec2Match
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param spec.exp spec.exp
#'@param spec.lib spec.lib
#'@param ppm.ms2match ppm.ms2match
#'@param mz.ppm.thr mz.ppm.thr
#'@param direction direction
#'@export
#'

GetSpec2Match <- function(spec.exp, spec.lib,
                          ppm.ms2match = 30,
                          mz.ppm.thr = 400,
                          direction = c('reverse', 'forward')) {
  #
  direction = match.arg(direction)
  mz.pool   <- sort(c(spec.exp[, 'mz'], spec.lib[, 'mz']))
  spec.temp <- cbind('mz' = mz.pool, 'intensity' = 0)

  spec.exp.temp  <- MatchFromTemp(spec.exp, spec.temp)
  spec.lib.temp <- MatchFromTemp(spec.lib, spec.temp)

  # combine nearby peaks
  pk.spec  <- MatchSpec(spec.exp.temp,  ppm.ms2match = ppm.ms2match, mz.ppm.thr = mz.ppm.thr)
  lib.spec <- MatchSpec(spec.lib.temp, ppm.ms2match = ppm.ms2match, mz.ppm.thr = mz.ppm.thr)

  if (direction == 'reverse') {
    idx.rm <- which(lib.spec[, 'intensity'] == 0)
    if (length(idx.rm > 0)) {
      pk.spec  <- pk.spec[-idx.rm, , drop = FALSE]
      lib.spec <- lib.spec[-idx.rm, , drop = FALSE]
    }
  }

  return(list('exp' = pk.spec, 'lib' = lib.spec))
}



#-----------------------------------------------------------------------------
###MatchFromTemp
#'@title MatchFromTemp
#'@description MatchFromTemp
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param spec spec
#'@param temp temp
#'@export
#'

MatchFromTemp <- function(spec, temp) {
  temp[match(spec[, 'mz'], temp[, 'mz']), 'intensity'] <- spec[, 'intensity']
  temp
}




#-----------------------------------------------------------------------------
###MatchSpec
#'@title MatchSpec
#'@description MatchSpec
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param spec spec
#'@param ppm.ms2match ppm.ms2match
#'@param mz.ppm.thr mz.ppm.thr
#'@export
#'


MatchSpec <- function(spec, ppm.ms2match = 30, mz.ppm.thr = 400) {
  while (TRUE) {
    mz.diff.ppm <- GetDiffMZppm(spec[, 'mz'], mz.ppm.thr = mz.ppm.thr)
    idx <- which(mz.diff.ppm < ppm.ms2match)
    if (length(idx) > 0) {
      i <- tail(idx, 1)
      j <- which.max(spec[c(i, i + 1), 'intensity'])
      spec[i, 'intensity'] <- spec[i + j - 1, 'intensity']
      i2 <- i + 1
      spec[i, 'mz'] <- spec[i2, 'mz']
      spec <- spec[-i - 1, , drop = FALSE]
    } else {
      break
    }
  }
  return(spec)
}



#-----------------------------------------------------------------------------
#'@title GetDiffMZppm
#'@description GetDiffMZppm
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param mz mz
#'@param mz.ppm.thr mz.ppm.thr
#'@export

GetDiffMZppm <- function(mz, mz.ppm.thr = NULL) {
  mz.diff <- diff(mz) / mz[-1] * 1e6
  if (!is.null(mz.ppm.thr)) {
    idx <- which(mz[-1] <= mz.ppm.thr)
    mz.diff[idx] <- mz.diff[idx] * mz[-1] / mz.ppm.thr
  }
  mz.diff
}



#-----------------------------------------------------------------------------
#'@title GetWeightedInt
#'@description GetWeightedInt
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param spec spec
#'@param weight.mz weight.mz
#'@param weight.int weight.int
#'@export
#'


GetWeightedInt <- function(spec, weight.mz = 0, weight.int = 1) {
  return(spec[, 'mz'] ^ weight.mz * spec[, 'intensity'] ^ weight.int)
}


#-----------------------------------------------------------------------------
#'@title GetDotProduct
#'@description GetDotProduct
#'@author Hao Li, Yandong Yin, Kai Weng
#'\email{shenxt@@sioc.ac.cn}
#'@param x x
#'@param y y
#'@export

GetDotProduct <- function(x, y) {
  # return(sum(x * y) ^ 2 / (sum(x ^ 2) * sum(y ^ 2)))
  return(sum(x * y) / sqrt((sum(x ^ 2) * sum(y ^ 2))))
}


#-----------------------------------------------------------------------------

TuneMS2 <- function(spec,
                    mz.precursor,
                    is.include.precursor = TRUE,
                    snthr = 3,
                    noise.ms2 = 3,
                    mz.range.ms2 = NULL,
                    int.ms2.min.abs,
                    int.ms2.min.relative = 0.01,
                    is.apply.ms2.min.relative = TRUE,
                    is.check.sanity = TRUE,
                    int.check.sanity = 50,
                    ppm.precursor.filter = 20, ...) {
  # re-orgnize spec with increasing mz
  spec <- spec[order(spec[, 'mz']), , drop = FALSE]
  # define the lowest non-noise signal
  if (missing(int.ms2.min.abs)) {
    int.ms2.min.abs <- noise.ms2 * snthr
  }
  #
  # provent noise over estimation
  if (is.check.sanity & int.ms2.min.abs > int.check.sanity) {
    stop('Noise estimation is too high!')
  }

  # considering precursor ion
  if (missing(mz.precursor)) {
    mz.precursor <- max(spec[, 'mz'])
  }
  mz.precursor.range <- GetRangePPM(mz.precursor, ppm.precursor.filter)
  idx.mz.precursor.range <- ifelse(is.include.precursor, 2, 1)
  mz.cutoff <- mz.precursor.range[idx.mz.precursor.range]
  spec <- spec[spec[,'mz'] < mz.cutoff, , drop = FALSE]

  if (nrow(spec) == 0) {
    return()
  }

  if (!is.null(mz.range.ms2)) {
    nr.keep <- which(spec[, 'mz'] >= mz.range.ms2[1] &
                       spec[, 'mz'] <= mz.range.ms2[2])
    if (length(nr.keep) > 0) {
      spec <- spec[nr.keep, , drop = FALSE]
    }
    else {
      return()
    }
  }

  if ((max(spec[, 'intensity']) * int.ms2.min.relative) < int.ms2.min.abs) {
    is.warning.lowspec <- TRUE
  }

  # discarding low intensity spec (1% highest int and int.ms2.min.abs)
  int.cutoff <- max(max(spec[, 'intensity']) * int.ms2.min.relative,
                    int.ms2.min.abs)
  spec <- spec[spec[, 'intensity'] >= int.cutoff, , drop = FALSE]
  if (nrow(spec) == 0) {
    return()
  }

  # discarding ring effects
  spec <- RemoveRingEffect(spec)
}


#-----------------------------------------------------------------------------

RemoveRingEffect <- function(spec, mz.diff.thr = 0.3, int.rel.thr = 0.2) {
  nr.ring <- nrow(spec) + 1
  mz <- spec[, 'mz']

  mz.diff <- diff(mz)
  idx.mzdiff <- which(mz.diff <= mz.diff.thr)
  if (length(idx.mzdiff) == 0) {
    return(spec)
  }

  nr.ring.possible <- unique(c(idx.mzdiff, idx.mzdiff + 1))
  while (TRUE) {

    idx.int.max <- which.max(spec[nr.ring.possible, 2])
    nr.int.max <- nr.ring.possible[idx.int.max]
    int.thr <- spec[nr.int.max, 2] * int.rel.thr

    mz.diff <- abs(mz[nr.ring.possible[-idx.int.max]] - mz[nr.int.max])
    int <- spec[nr.ring.possible[-idx.int.max], 2]
    nr.ring <- append(nr.ring, nr.ring.possible[-idx.int.max][which(mz.diff <= mz.diff.thr & int <= int.thr)])
    nr.ring.possible <- nr.ring.possible[!nr.ring.possible %in% c(nr.ring, nr.int.max)]
    if (length(nr.ring.possible) == 0) {
      break
    }
  }

  return(spec[-nr.ring, , drop = FALSE])
}


#-----------------------------------------------------------------------------

NormalizeSpec <- function(spec, ref = max(abs(spec[, 2])), pos = 'top') {
  spec[, 2] <- spec[, 2] / ref * ifelse(pos == 'down', -1, 1)
  return(spec)
}



#-----------------------------------------------------------------------------

GetRangePPM <- function(data, ppm) {
  t(sapply(data, function(dt) dt * (1 + c(-1, 1) * ppm * 1e-6)))
}
#-----------------------------------------------------------------------------

# PlotIDResults <- function(id.peaks.list, info.peak.plot,
#                           ms.assigned, dt.peaktable,
#                           lib.spec,
#                           lib.meta,
#                           ce,
#                           polarity = c('positive', 'negative'),
#                           direction = c('reverse', 'forward'),
#                           d.spec = 'ms2Result/MSMSfigures',
#                           width = 20, height = 7,
#                           is.include.precursor = TRUE,
#                           is.tune.ms2.exp = TRUE,
#                           is.tune.ms2.lib = FALSE,
#                           ...) {
#   polarity <- match.arg(polarity)
#   direction <- match.arg(direction)
#   col.plot <- c( 'lib' = 'red', 'exp' = 'blue', 'filtered' = 'gray')
#
#   for (i in rev(seq(nrow(info.peak.plot)))) {
#     cat(i); cat(" ")
#     apply(id.peaks.list[[info.peak.plot$idx.peaklist[i]]], 1, function(r.id) {
#       id <- as.character(r.id['labid'])
#       score <- round(as.numeric(r.id['score']), 3)
#       spec.lib <- spec.lib.all <- lib.spec[[id]][[ce]]
#       if (!is.include.precursor) {
#         mz.precursor <- as.numeric(lib.meta[lib.meta[, 'labid'] == id, 'mz'])
#         nr.remove <- which(spec.lib[, 'mz'] == mz.precursor)
#         spec.lib <- spec.lib[-nr.remove, , drop = FALSE]
#       }
#       if (is.tune.ms2.lib) {
#         spec.lib <- TuneMS2(spec.lib, mz.precursor,
#                             is.include.precursor = is.include.precursor,
#                             ...)
#       }
#
#       spec.lib.filtered <- spec.lib.all[
#         which(!spec.lib.all[, 'mz'] %in% spec.lib[, 'mz']), ,
#         drop = FALSE]
#
#       nr.peaktable <- info.peak.plot$nr.peaktable[i]
#       spec.exp <- spec.exp.all <- ms.assigned[[nr.peaktable]]
#       if (is.tune.ms2.exp) {
#         spec.exp <- TuneMS2(spec.exp, dt.peaktable[nr.peaktable, 'mzmed'],
#                             is.include.precursor = is.include.precursor,
#                             ...)
#       }
#       spec.exp.filtered <- spec.exp.all[
#         which(!spec.exp.all[, 'mz'] %in% spec.exp[, 'mz']), ,
#         drop = FALSE]
#
#       spec2match <- GetSpec2Match(spec.exp, spec.lib, direction = direction)
#
#       nr.matched <- which(spec2match$exp[, 'intensity'] > 0 &
#                             spec2match$lib[, 'intensity'] > 0)
#       spec.matched <- lapply(spec2match, function(spec) {
#         spec[nr.matched, , drop = FALSE]
#       })
#
#       d.plot <- file.path(d.spec, paste(dt.peaktable[nr.peaktable, 'name'], direction, sep = '_'))
#       dir.create(d.plot)
#       cmpd.replaced <- paste(
#         sapply(strsplit(r.id['name'], '')[[1]], function(x) {
#           # switch(x, ':' = '：', '/' = '／', x)
#           switch(x, ':' = '_', '/' = '-', x)
#         }),
#         collapse = '')
#
#       fn.plot <- switch(as.character(length(r.id)),
#                         '3' = file.path(d.plot,
#                                         paste(score, ',',
#                                               cmpd.replaced, '.pdf',
#                                               sep = ''))
#                         ,
#                         '4' = file.path(d.plot,
#                                         paste(score, ',',
#                                               cmpd.replaced, ',',
#                                               r.id['adduct'], '.pdf',
#                                               sep = ''))
#       )
#       range.mz <- range(c(spec.lib.all[, 'mz'], spec.exp.all[, 'mz']))
#       range.int <- c(-1, 1)
#
#       pdf(file = fn.plot, height = 7, width = 20, family = 'mono')
#       plot(range.mz, range.int, type = 'n', main = r.id['name'],
#            xlab = 'm/z', ylab = 'Relative intensity')
#       abline(h = 0, col = 'black')
#
#       ref.lib <- max(spec.lib.all[, 'intensity'])
#       points(NormalizeSpec(spec.lib, ref.lib, 'down'),
#              type = 'h', col = col.plot['lib'])
#       if (nrow(spec.lib.filtered) > 0) {
#         points(NormalizeSpec(spec = spec.lib.filtered,
#                              ref = ref.lib,
#                              pos = 'down'),
#                type = 'h', col = col.plot['filtered'])
#       }
#       ref.exp <- max(spec.exp.all[, 'intensity'])
#
#       points(NormalizeSpec(spec.exp, ref.exp, 'top'),
#              type = 'h', col = col.plot['exp'])
#       if (nrow(spec.exp.filtered) > 0) {
#         points(NormalizeSpec(spec = spec.exp.filtered,
#                              ref = ref.exp,
#                              pos = 'top'),
#                type = 'h', col = col.plot['filtered'])
#       }
#
#       points(NormalizeSpec(spec.matched$lib, ref.lib, 'down'),
#              type = 'p', pch = 20, col = col.plot['lib'])
#       points(NormalizeSpec(spec.matched$exp, ref.exp, 'top'),
#              type = 'p', pch = 20, col = col.plot['exp'])
#       if (ce == 'spec') {
#         legend('bottomleft',
#                legend = c(paste('Name:', r.id['name']),
#                           paste('Polarity:', polarity)),
#                pch = NA, bty = 'n')
#       } else {
#         legend('bottomleft',
#                legend = c(paste('LabID:', r.id['labid']),
#                           paste('Polarity:', polarity),
#                           paste('CE:', ce)),
#                pch = NA, bty = 'n')
#       }
#
#       legend('topleft',
#              legend = c(paste('Score:', score),
#                         paste('Matched peaks:', c('data', 'library'))),
#              col = c('white', col.plot[c('exp', 'lib')]),
#              pch = list(NA, 20, 20),
#              bty = 'n')
#       dev.off()
#     })
#   }
# }








setGeneric(name = "readAnnotation",
           def = function(data = annotation.result,
                          rt.filter = FALSE,
                          rt.tol = 30,
                          inHouse.compound = inhouse.compound){
             options(warn = -1)

             data <- as.data.frame(data)

             tags.idx <- match(c("name", "mz", "rt", "nhits.reverse",
                                 "hits.reverse", "nhits.forward",
                                 "hits.forward"), colnames(data))
             tags.idx <- tags.idx[!is.na(tags.idx)]
             tags <- data[,tags.idx]
             # sample <- data[,-tags.idx]

             rm(list = "data")
             gc()

             hit.reverse <- tags$hits.reverse
             idx1 <- which(!is.na(hit.reverse))

             hit.reverse1 <- hit.reverse[idx1]
             peak.rt <- tags$rt[idx1]
             peak.mz <- tags$mz[idx1]

             rm(list = c("hit.reverse"))
             gc()

             hit.reverse2 <- lapply(hit.reverse1,
                                    function(x) {strsplit(x, split = ";")[[1]]})

             rm(list = c("hit.reverse1"))
             gc()

             labid2 <- lapply(hit.reverse2, function(x) {
               y <- stringr::str_extract(string = x,
                                         pattern = "labid\\{[a-zA-Z0-9]+\\}")
               y <- gsub(pattern = "labid\\{", replacement = "", x = y)
               y <- gsub(pattern = "\\}", replacement = "", x = y)
               y
             })

             adduct2 <- stringr::str_extract_all(string = hit.reverse2,
                                                 pattern = "adduct\\{[\\(0-9a-zA-Z\\+\\-]+")
             adduct2 <-
               lapply(adduct2, function(x) {
                 gsub(pattern = "adduct\\{", replacement = "", x = x)})
             adduct2 <-
               lapply(adduct2, function(x) {
                 gsub(pattern = "\\(", replacement = "", x = x)})

             # ##remove M- adduct
             # mapply(function(x, y){
             # temp.index <- which(y != "M-")
             # if(length(temp.index) == 0) return(NA)
             # temp.index
             # },
             # x = labid2,
             # y = adduct2)

             ##remove wrong annotation, for examle n01170 (C18H22N2) can't
             ## -H2O

             right.idx <- mapply(function(x, y) {
               temp.idx <- match(x, inHouse.compound$Lab.ID)
               temp.formula <- inHouse.compound$Formula[temp.idx]
               temp.formula1 <- mapply(function(x, y){sumFormula(x,y)},
                                       x = temp.formula,
                                       y = y)
               which(!is.na(temp.formula1))
             },
             x = labid2,
             y = adduct2)

             ##fix bugs
             # for(i in 1:length(labid2)){
             #   cat(i); cat(" ")
             #   x <- labid2[[i]]
             #   y <- adduct2[[i]]
             #   temp.idx <- match(x, inHouse.compound$Lab.ID)
             #   temp.formula <- inHouse.compound$Formula[temp.idx]
             #   temp.formula1 <- mapply(function(x, y){sumFormula(x,y)},
             #                           x = temp.formula,
             #                           y = y)
             #
             # }


             remove.idx <- which(unlist(lapply(right.idx, length)) == 0)
             if(length(remove.idx) != 0){
               right.idx <- right.idx[-remove.idx]
               hit.reverse2 <- hit.reverse2[-remove.idx]
               labid2 <- labid2[-remove.idx]
               adduct2 <- adduct2[-remove.idx]
               idx1 <- idx1[-remove.idx]
             }
             #------------------------------------------------------------------------------
             ##fix bugs
             # for(i in 1:length(labid2)){
             #   cat(i); cat(" ")
             #   x <- labid2[[i]]
             #   y <- adduct2[[i]]
             #   temp.idx <- match(x, inHouse.compound$Lab.ID)
             #   temp.formula <- inHouse.compound$Formula[temp.idx]
             #   temp.formula1 <- mapply(function(x, y){sumFormula(x,y)},
             #                           x = temp.formula,
             #                           y = y)
             # }
             #------------------------------------------------------------------------------


             hit.reverse2 <- mapply(function(x, y) {
               x[y]
             },
             x = hit.reverse2,
             y = right.idx)

             labid2 <- mapply(function(x, y) {
               x[y]
             },
             x = labid2,
             y = right.idx)

             adduct2 <- mapply(function(x, y) {
               x[y]
             },
             x = adduct2,
             y = right.idx)

             rm(list = c("adduct2"))
             gc()

             if(!rt.filter){
               standard.rt <- lapply(labid2, function(x) {
                 rep(NA, length(x))
               })

               rt.error2 <- mapply(function(x,y) {
                 error <- abs(y - x)*100/y
               }, x = peak.rt, y = standard.rt)


               index <- mapply(function(x,y) {
                 idx <- abs(y - x)*100/y
                 idx[is.na(idx)] <- 0
                 idx <- sapply(idx, function(x) {ifelse(x > rt.tol, FALSE, TRUE)})
                 idx
               }, x = peak.rt, y = standard.rt)
             }else{
               standard.rt <- lapply(labid2, function(x) {
                 # temp.idx <- match(x, rt.all$`in house ID`)
                 temp.idx <- match(x, inHouse.compound$Lab.ID)
                 # temp.rt <- rt.all$`RT （min）`[temp.idx]
                 temp.rt <- inHouse.compound$RT[temp.idx]
                 temp.rt
               })

               rt.error2 <- mapply(function(x,y) {
                 error <- abs(y - x)*100/y
               }, x = peak.rt, y = standard.rt)

               index <- mapply(function(x,y) {
                 idx <- abs(y - x)*100/y
                 idx[is.na(idx)] <- rt.tol+1
                 idx <- sapply(idx, function(x) {ifelse(x > rt.tol, FALSE, TRUE)})
                 idx
               }, x = peak.rt, y = standard.rt)
             }

             rm(list = c("peak.rt"))
             gc()

             hit.reverse3 <-
               mapply(function(x,y){x[y]}, x = hit.reverse2, y = index)

             rm(list = c("hit.reverse2"))
             gc()

             hit.reverse4 <-
               lapply(hit.reverse3, function(x) {paste(x, collapse = ";")})
             hit.reverse4 <- unlist(hit.reverse4)

             rm(list = c("hit.reverse3"))
             gc()

             rt.error3 <- mapply(function(x,y){x[y]}, x = rt.error2, y = index)
             rm(list = c("rt.error2"))
             gc()
             rt.error4 <-
               lapply(rt.error3, function(x) {paste(x, collapse = ";")})
             rt.error4 <- unlist(rt.error4)
             rm(list = c("rt.error3"))
             gc()

             ms2.sim3 <-
               stringr::str_extract_all(string = hit.reverse4,
                                        pattern = "score\\{0\\.[0-9]+\\}|score\\{1\\}")
             ms2.sim3 <-
               lapply(ms2.sim3, function(x) {
                 gsub(pattern = "score\\{", replacement = "", x = x)})
             ms2.sim3 <-
               lapply(ms2.sim3, function(x) {
                 gsub(pattern = "\\}", replacement = "", x = x)})
             ms2.sim4 <-
               lapply(ms2.sim3, function(x) {paste(x, collapse = ";")})
             ms2.sim4 <- unlist(ms2.sim4)

             rm(list = c("ms2.sim3"))
             gc()

             adduct3 <- stringr::str_extract_all(string = hit.reverse4,
                                                 pattern = "adduct\\{[\\(0-9a-zA-Z\\+\\-]+")
             adduct3 <-
               lapply(adduct3, function(x) {
                 gsub(pattern = "adduct\\{", replacement = "", x = x)})
             adduct3 <-
               lapply(adduct3, function(x) {
                 gsub(pattern = "\\(", replacement = "", x = x)})
             adduct4 <- lapply(adduct3, function(x) {paste(x, collapse = ";")})
             adduct4 <- unlist(adduct4)

             name3 <-
               stringr::str_extract_all(string = hit.reverse4,
                                        pattern = "name\\{[^\\{]+\\}")

             rm(list = c("hit.reverse4"))
             gc()

             name3 <- lapply(name3, function(x) {
               gsub(pattern = "name\\{", replacement = "", x = x)})
             name3 <- lapply(name3, function(x) {
               gsub(pattern = "\\}", replacement = "", x = x)})
             name4 <- lapply(name3, function(x) {paste(x, collapse = ";")})
             name4 <- unlist(name4)
             rm(list = "name3")
             gc()

             labid3 <- mapply(function(x,y){x[y]}, x = labid2, y = index)
             rm(list = "labid2")
             gc()
             labid4 <- lapply(labid3, function(x) {paste(x, collapse = ";")})
             labid4 <- unlist(labid4)

             ##kegg.id
             KEGG.ID3 <- lapply(labid3, function(x){
               inHouse.compound$KEGG.ID[match(x, inHouse.compound$Lab.ID)]
             })

             ##if is more than 1 ID, remain first ID
             KEGG.ID3 <- lapply(KEGG.ID3, function(x){
               id.len <- nchar(x)
               id.len[is.na(id.len)] <- 6
               temp.idx <- which(id.len > 6)
               if(length(temp.idx) > 0){
                 x[temp.idx] <-
                   unlist(lapply(strsplit(x[temp.idx], split = "/"), function(x){
                     x[[1]]}
                   ))}
               x
             })

             KEGG.ID4 <- lapply(KEGG.ID3, function(x) {
               paste(x, collapse = ";")
             })
             KEGG.ID4 <- unlist(KEGG.ID4)
             KEGG.ID4[grep('NA', KEGG.ID4)] <- NA
             rm(list = c("KEGG.ID3"))
             gc()
             ###calculate mz error
             accurate.mass3 <- mapply(function(x, y) {
               temp.idx <- match(x, inHouse.compound$Lab.ID)
               temp.formula <- inHouse.compound$Formula[temp.idx]
               temp.formula1 <- mapply(function(x, y){sumFormula(x,y)},
                                       x = temp.formula,
                                       y = y)
               a.mass <- sapply(temp.formula1, function(x) {
                 Rdisop::getMass(Rdisop::getMolecule(formula = x))})
               a.mass
             },
             x = labid3,
             y = adduct3)

             rm(list = c("adduct3"))
             gc()
             # for(i in 1:length(labid3)){
             #   cat(i); cat(" ")
             # x <- labid3[[i]]
             # y <- adduct3[[i]]
             # temp.formula <- inHouse.compound$Formula[match(x, inHouse.compound$lab.ID)]
             # temp.formula1 <- mapply(function(x, y){sumFormula(x,y)},
             #                         x = temp.formula,
             #                         y = y)
             # a.mass <- sapply(temp.formula1, function(x) {
             #   Rdisop::getMass(Rdisop::getMolecule(formula = x))})
             # }


             mz.error3 <- mapply(function(x, y) {
               if(is.list(x)) x <- NA
               # error <- abs(x - y)*10^6/y
               error <- abs(x - y)*10^6/ifelse(y>=400,y,400)
               unname(error)
             },
             x = accurate.mass3,
             y = peak.mz)

             rm(list = c("peak.mz"))
             gc()
             ##should be fixed
             # mz.error3 <- lapply(mz.error3, function(x){
             #   x[!is.na(x)] <- 10
             #   x
             # })

             mz.error4 <-
               lapply(mz.error3, function(x) {paste(x, collapse = ";")})
             mz.error4 <- unlist(mz.error4)
             mz.error4[mz.error4=="NA"] <- ""


             #formula
             Formula3 <- lapply(labid3, function(x){
               inHouse.compound$Formula[match(x, inHouse.compound$Lab.ID)]
             })

             rm(list = c("labid3"))
             gc()

             Formula4 <- lapply(Formula3,  function(x) {
               paste(x, collapse = ";")
             })
             Formula4 <- unlist(Formula4)

             tags <- as.data.frame(tags)
             tags <- tags[,c("name","mz","rt")]
             colnames(tags) <- c("Peak.name","mz","rt")
             ms2.sim <- rep(NA, nrow(tags))
             adduct <- rep(NA, nrow(tags))
             rt.error <- rep(NA, nrow(tags))
             mz.error <- rep(NA, nrow(tags))
             Metabolite.name <- rep(NA, nrow(tags))
             KEGG.ID <- rep(NA, nrow(tags))
             Formula <- rep(NA, nrow(tags))
             isotope <- rep(NA, nrow(tags))
             labid <- rep(NA, nrow(tags))

             ms2.sim[idx1] <- ms2.sim4
             adduct[idx1] <- adduct4
             rt.error[idx1] <- rt.error4
             mz.error[idx1] <- mz.error4
             Metabolite.name[idx1] <- name4
             KEGG.ID[idx1] <- KEGG.ID4
             Formula[idx1] <- Formula4
             labid[idx1] <- labid4
             isotope[idx1] <- "[M]"

             rm(list = c("labid4", "adduct4", "ms2.sim4", "rt.error4",
                         "mz.error4", "name4", "KEGG.ID4", "Formula4"))
             gc()

             tags <- data.frame(tags, adduct, isotope, ms2.sim, rt.error, mz.error,
                                Formula, Metabolite.name, KEGG.ID, labid,
                                stringsAsFactors = FALSE)
             rm(list = c("adduct", "isotope", "ms2.sim", "rt.error", "mz.error",
                         "Formula", "Metabolite.name", "KEGG.ID", "labid"))
             gc()

             tags[which(tags == "", arr.ind = TRUE)] <- NA

             cat("readAnnotation is done!\n")
             tags

             # ##change tags to tags2 style
             # result <- list(tags, sample)
             # names(result) <- c("tags", "sample")
             # result <- result
           })



###MS2 data processing
tempFunction <- function(){

}





################################################################################

setGeneric(name = "removeByRT",
           def = function(data,
                          rt.data,
                          rt.tol = 10,#second
                          direction = c("less", "bigger")
           ){
             direction <- match.arg(direction)
             tags.idx <- match(c("name", "mz", "rt", "nhits.reverse",
                                 "hits.reverse", "nhits.forward",
                                 "hits.forward"), colnames(data))
             tags <- data[,tags.idx]
             sample <- data[,-tags.idx]

             #####reverse
             hit.reverse <- tags$hits.reverse
             idx.reverse <- which(!is.na(hit.reverse))

             new.hit.reverse <- hit.reverse[idx.reverse]
             reverse.rt <- tags$rt[idx.reverse]

             new.hit.reverse <- lapply(new.hit.reverse,
                                       function(x) {strsplit(x, split = ";")[[1]]})

             reverse.labid <- lapply(new.hit.reverse, function(x) {
               y <- stringr::str_extract(string = x,
                                         pattern = "labid\\{[a-zA-Z0-9]+\\}")
               y <- gsub(pattern = "labid\\{", replacement = "", x = y)
               y <- gsub(pattern = "\\}", replacement = "", x = y)
               y
             })

             standard.reverse.rt <- lapply(reverse.labid, function(x){
               rt.data$RT[match(x, rt.data$labID)]
             })


             reverse.rt.error <- mapply(function(x, y){
               abs(x - y)
             },
             x = reverse.rt,
             y = standard.reverse.rt)


             if(direction == "less"){
               temp.index <- lapply(reverse.rt.error, function(x){
                 x[is.na(x)] <- rt.tol + 100
                 which(x <= rt.tol)
               })
             }else{
               temp.index <- lapply(reverse.rt.error, function(x){
                 x[is.na(x)] <- rt.tol - 100
                 which(x > rt.tol)
               })
             }



             new.hit.reverse <- mapply(function(x, y){
               if(length(y) == 0) return(NA)
               x[y]
             },
             x = new.hit.reverse,
             y = temp.index)

             new.hit.reverse <- unlist(lapply(new.hit.reverse, function(x){
               paste(x, collapse = ";")
             }))

             new.hit.reverse[new.hit.reverse == "NA"] <- NA
             hit.reverse[idx.reverse] <- new.hit.reverse



             ##############################################################
             #####forward
             hit.forward <- tags$hits.forward
             idx.forward <- which(!is.na(hit.forward))

             new.hit.forward <- hit.forward[idx.forward]
             forward.rt <- tags$rt[idx.forward]

             new.hit.forward <- lapply(new.hit.forward,
                                       function(x) {strsplit(x, split = ";")[[1]]})

             forward.labid <- lapply(new.hit.forward, function(x) {
               y <- stringr::str_extract(string = x,
                                         pattern = "labid\\{[a-zA-Z0-9]+\\}")
               y <- gsub(pattern = "labid\\{", replacement = "", x = y)
               y <- gsub(pattern = "\\}", replacement = "", x = y)
               y
             })

             standard.forward.rt <- lapply(forward.labid, function(x){
               rt.data$RT[match(x, rt.data$labID)]
             })


             forward.rt.error <- mapply(function(x, y){
               abs(x - y)
             },
             x = forward.rt,
             y = standard.forward.rt)


             if(direction == "less"){
               temp.index <- lapply(forward.rt.error, function(x){
                 x[is.na(x)] <- rt.tol + 100
                 which(x <= rt.tol)
               })
             }else{
               temp.index <- lapply(forward.rt.error, function(x){
                 x[is.na(x)] <- rt.tol - 100
                 which(x > rt.tol)
               })
             }


             new.hit.forward <- mapply(function(x, y){
               if(length(y) == 0) return(NA)
               x[y]
             },
             x = new.hit.forward,
             y = temp.index)

             new.hit.forward <- unlist(lapply(new.hit.forward, function(x){
               paste(x, collapse = ";")
             }))

             new.hit.forward[new.hit.forward == "NA"] <- NA
             hit.forward[idx.forward] <- new.hit.forward


             tags$hits.reverse <- hit.reverse
             tags$hits.forward <- hit.forward

             data <- data.frame(tags, sample, stringsAsFactors = FALSE)
             return(data)
           })





################################################################################

setGeneric(name = "removeByDP",
           def = function(data,
                          dp.cutoff = 0.8,#second
                          direction = c("less", "bigger")
           ){
             direction <- match.arg(direction)
             tags.idx <- match(c("name", "mz", "rt", "nhits.reverse",
                                 "hits.reverse", "nhits.forward",
                                 "hits.forward"), colnames(data))
             tags <- data[,tags.idx]
             sample <- data[,-tags.idx]

             #####reverse
             hit.reverse <- tags$hits.reverse

             reverse.dp <- lapply(hit.reverse, function(x){
               if(is.na(x)) return(NA)
               temp.score <- stringr::str_extract_all(string = x, pattern = "score\\{[0-9\\.]{0,30}\\}")
               unlist(lapply(temp.score, function(x){
                 as.numeric(gsub(pattern = "score\\{|\\}", replacement = "", x = x))
               }))
             })



             new.hit.reverse <- lapply(hit.reverse,
                                       function(x) {strsplit(x, split = ";")[[1]]})



             reverse.index <- lapply(reverse.dp, function(x){
               if(is.na(x)) return(NA)
               if(direction == "less"){
                 temp.idx <- which(x <= dp.cutoff)
               }else{
                 temp.idx <- which(x >= dp.cutoff)
               }
               if(length(temp.idx)==0) return(NA)
               temp.idx
             })

             new.hit.reverse <- mapply(function(x, y){
               if(is.na(y)) return(NA)
               x[y]
             },
             x = new.hit.reverse,
             y = reverse.index)

             new.hit.reverse <- unlist(lapply(new.hit.reverse, function(x){
               paste(x, collapse = ";")
             }))

             new.hit.reverse[new.hit.reverse == "NA"] <- NA



             ##############################################################
             #####forward
             hit.forward <- tags$hits.forward

             forward.dp <- lapply(hit.forward, function(x){
               if(is.na(x)) return(NA)
               temp.score <- stringr::str_extract_all(string = x, pattern = "score\\{[0-9\\.]{0,30}\\}")
               unlist(lapply(temp.score, function(x){
                 as.numeric(gsub(pattern = "score\\{|\\}", replacement = "", x = x))
               }))
             })



             new.hit.forward <- lapply(hit.forward,
                                       function(x) {strsplit(x, split = ";")[[1]]})



             forward.index <- lapply(forward.dp, function(x){
               if(is.na(x)) return(NA)
               if(direction == "less"){
                 temp.idx <- which(x <= dp.cutoff)
               }else{
                 temp.idx <- which(x >= dp.cutoff)
               }
               if(length(temp.idx)==0) return(NA)
               temp.idx
             })

             new.hit.forward <- mapply(function(x, y){
               if(is.na(y)) return(NA)
               x[y]
             },
             x = new.hit.forward,
             y = forward.index)

             new.hit.forward <- unlist(lapply(new.hit.forward, function(x){
               paste(x, collapse = ";")
             }))

             new.hit.forward[new.hit.forward == "NA"] <- NA


             tags$hits.reverse <- new.hit.reverse
             tags$hits.forward <- new.hit.forward

             data <- data.frame(tags, sample, stringsAsFactors = FALSE)
             return(data)
           })





setGeneric(name = "getMS2", def = function(libID = "L0018",
                                           polarity = "positive",
                                           ce = "30",
                                           lib = zhulib.ms2){
  ms2.lib <- lib$compound

  if(polarity == "positive"){
    ms2.lib <- ms2.lib[["pos"]]
  }else{
    ms2.lib <- ms2.lib[["neg"]]
  }

  idx <- which(names(ms2.lib)==libID)
  if(length(idx) == 0) return(NULL)
  ms2 <- ms2.lib[[idx]]
  idx <- which(names(ms2) == ce)
  if(length(idx) == 0){return(NULL)}

  return.ms2 <- ms2[[idx]]
  return(return.ms2)
}
)





setGeneric(name = "getInfo", def = function(libID = "L0018",
                                            polarity = "positive",
                                            ce = "30",
                                            lib = zhulib.ms2){
  info <- lib$meta

  info1 <- info[["compound"]]
  if(polarity == "positive"){
    info2 <- info[["pos"]]
  }else{
    info2 <- info[["neg"]]
  }

  idx <- which(names(info2)==ce)
  if(length(idx) == 0) return(NULL)
  info2 <- info2[[idx]]
  idx <- which(info2$labid == libID)
  if(length(idx) == 0){return(NULL)}

  return.info2 <- info2[idx,, drop = FALSE]
  idx <- which(info1$labid == libID)
  return.info1 <- info1[idx,, drop = FALSE]
  return.info <- cbind(return.info1, return.info2)

  return(return.info)
}
)











ms2Plot <- function(object){
  info <- object[[1]]
  spectrum <- object[[2]]
  spectrum <- as.data.frame(spectrum)

  spectrum$intensity <- spectrum$intensity*100/max(spectrum$intensity)

  # my.theme <- ggplot2::theme_bw()+
  #   ggplot2::theme(axis.title.x = ggplot2::element_text(size = 10),
  #                  axis.title.y = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
  #                  axis.text.y = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(legend.title = ggplot2::element_text(size = 10)) +
  #   ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

  par(mar = c(5,5,4,2))
  plot <-  ggplot2::ggplot(spectrum, ggplot2::aes(x = mz, xend = mz, y = 0, yend = intensity)) +
    ggplot2::geom_segment(colour = "dodgerblue") +
    ggplot2::labs(x = "Mass to charge ratio (m/z)",
                  y = "Relative intensity")+
    ggplot2::ggtitle(paste("Peak:", info[1,1], "; m/z:",info[2,1], "; RT:",info[3,1]))+
    my.theme
  # ggplot2::theme(legend.justification=c(0,1), legend.position=c(0,1), legend.background = element_rect(fill=alpha('white', 0)))
  # ggplot2::coord_cartesian(xlim = xlim,
  # ylim = ylim, expand = TRUE)
  return(plot)

}



#####identify metabolites using m/z
mzIdentify <- function(peak.table,
                       polarity = c("positive", "negative"),
                       adduct = c("mi.plus.h", "mi.plus.k","mi.plus.na", "mi.plus.nh4"),
                       database = c("kegg", "hmdb"),
                       mz.tol = 25,
                       candidate.num = 100){
  mz <- as.numeric(peak.table$mz)
  polarity <- match.arg(polarity)
  database <- match.arg(database)

  if(polarity == "positive"){
    load("data/database/kegg.adduct.pos.rda")
    ms1.database <- kegg.adduct.pos
    rm(kegg.adduct.pos)
  }else{
    load("data/database/kegg.adduct.neg.rda")
    ms1.database <- kegg.adduct.neg
    rm(kegg.adduct.neg)
  }

  ms1.database <- ms1.database[which(ms1.database$Adduct %in% adduct),]

  match.result <- mzMatch(mz = mz,
                         ms1.database = ms1.database,
                         mz.tol = mz.tol)

  ###order match.result according to m/z error and remain the candidate
  match.result <- lapply(match.result, function(x){
    if(is.null(x)) return(x)
    x <- x[order(x$mz.error),]
    if(candidate.num > nrow(x)){
      x
    }else{
      x[1:candidate.num,,drop = FALSE]
    }
  })

  load("data/database/kegg.compound.rda")

  match.result <- pbapply::pblapply(match.result, function(x){
    if(is.null(x)) return(x)
    name <- unlist(lapply(strsplit(kegg.compound$Name[match(x$ID, kegg.compound$ID)], split = ";"),
                          function(x){
                            x[1]
                          }))

    x <- data.frame(x, name, stringsAsFactors = FALSE)
    x
  })

  match.result <- mapply(function(x,y){
    if(is.null(y)) return(unname(c(x[[1]], rep(NA,4))))
    name <- paste(y$name, collapse = ";")
    ID <- paste(y$ID, collapse = ";")
    Adduct <- paste(y$Adduct, collapse = ";")
    mz.error <- paste(round(y$mz.error, 3), collapse = ";")
    return(unname(c(x[[1]], name, ID, Adduct, mz.error)))
  },
  x = apply(peak.table, 1, list),
  y = match.result)

  match.result <- as.data.frame(t(match.result))

  colnames(match.result) <- c(colnames(peak.table), c("Compound.name", "ID", "Adduct", "mz.error"))
  match.result[,"name"] <- as.character(match.result[,"name"])
  match.result[,"Compound.name"] <- as.character(match.result[,"Compound.name"])
  match.result[,"ID"] <- as.character(match.result[,"ID"])
  match.result[,"Adduct"] <- as.character(match.result[,"Adduct"])
  match.result[,"mz.error"] <- as.character(match.result[,"mz.error"])

  rm(list = c("kegg.compound", "ms1.database"))
  match.result
}





