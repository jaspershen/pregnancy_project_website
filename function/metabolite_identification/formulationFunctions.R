#formula functions
#############------------------------------------------------------------------
#' @title sumFormula
#' @description Combine metabolite and adduct as a new sum formula.
#' If there are no enough element to remove, return NA.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param formula The formula of metabolite.
#' @param adduct The adduct of metabolite.
#' @return  A sum formula.
#' @export

setGeneric(name = "sumFormula",
           def = function(formula = "C9H11NO2",
                          adduct = "M-H2O+H"){

             if(is.na(formula)) return(NA)
             if(is.na(adduct)) return(formula)
             if(adduct == "M+" | adduct == "M-"){
               return(formula)
             }

             formula1 <- splitFormula(formula)
             adduct1 <- strsplit(x = adduct, split = "\\-|\\+")[[1]][-1]
             polymer <- as.numeric(gsub(pattern = "M", replacement = "",
                                        strsplit(x = adduct, split = "\\-|\\+")[[1]][1]))
             if (is.na(polymer)) polymer <- 1

             plusorminus <- strsplit(x = adduct, split = "")[[1]]
             plusorminus <- grep("\\+|\\-", plusorminus, value = TRUE)

             formula1$number <- formula1$number * polymer

             adduct1 <- mapply(function(x, y){
               temp <- splitFormula(x)
               temp$number <- temp$number * ifelse(y == "+", 1, -1)
               list(temp)
             },
             x = adduct1,
             y = plusorminus)

             adduct1 <- do.call(rbind, adduct1)

             formula <- rbind(formula1, adduct1)
             rownames(formula) <- NULL

             unique.element <- unique(formula$element.name)
             if(length(unique.element) == nrow(formula)){
               if(any(formula$number < 0)) {
                 return(NA)
               }else{
                 formula$number[formula$number==1] <- "W"
                 formula <- paste(paste(formula$element.name, formula$number, sep = ""), collapse = "")
                 formula <- strsplit(formula, split = "")[[1]]
                 formula[formula == "W"] <- ""
                 formula <- paste(formula, collapse = "")
                 return(formula)
               }
             }else{
               formula <- lapply(unique.element, function(x){
                 formula[formula$element.name == x,,drop = FALSE]
               })

               formula <- lapply(formula, function(x){
                 data.frame(unique(x$element.name), sum(x$number),
                            stringsAsFactors = FALSE)
               })

               formula <- do.call(rbind, formula)
               formula <- formula[formula[,2] != 0,]
               colnames(formula) <- c("element.name", "number")
               if(any(formula$number < 0)) {return(NA)}else{
                 formula$number[formula$number==1] <- "W"
                 formula <- paste(paste(formula$element.name, formula$number, sep = ""), collapse = "")
                 formula <- strsplit(formula, split = "")[[1]]
                 formula[formula == "W"] <- ""
                 formula <- paste(formula, collapse = "")
                 return(formula)
               }
             }
           })

# setGeneric(name = "sumFormula",
#            def = function(formula = "C9H11NO2",
#                           adduct = "M+H"){
#              if(is.na(formula)) return(NA)
#              if(is.na(adduct)) return(formula)
#              if(adduct == "M+" | adduct == "M-"){
#                return(formula)
#              }
#
#              suppressMessages(expr = data(thermo, package = "CHNOSZ"))
#              adduct.species <- c("2ACN", "2Cl", "2H", "2H2O", "2K", "2Na",
#                                  "3ACN", "3H", "3H2O", "3Na", "ACN", "Br",
#                                  "CF3COOH", "CH3OH", "Cl", "H", "H2O", "Hac",
#                                  "HCOOH", "IsoProp", "K", "Na", "NaCOOH", "NH4",
#                                  "DMSO", "CH3CN", "3K", "CH3COO", "F", "NH3",
#                                  "HCOO")
#              adduct.number <- c(2, 2, 2, 2, 2, 2,
#                                 3, 3, 3, 3, 1, 1,
#                                 1, 1, 1, 1, 1, 1,
#                                 1, 1, 1, 1, 1, 1,
#                                 1, 1, 3, 1, 1, 1,
#                                 1)
#
#              adduct.element <- c("C4H6N", "Cl2" ,"H2", "H4O2", "K2", "Na2",
#                                  "C6H9N3", "H3", "H6O3", "Na3", "C2H3N", "Br",
#                                  "C2F3O2H", "CH4O", "Cl", "H", "H2O", "C2H4O2",
#                                  "CO2H2", "C3H8O", "K", "Na", "NaCO2H", "NH4",
#                                  "C2H6OS", "C2H3N", "K3", "C2H3O2", "F", "NH3",
#                                  "CHO2")
#              ###formula has the charge
#              if(adduct == "M+"){return(formula)}
#
#              adduct1 <- strsplit(x = adduct, split = ("\\+|\\-"))[[1]]
#
#              plusorminus <- strsplit(x = adduct, split = "")[[1]]
#              idx1 <- which(plusorminus == "+")
#              idx2 <- which(plusorminus == "-")
#              plusorminus1 <- NULL
#              plusorminus1[idx1] <- "+"
#              plusorminus1[idx2] <- "-"
#              plusorminus1 <- plusorminus1[!is.na(plusorminus1)]
#
#              ## ploymer
#              polymer <- as.numeric(gsub(pattern = "M", replacement = "", x =  adduct1[1]))
#              if (is.na(polymer)) polymer <- 1
#
#              ## adduct information
#              adduct2 <- adduct1[-1]
#
#              ## add polymer
#              formula1 <- CHNOSZ::makeup(unname(formula))
#              formula1 <- CHNOSZ::as.chemical.formula(formula1 * polymer)
#
#              for (i in 1:length(adduct2)) {
#                element <- adduct2[i]
#                if(!is.element(el = element, set = adduct.species)){
#                  stop(element, " is not in the adduct.species")
#                }
#                element <- adduct.element[which(element == adduct.species)]
#
#                ## add some thing
#                if (plusorminus1[i] == "+") {
#                  temp.formula1 <- splitFormula(formula1)
#                  temp.element <- splitFormula(element)
#                  if(nrow(temp.formula1) == 1 & nrow(temp.element) == 1 &
#                     temp.formula1[1,1] == temp.element[1,1]){
#                    formula1 <- CHNOSZ::makeup(formula1) + CHNOSZ::makeup(element)
#                  }else{
#                    temp <- CHNOSZ::i2A(c(formula1, element))
#                    formula1 <- apply(temp, 2, sum)
#                  }
#                  # formula1 <- temp[1,] + temp[2,]
#                  formula1 <- CHNOSZ::as.chemical.formula(formula1)
#                }else{
#                  temp.formula1 <- splitFormula(formula1)
#                  temp.element <- splitFormula(element)
#
#                  if(nrow(temp.formula1) == 1 & nrow(temp.element) == 1 &
#                     temp.formula1[1,1] == temp.element[1,1]){
#                    formula1 <- CHNOSZ::makeup(formula1) - CHNOSZ::makeup(element)
#                  }else{
#                    temp <- CHNOSZ::i2A(c(formula1, element))
#                    formula1 <- temp[1,] - temp[2,]
#                  }
#                  if(any(formula1 < 0)) return(NA)
#                  formula1 <- CHNOSZ::as.chemical.formula(formula1)
#                }
#              }
#              return(formula1)
#
#            })





#-----------------------------------------------------------------------------
#' @title splitFormula
#' @description Split a formula into element and number.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param formula The formula of metabolite.
#' @return  A splited formula.
#' @export

setGeneric(name = "splitFormula",
           def = function(formula = "C9H11NO2"){
             temp.formula <- strsplit(formula, split = "")[[1]]

             number <- NULL
             for(i in 1:length(temp.formula)){
               if(length(grep("[0-9]{1}", temp.formula[i])) == 0){break}
               number[i] <- temp.formula[i]
             }

             if(!is.null(number)) {
               number <- as.numeric(paste(number, collapse = ""))
             }else{
               number <- 1
             }
             ##first select the Na, Cl and so on element
             idx1 <- gregexpr("[A-Z][a-z][0-9]*", formula)[[1]]
             len1 <- attributes(idx1)$match.length
             ##no double element
             if(idx1[1] == -1) {
               double.formula <- matrix(NA, ncol = 2)
               formula1 <- formula
             }else{
               double.letter.element <- NULL
               double.number <- NULL
               remove.idx <- NULL
               for (i in 1:length(idx1)) {
                 double.letter.element[i] <- substr(formula, idx1[i], idx1[i] + len1[i] - 1)
                 if(nchar(double.letter.element[i]) == 2){
                   double.number[i] <- 1
                 }else{
                   double.number[i] <- as.numeric(substr(double.letter.element[i], 3, nchar(double.letter.element[i])))
                 }
                 double.letter.element[i] <- substr(double.letter.element[i], 1, 2)
                 remove.idx <- c(remove.idx, idx1[i] : (idx1[i] + len1[i] - 1))
               }

               double.formula <- data.frame(double.letter.element, double.number, stringsAsFactors = FALSE)
               formula1 <- strsplit(formula, split = "")[[1]]
               formula1 <- formula1[-remove.idx]
               formula1 <- paste(formula1, collapse = "")
             }

             ## no one element
             if(formula1 == ""){
               one.formula <- matrix(NA, ncol = 2)
             }else{
               idx2 <- gregexpr("[A-Z][0-9]*", formula1)[[1]]
               len2 <- attributes(idx2)$match.length
               one.letter.element <- NULL
               one.number <- NULL
               for (i in 1:length(idx2)) {
                 one.letter.element[i] <- substr(formula1, idx2[i], idx2[i] + len2[i] - 1)
                 if(nchar(one.letter.element[i]) == 1){
                   one.number[i] <- 1
                 }else{
                   one.number[i] <- as.numeric(substr(one.letter.element[i], 2, nchar(one.letter.element[i])))
                 }
                 one.letter.element[i] <- substr(one.letter.element[i], 1, 1)
               }
               one.formula <- data.frame(one.letter.element, one.number, stringsAsFactors = FALSE)
             }

             colnames(double.formula) <- colnames(one.formula) <- c("element.name","number")
             formula <- rbind(double.formula, one.formula)
             formula <- formula[!apply(formula, 1, function(x) any(is.na(x))),]

             formula <- formula[order(formula$element.name),]
             formula$number <- formula$number * number
             unique.element <- unique(formula$element.name)
             if(length(unique.element) == nrow(formula)){
               return(formula)
             }else{
               formula <- lapply(unique.element, function(x){
                 formula[formula$element.name == x,,drop = FALSE]
               })

               formula <- lapply(formula, function(x){
                 data.frame(unique(x$element.name), sum(x$number), stringsAsFactors = FALSE)
               })

               formula <- do.call(rbind, formula)
               colnames(formula) <- c("element.name", "number")
               return(formula)
             }
           })

#-----------------------------------------------------------------------------
#' @title pasteElement
#' @description Paste formula and element.
#' Combine metabolite and adduct as a new sum formula.
#' If there are no enough element to remove, return NA.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param formula The formula of metabolite.
#' @param element The element.
#' @param mode Add or remove a module
#' @export
#' @return  A formula.


pasteElement <- function(formula = "C9H11NO2",
                         element = "H",
                         mode = c("plus", "minus")){

  mode <- match.arg(mode)
  formula <- splitFormula(formula = formula)
  element <- splitFormula(formula = element)


  ## mode = plus
  if(mode == "plus"){
    for (i in 1:nrow(element)){
      temp.name <- as.character(element[i,1])
      temp.number <- as.numeric(element[i,2])
      temp.idx <- match(temp.name, formula[,1])
      if(is.na(temp.idx)) {
        formula <- rbind(formula, element[i,])
      }else{
        formula[temp.idx, 2] <- formula[temp.idx, 2] + temp.number
      }
    }
  }else{
    for (i in 1:nrow(element)){
      temp.name <- as.character(element[i,1])
      temp.number <- as.numeric(element[i,2])
      temp.idx <- match(temp.name, formula[,1])
      if(is.na(temp.idx)) {
        # warning("Formula has no element in adduct!\n")
        return(NA)
      }else{
        formula[temp.idx,2] <- formula[temp.idx,2] - temp.number
        if(formula[temp.idx,2] < 0) {
          # warning("Formula has no enough element in adduct!\n")
          return(NA)
        }
      }
    }
  }

  ###return formula
  formula <- as.data.frame(formula)
  formula <- formula[formula[,2] != 0, , drop = FALSE]
  formula <- c(t(formula))
  formula <- gsub(pattern = " ", replacement = "", x = formula)
  formula <- formula[formula != "1"]
  formula <- paste(formula, collapse = "")
  return(formula)
}




#-----------------------------------------------------------------------------
#' @title checkElement
#' @description Check a formula can add one adduct or not.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param formula The formula of metabolite.
#' @param adduct The adduct.
#' @return  valid, return TRUE; invalid.
#' @export

setGeneric(name = "checkElement",
           def = function(formula = "C9H11NO2",
                          adduct = "M-H2O+H"){
             formula1 <- splitFormula(formula)
             adduct1 <- strsplit(x = adduct, split = "\\-|\\+")[[1]][-1]
             plusorminus <- strsplit(x = adduct, split = "")[[1]]
             plusorminus <- grep("\\+|\\-", plusorminus, value = TRUE)
             if(all(plusorminus == "+")) return(TRUE)

             adduct1 <- mapply(function(x, y){
               temp <- splitFormula(x)
               temp$number <- temp$number * ifelse(y == "+", 1, -1)
               list(temp)
             },
             x = adduct1,
             y = plusorminus)

             adduct1 <- do.call(rbind, adduct1)

             formula <- rbind(formula1, adduct1)
             rownames(formula) <- NULL

             unique.element <- unique(formula$element.name)
             if(length(unique.element) == nrow(formula)){
               if(any(formula$number < 0)) {return(FALSE)}else{return(TRUE)}
             }else{
               formula <- lapply(unique.element, function(x){
                 formula[formula$element.name == x,,drop = FALSE]
               })

               formula <- lapply(formula, function(x){
                 data.frame(unique(x$element.name), sum(x$number), stringsAsFactors = FALSE)
               })

               formula <- do.call(rbind, formula)
               colnames(formula) <- c("element.name", "number")
               if(any(formula$number < 0)) {return(FALSE)}else{return(TRUE)}
             }
           })