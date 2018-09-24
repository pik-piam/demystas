#' Parallel pairwise grep-style matching
#'
#' Performs a two-way grep-style analysis on two character vectors using parallel computation. Calculates pairwise matching scores based on a rigid customized routine and returns matching strings ranked
#' from best to worst. The user is able to influence the algorithm by tweaking matching parameters.
#'
#' @name grepsParallel
#' @param x a character vector containing elements to be considered in pairwise grep-analysis. Words are separated by `sepx`.
#' @param y a character vector containing elements to be considered in pairwise grep-analysis. Words are separated by `sepy`.
#' @param noCores is a numerical value specifying the number of cores to be used for parallel computation.
#' @param sepx a regex-style expression which indicates how words are separated in `x`. If `x` is already a final vector and does not need to be segmented, input `sepx = NULL`. Defaults to "\\\\."
#' @param sepy a regex-style expression which indicates how words are separated in `y`. If `y` is already a final vector and does not need to be segmented, input `sepy = NULL`. Defaults to "\\\\."
#' @param limitChar a numerical value from 0 to 1 which provides a lower proportional bound for a word-to-word match to be considered significant. If the user prioritizes
#' loosely matched words, one can leave this value low such as 0.1. Alternatively, if the end-user prioritizes strongly matched individual words, `limitChar` can be increased to a value of
#' say, 0.7. Defaults to 0.
#' @param limitWord a numerical value greater than or equal to 0 which provides a proportional filter for significant overall characters matched. Defaults to 0.
#' @param booster a numerical value between 0 to 1 which provides a boost to the matching score of exceptionally well-matched words. For meaningful results, its value should be greater than `limitWord`.
#' Defaults to 0.9.
#' @param wordIgnore a character vector which should be ignored while searching for matches. Examples could be redundant characters such as "the" or "of". Defaults to NULL.
#' @param checkBoth a logical which indicates whether both left and right grep analyses should be conducted (TRUE), or if only a left grep analysis is necessary (FALSE). Defaults to TRUE.
#' @param ignore.case a logical which indicates if cases should be ignored when matching. Defaults to TRUE.
#' @return a list containing two matrices. The first "result" matrix has a total number of rows equal to the length of vector x. The first column contains a repeat of vector `x` and the
#' corresponding columns contain ranked `y` vector matches to the corresponding rows. The matches are ranked from best to worst as column number increases.
#' The second "rank" matrix contains a matrix with equivalent dimension as the first matrix. Instead of containing the matches from `y`, this matrix contains the matching
#' scores of the respective components from the first matrix. A ranking score of 99 implies a perfect match. Perfect matches are isolated for each row.
#' @import doParallel
#' @importFrom parallel detectCores
#' @importFrom snow makeSOCKcluster
#' @importFrom snow stopCluster
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom foreach %:%
#' @importFrom doSNOW registerDoSNOW
#' @export
#' @author Atreya Shankar
#' @examples
#' \dontrun{
#'
#' x <- c("foo.test.xyz", "baz.foosh", "bat")
#' y <- c("ba","foosba.asd", "bats.at", "foos", "gams.asd")
#' test <- grepsParallel(x, y, 2)
#' }

grepsParallel <- function(x, y, noCores, sepx = "\\.", sepy = "\\.", limitChar = 0, limitWord = 0, booster = 0.9, wordIgnore = NULL, checkBoth = TRUE, ignore.case = TRUE){

  ### check dependencies ###

  if(!is.vector(x)){
    stop("x must be a vector")
  }

  if(!is.vector(y)){
    stop("y must be a vector")
  }

  if(!is.numeric(noCores)){
    stop("must specify noCores for parallel processing")
  } else if(noCores > detectCores()){
    stop("more noCores supplied than available cores")
  }

  if(!is.character(sepx) & !is.null(sepx)){
    stop("sepx must be NULL or a regex-style expression")
  } else if(is.null(sepx)){
    xList <- as.list(x)
  } else xList <- strsplit(x, sepx)

  if(!is.character(sepy) & !is.null(sepy)){
    stop("sepy must be NULL or a regex-style expression")
  } else if(is.null(sepy)){
    yList <- as.list(y)
  } else yList <- strsplit(y, sepy)

  if(!is.numeric(limitChar)){
    stop("limitChar must be numerical")
  }

  if(!is.numeric(limitWord)){
    stop("limitWord must be numerical")
  }

  if(!is.numeric(booster)){
    stop("booster must be numerical")
  }

  if(!is.null(wordIgnore) & !is.character(wordIgnore)){
    stop("wordIgnore must be NULL or a character vector")
  }

  if(!is.logical(checkBoth)){
    stop("checkBoth must be logical")
  }

  if(!is.logical(ignore.case)){
    stop("ignore.case must be logical")
  }

  ### main body ###

  start <- proc.time()

  cl <- makeSOCKcluster(noCores)
  registerDoSNOW(cl)
  i = 0
  k = 0

  tmp <- foreach(i = 1:length(xList), .combine = "rbind") %:% foreach(k = 1:length(yList), .combine = "cbind") %dopar% {
    if(x[i] == y[k] | (all(xList[[i]] %in% yList[[k]]) & all(yList[[k]] %in% xList[[i]]))) {
      return(c(y[k],99))
    } else {
      checkIndex1 <- lapply(xList[[i]], function(x) return(grep(paste0("\\Q", x, "\\E"), yList[[k]], ignore.case = ignore.case)))
      check1 <- lapply(checkIndex1, length)

      if(checkBoth == TRUE){
        checkIndex2 <- lapply(yList[[k]], function(x) return(grep(paste0("\\Q", x, "\\E"), xList[[i]], ignore.case = ignore.case)))
        check2 <- lapply(checkIndex2, length)
      }

      if(length(which(check1 > 0)) != 0){
        charcheck1 <- sapply(rep(xList[[i]][which(check1 > 0)],  check1[which(check1 > 0)]), nchar)/sapply(yList[[k]][do.call("c", checkIndex1)], nchar)
        if(ignore.case == TRUE){
          charcheck1 <- charcheck1[which(!toupper(names(charcheck1)) %in% toupper(wordIgnore))]
        } else charcheck1 <- charcheck1[which(!names(charcheck1) %in% wordIgnore)]
        boost1 <- length(which(charcheck1 >= booster))
        if(length(which(charcheck1 > limitChar)) > 0){
          if(ignore.case == TRUE){
            wordcheck1 <- sum(sapply(names(charcheck1)[which(charcheck1 > limitChar)], nchar))/sum(sapply(xList[[i]][which(!toupper(xList[[i]]) %in% toupper(wordIgnore))], nchar))
          } else wordcheck1 <- sum(sapply(names(charcheck1)[which(charcheck1 > limitChar)], nchar))/sum(sapply(xList[[i]][which(!xList[[i]] %in% wordIgnore)], nchar))
        } else wordcheck1 <- 0
      } else {
        boost1 <- 0
        wordcheck1 <- 0
      }

      if(checkBoth == TRUE){
        if(length(which(check2 > 0)) != 0){
          charcheck2 <- sapply(rep(yList[[k]][which(check2 > 0)], check2[which(check2 > 0)]), nchar)/sapply(xList[[i]][do.call("c", checkIndex2)], nchar)
          if(ignore.case == TRUE){
            charcheck2 <- charcheck2[which(!toupper(names(charcheck2)) %in% toupper(wordIgnore))]
          } else charcheck2 <- charcheck2[which(!names(charcheck2) %in% wordIgnore)]
          boost2 <- length(which(charcheck2 >= booster))
          if(length(which(charcheck2 > limitChar)) > 0){
            if(ignore.case == TRUE){
              wordcheck2 <- sum(sapply(names(charcheck2)[which(charcheck2 > limitChar)], nchar))/sum(sapply(yList[[k]][which(!toupper(yList[[k]]) %in% toupper(wordIgnore))], nchar))
            } else wordcheck2 <- sum(sapply(names(charcheck2)[which(charcheck2 > limitChar)], nchar))/sum(sapply(yList[[k]][which(!yList[[k]] %in% wordIgnore)], nchar))
          } else wordcheck2 <- 0
        } else {
          boost2 <- 0
          wordcheck2 <- 0
        }
        measure <- ((wordcheck1+wordcheck2)/2)*(2^(boost1))*(2^(boost2))
      } else measure <- wordcheck1*(2^(boost1))

      if(measure > limitWord){
        return(c(y[k],measure))
      } else return(c(NA,NA))
    }
  }

  stopCluster(cl)

  result <- cbind(x, tmp[seq(1,nrow(tmp),2),, drop= FALSE])
  rank <- cbind(x, tmp[seq(2,nrow(tmp),2),, drop = FALSE])
  row.names(result) <- NULL
  row.names(rank) <- NULL

  for(i in 1:nrow(result)){
    a <- unique(result[i,-1])[!is.na(unique(result[i,-1]))]
    for(j in 1:length(a)){
      if(length(which(result[i,-1] == a[j])) > 1){
        rank[i,which(result[i,-1] == a[j])[2:length(which(result[i,-1] == a[j]))] + 1] <- NA
        result[i,which(result[i,-1] == a[j])[2:length(which(result[i,-1] == a[j]))] + 1] <- NA
      }
    }
  }

  # given exact matches remove all others

  v <- as.matrix(which(rank == 99, arr.ind = TRUE))

  if(nrow(v) > 0){
    q <- as.matrix(which(rank != 99, arr.ind = TRUE))
    q <- q[which(q[,1] %in% v[,1]),,drop=FALSE]
    q <- q[-which(q[,2] == 1),,drop=FALSE]
    if(nrow(q) != 0){
      result[q] <- NA
      rank[q] <- NA
    }
  }

  result <- as.data.frame(t(apply(result, 1, function(x) return(c(x[!is.na(x)],x[is.na(x)])))), stringsAsFactors = FALSE)
  rank <- as.data.frame(t(apply(rank, 1, function(x) return(c(x[!is.na(x)],x[is.na(x)])))), stringsAsFactors = FALSE)

  if(ncol(rank) > 1){
    if(!is.numeric(rank[,-1]) | !is.vector(rank[,-1])){
      result[,-1] <- data.frame(t(sapply(1:nrow(rank), function(i) return(result[i,-1][as.numeric(order(-as.numeric(rank[i,-1])))]))))
      rank[,-1] <- data.frame(t(sapply(1:nrow(rank), function(i) return(rank[i,-1][as.numeric(order(-as.numeric(rank[i,-1])))]))))
    } else {
      result[,-1] <- sapply(1:nrow(rank), function(i) return(result[i,-1][as.numeric(order(-as.numeric(rank[i,-1])))]))
      rank[,-1] <- sapply(1:nrow(rank), function(i) return(rank[i,-1][as.numeric(order(-as.numeric(rank[i,-1])))]))
    }
  }

  # remove unneccessary NA columns

  if(length(which(colSums(is.na(result)) == nrow(result))) > 0){
    result <- result[,-which(colSums(is.na(result)) == nrow(result)), drop = FALSE]
    rank <- rank[,-which(colSums(is.na(rank)) == nrow(rank)), drop = FALSE]
  }

  end <- proc.time()
  print(end-start)
  return(list(result,rank))
}
