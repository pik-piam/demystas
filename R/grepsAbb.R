#' Sequential abbreviation mapping
#'
#' Sequentially maps abbreviations onto complete words and returns a matrix of valid possibilities.
#'
#' @name grepsAbb
#' @aliases demystas
#' @param x a character vector containing abbreviations.
#' @param y a character vector containing whole words that could correspond to abbreviations.
#' @return a matrix with total number of rows equal to the length of vector `x`. The first column is a repeat of vector `x`. Further columns represent matched possible `y` vector components.
#' @author Atreya Shankar
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @export
#' @examples
#' \dontrun{
#'
#' x <- c("BLG", "BLD", "LAT", "EMM")
#' y <- c("Boulder","Latino", "Eminem", "Emmys", "Building")
#' test <- grepsAbb(x, y)
#' }

grepsAbb <- function(x, y){

  ### check dependencies ###

  if(!is.vector(x)){
    stop("x must be a vector")
  }

  if(!is.vector(y)){
    stop("y must be a vector")
  }

  ### main body ###

  start <- proc.time()

  result <- data.frame(matrix(ncol=length(y)+1), stringsAsFactors = FALSE)
  xList <- strsplit(x, "")
  yList <- as.list(y)

  for(i in 1:length(x)){
    result[i,1] <- x[i]
  }

  pb.overall <- txtProgressBar(min = 0, max = length(xList), initial = 0, char = "=",
                               width = options()$width, style = 3, file = "")

  for(i in 1:length(xList)){
    for(k in 1:length(yList)){
        if(!y[k] %in% result[i,-1]){
        checkIndex <- lapply(xList[[i]], function(x) return(grep(paste0("\\Q", x, "\\E"), strsplit(yList[[k]], "")[[1]], ignore.case = TRUE)))
        check <- lapply(checkIndex, length)

        if(!any(check == 0)) {
          if(min(checkIndex[[1]]) == 1 & all(sapply(1:(length(checkIndex)-1), function(i) any(sapply(checkIndex[[i]], function(x) any(checkIndex[[i+1]]>x)))))) {
            result[i,min(which(is.na(result[i,])))] <- y[k]
          }
        }
      }
    }
    Sys.sleep(1/1000)
    setTxtProgressBar(pb.overall, i, title = NULL, label = NULL)
  }
  close(pb.overall)

  # remove unneccessary NA columns

  if(length(which(colSums(is.na(result)) == nrow(result))) > 0){
    result <- result[,-which(colSums(is.na(result)) == nrow(result)), drop = FALSE]
  }

  end <- proc.time()
  print(end-start)

  return(result)
}
