## ----set-options, include = FALSE, cache=FALSE---------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
x <- read.csv(system.file("extdata", "sample1.csv", package = "demystas"), stringsAsFactors = FALSE)[337:347,1]

y <- read.csv(system.file("extdata", "sample2.csv", package = "demystas"), stringsAsFactors = FALSE)[c(29:35,553,737),1]

## ---- echo=FALSE, results='asis'-----------------------------------------
new <- data.frame(matrix(ncol = 2, nrow = 0))
new[1:length(x),1] <- x
new[1:length(y),2] <- y
names(new) <- c("x", "y")
knitr::kable(new)

## ---- echo=TRUE, eval = FALSE--------------------------------------------
#  test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|")

## ---- echo=FALSE, results='hide'-----------------------------------------
test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|")

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[1]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[1]])

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[2]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[2]])

## ---- echo=TRUE, eval = FALSE--------------------------------------------
#  test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|", limitWord = 0.17)

## ---- echo=FALSE, results='hide'-----------------------------------------
test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|", limitWord = 0.17)

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[1]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[1]])

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[2]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[2]])

## ---- echo=TRUE, eval = FALSE--------------------------------------------
#  test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|", limitWord = 0.17, limitChar = 0.5)

## ---- echo=FALSE, results='hide'-----------------------------------------
test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|", limitWord = 0.17, limitChar = 0.5)

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[1]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[1]])

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[2]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[2]])

## ---- echo=TRUE, eval = FALSE--------------------------------------------
#  test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|", limitWord = 0.15, limitChar = 0.5, wordIgnore = c("other"))

## ---- echo=FALSE, results='hide'-----------------------------------------
test <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|", limitWord = 0.15, limitChar = 0.5, wordIgnore = c("other"))

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[1]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[1]])

## ---- echo=TRUE, eval = FALSE, results='markup'--------------------------
#  View(test[[2]])

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test[[2]])

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  x <- read.csv(system.file("extdata", "sample1.csv", package = "demystas"), stringsAsFactors = FALSE)[1:100,1]
#  
#  y <- read.csv(system.file("extdata", "sample2.csv", package = "demystas"), stringsAsFactors = FALSE)[1:300,1]

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  test1 <- demystas::greps(x, y, sepx = "\\.", sepy = "\\|")
#  
#  test2 <- demystas::grepsParallel(x, y, noCores = 4, sepx = "\\.", sepy = "\\|")

## ---- echo=FALSE, results='asis'-----------------------------------------
new <- data.frame(matrix(ncol = 3, nrow = 0))
new[1,1] <- "84.441s"
new[1,2] <- "18.761s"
new[1,3] <- 84.441/18.761
names(new) <- c("greps", "grepsParallel", "speedRatio (par:seq)")
knitr::kable(new)

## ----echo=FALSE, out.width='70%', fig.align='center'---------------------
knitr::include_graphics('./seqVparTime.png')

## ----echo=FALSE, out.width='70%', fig.align='center'---------------------
knitr::include_graphics('./seqVparSpeed.png')

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
abb <- read.csv(system.file("extdata", "sample4.csv", package = "demystas"), stringsAsFactors = FALSE)[,1]

words <- read.csv(system.file("extdata", "sample3.csv", package = "demystas"), stringsAsFactors = FALSE)[,1]

## ---- echo=FALSE, results='asis'-----------------------------------------
new <- data.frame(matrix(ncol = 2, nrow = 0))
new[1:length(abb),1] <- abb
new[1:length(words),2] <- words
names(new) <- c("abb", "words")
knitr::kable(new)

## ---- echo=TRUE, eval = TRUE, results='hide'-----------------------------
test <- demystas::grepsAbb(abb, words)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(test)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
coords <- read.csv(system.file("extdata", "sample5.csv", package = "demystas"), stringsAsFactors = FALSE)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(coords, row.names = TRUE)

## ----echo=FALSE, out.width='100%', fig.align='center'--------------------
knitr::include_graphics('./globalSample.png')

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  library(rworldmap)
#  library(rworldxtra)
#  global <- getMap(resolution="high")
#  
#  results <- demystas::coords2spi(coords, global)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  results$ADMIN

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(c(rep("Canada", 2), "United States of America", rep("Canada", 2), rep("Mexico", 2), "Guatemala", "El Salvador", "Canada", rep(NA,4)), row.names = TRUE)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
coords <- read.csv(system.file("extdata", "sample5.csv", package = "demystas"), stringsAsFactors = FALSE)[11:14,]

## ---- echo=FALSE, results='asis'-----------------------------------------
row.names(coords) <- NULL
knitr::kable(coords, row.names = TRUE)

## ----echo=FALSE, out.width='90%', fig.align='center'---------------------
knitr::include_graphics('./offSample.png')

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  library(rworldmap)
#  library(rworldxtra)
#  global <- getMap(resolution="high")
#  
#  results <- demystas::spNearest(coords, global)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  results$ADMIN

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(c(rep("Canada", 2), "Brazil", "Senegal"), row.names = TRUE)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  ??geosphere::dist2Line

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  ??rgeos::gDistance

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
x <- c(rep("SRB", 5), rep("CZE", 5), rep("SRB", 2), rep("VAT", 3), rep("CZE", 4), rep("NER", 2))

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(x)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
x1 <- paste0(x, ".", 1:length(x))

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(x1)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
x2 <- x1[order(nchar(x1), x1)]

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(x2)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
x3 <- demystas::vectorEnum(x) 

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(x3)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
x4 <- x3[order(nchar(x3), x3)]

## ---- echo=FALSE, eval=TRUE----------------------------------------------
knitr::kable(x4)

