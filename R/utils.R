load("./data/ibex.data.rda")

"%!in%" <- Negate("%in%")
aa.eval <- function(x) { x %in% c("AF", "KF", "other")}

#Shhhhhh
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

AF.col <- c(2,3,4,5,6)
KF.col <- c(7,8,9,10,11,12,13,14,15,16)

#Generates the 30 vector based on autoencoder model 
#First normalizes the value by the min and max of the autoencoder training data
auto.embedder <- function(array.reshape, aa.model, local.max, local.min, AA.properties) {
  #OHE is already min/max normalized - each aa residue has 1 value and 19 0s
  if(AA.properties != "OHE") {
    for(i in seq_len(length(array.reshape))) {
      (array.reshape[i] - local.min[i])/(local.max[i] - local.min[i])
    }
  }
  array.reshape[is.na(array.reshape)] <- 0
  score <- stats::predict(aa.model, t(array.reshape))
  return(score)
}

one.hot.organizer <- function(refer) {
  aa.int.vals <- ibex.data[[1]]$aa
  names(aa.int.vals) <- 1:20
  intermediate.value <- rep(0,21)
  
  int <- matrix(nrow = 21, ncol = length(refer), 0)
  for(i in seq_along(refer)) {
    if (is.na(refer[i])) {
      int[1,i] <- 1
    } else {
      val <- as.integer(names(aa.int.vals[which(aa.int.vals == refer[i])]))
      new.value <- intermediate.value
      new.value[val+1] <- 1
      int[,i] <- new.value
    }
  }
  return(int)
}

#Selects columns to normalize input data based on the inputs to the model
aa.range.loader <- function(chain, AA.properties, ibex.data) {
  range <- ibex.data[["model.ranges"]][[chain]]
  min <- range[["min"]]
  max <- range[["max"]]
  ref <- seq(1, 900, 15)
  if (AA.properties == "AF") {
    ref2 <- sort(c(ref, ref+1, ref+2, ref+3, ref+4))
    min <- min[ref2]
    max <- max[ref2]
  } else if (AA.properties == "KF") {
    ref2 <- sort(c(ref+5, ref+6, ref+7, ref+8, ref+9, ref+10, ref+11, ref+12, ref+13, ref+14))
    min <- min[ref2]
    max <- max[ref2]
  }
  range <- list(min = min, max = max)
  return(range)
}

#Returns appropriate model for autoencoder
library(tensorflow)
library(keras)
aa.model.loader <- function(chain, AA.properties) {
  quiet(tensorflow::tf$compat$v1$disable_eager_execution())
  select  <- paste0("./encoders/", chain, "_", AA.properties, "_Encoder.h5")
  model <- quiet(load_model_hdf5(select, compile = FALSE))
  return(model)
}