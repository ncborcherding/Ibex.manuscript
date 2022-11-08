auto.encoder <- function(BCR, 
                         chain = "Heavy",
                         AA.properties = AA.properties) { 
  return <- list() ### Need to add reference data
  reference <- ibex.data[[1]] #AA properties
  col.ref <- grep(tolower(paste(AA.properties, collapse = "|")), colnames(reference))
  length <- NULL
  if (AA.properties == " both") {
    column.ref <- unique(sort(c(AF.col, KF.col)))
  } else {
    column.ref <- unique(sort(col.ref))
  }

  array.reshape <- NULL
  aa.model <- quiet(aa.model.loader(chain, AA.properties))
  range <- aa.range.loader(chain, AA.properties, ibex.data) 
  local.min <- range[[1]]
  local.max <- range[[2]]

  cells <- BCR
  score <- NULL
  for (n in seq_len(length(cells))) {
    tmp.CDR <- cells[n]
    refer <- unlist(strsplit(tmp.CDR, ""))
    refer <- c(refer, rep(NA, 70 - length(refer)))
    if(AA.properties == "OHE") {
      int <- one.hot.organizer(refer)
      array.reshape.tmp <- array_reshape(t(int), 1470)
    }else {
      int <- reference[match(refer, reference$aa),c(1,col.ref)]
      int <- as.matrix(int[,-1])
      array.reshape.tmp <- array_reshape(int, length(col.ref)*70)
    }
    score.tmp <- auto.embedder(array.reshape.tmp, aa.model, local.max, local.min, AA.properties)
    score <- rbind(score, score.tmp)
  }
  score <- data.frame(score)
  
  rownames(score) <- cells
  colnames(score) <- paste0("Ibex_", seq_len(ncol(score)))
  return(score)
}