


grab.tuner <- function(directory = NULL, 
                       project_name = NULL) {
  
  
  json.files <- list.files(paste0(directory, "/", project_name), 
                           pattern = "trial.json", 
                           recursive = TRUE, 
                           full.names = TRUE)
  
  index.files <- list.files(paste0(directory, "/", project_name), 
                            pattern = "checkpoint.index", 
                            recursive = TRUE)
  index.files <- stringr::str_split(index.files, "/", simplify = TRUE)[,1]
  
  json.files <- json.files[grepl(paste0(index.files, collapse = "/|"), json.files)]
  
  lapply(seq_len(length(json.files)), function(x){
    dat <- rjson::fromJSON(file = json.files[x])
    score <- dat$score
    id <- dat$trial_id
    values <- unlist(dat$hyperparameters$values)
    names(values) <- names(dat$hyperparameters$values)
    output <- c(id = id, score = score, values)
    output
    
  }) -> model.metrics
  
  model.metrics <- dplyr::bind_rows(model.metrics)
}
