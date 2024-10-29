findElbowPoint <- function(x, y, smooth = TRUE, span = 1/3) {
  # Ensure that x and y are numeric vectors
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # Optionally smooth the y-values to reduce noise
  if (smooth) {
    y <- stats::lowess(x, y, f = span, iter = 100)$y
  }
  
  # Sort the x and y values
  sorted_indices <- order(x)
  x <- x[sorted_indices]
  y <- y[sorted_indices]
  
  # Create a line between the first and last point
  line_vector <- c(x[length(x)] - x[1], y[length(y)] - y[1])
  
  # Normalize the line vector
  line_length <- sqrt(sum(line_vector^2))
  line_unit_vector <- line_vector / line_length
  
  # Calculate the distance from each point to the line
  distances <- sapply(1:length(x), function(i) {
    point_vector <- c(x[i] - x[1], y[i] - y[1])
    projection_length <- sum(point_vector * line_unit_vector)
    projection_vector <- projection_length * line_unit_vector
    perpendicular_vector <- point_vector - projection_vector
    sqrt(sum(perpendicular_vector^2))
  })
  
  # Find the index of the point with the maximum distance from the line
  elbow_index <- which.max(distances)
  
  return(list(
    elbow_index = elbow_index,
    elbow_x = x[elbow_index],
    elbow_y = y[elbow_index]
  ))
}


findElbowByCurvature <- function(x, y) {
  # Ensure numeric input
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # Smooth data to reduce noise
  y_smooth <- stats::lowess(x, y, f = 0.3, iter = 20)$y
  
  # Calculate first and second derivatives
  dy <- diff(y_smooth) / diff(x)
  d2y <- diff(dy) / diff(x[-1])
  
  # Find index of minimum second derivative
  elbow_index <- which.min(d2y)
  
  return(list(
    elbow_index = elbow_index,
    elbow_x = x[elbow_index + 1],
    elbow_y = y_smooth[elbow_index + 1]
  ))
}

