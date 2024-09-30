findElbowPoint <- function(x, y) {
  # Ensure that x and y are numeric vectors
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  # Create a line between the first and last point
  line_vector <- c(x[length(x)] - x[1], y[length(y)] - y[1])
  
  # Normalize the line vector
  line_length <- sqrt(sum(line_vector^2))
  line_unit_vector <- line_vector / line_length
  
  # Calculate the distance from each point to the line
  distances <- vector()
  
  for (i in 1:length(x)) {
    # Vector from the first point to the current point
    point_vector <- c(x[i] - x[1], y[i] - y[1])
    
    # Project the point vector onto the line unit vector
    projection_length <- sum(point_vector * line_unit_vector)
    projection_vector <- projection_length * line_unit_vector
    
    # Calculate the perpendicular distance
    perpendicular_vector <- point_vector - projection_vector
    perpendicular_distance <- sqrt(sum(perpendicular_vector^2))
    
    # Store the distance
    distances[i] <- perpendicular_distance
  }
  
  # Find the index of the point with the maximum distance from the line
  elbow_index <- which.max(distances)
  
  return(list(elbow_index = elbow_index, elbow_x = x[elbow_index], elbow_y = y[elbow_index]))
}
