
#' @param a Length of one side of the right triangle.
#' @param b Length of another side of the right triangle.
#' @param c Length of the hypotenuse side of the right triangle.
#'
#' @return Length of the third side of the right triangle (\eqn{c}).
#'
#' @examples
#' pythagorean_third_side(3, 4)  # Should return 5
#'
#@export
pythagorean_third_side <- function(a= 0 , b = 0, c = 0) {
  # Check if all inputs are numeric
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
    stop("All inputs must be numeric values.")
  }

  # Check if exactly two sides are provided
  if ((a == 0) + (b == 0) + (c == 0) != 1) {
    stop("Provide 2 non zero values")
  }

  # Calculate the length of the third side using the Pythagorean theorem given the input using the pythagorean
  # theorem to find the missing vlaue (the one equal to zero)
  if (a == 0) {
    # a is the missing side
    result <- sqrt(c^2 - b^2)
  } else if (b == 0) {
    # b is the missing side
    result <- sqrt(c^2 - a^2)
  } else {
    # c is the missing side
    result <- sqrt(a^2 + b^2)
  }
  return(result)
}





#@export
custom_trimmed_mean <- function(x, s, l) {
  # Check if x has at least s + l + 1 values
  if (length(x) < s + l + 1) {
    stop("Not enough values in 'x' to calculate trimmed mean.")
  }

  # Sort the vector in ascending order
  x_sorted <- sort(x)

  # Remove the smallest 's' values and the largest 'l' values
  trimmed_x <- x_sorted[(s + 1):(length(x_sorted) - l)]
  print(trimmed_x)
  # Calculate the mean of the trimmed vector
  mean_trimmed <- mean(trimmed_x)

  return(mean_trimmed)
}

#x <- c(1, 7, 3, 2, 5, 0.5, 9, 10)
#s <- 1
#result <- custom_trimmed_mean(x, s, l)
