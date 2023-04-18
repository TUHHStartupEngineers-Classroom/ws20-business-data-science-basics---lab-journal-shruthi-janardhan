roll3 <- function(faces = 1:6 , number_of_dice = 1) {
  probabilities_vector <- c(1/10, 1/10, 1/10, 1/10, 1/10, 1/2)
  dice <- sample(x = faces, size = number_of_dice, replace = TRUE, prob = probabilities_vector )
  sum(dice)
}

results <- replicate(n = 100, expr = roll3(), simplify=TRUE)
hist(results)


