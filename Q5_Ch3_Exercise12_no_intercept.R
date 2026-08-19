## =============================================================================
## Group Two  |  ISLR 2nd ed.  |  Chapter 3, Exercise 12
## Simple linear regression without an intercept
## =============================================================================

cat("============================================================\n")
cat("Chapter 3, Exercise 12  |  Regression with no intercept\n")
cat("============================================================\n\n")

## ---------------------------------------------------------------------------
## (a) When are the two coefficient estimates the same?
## ---------------------------------------------------------------------------
cat("(a) When is the no-intercept slope of Y on X equal to that of X on Y?\n\n")
cat("Equation (3.38):  beta_hat(Y | X) = sum(x_i y_i) / sum(x_i^2)\n")
cat("Reverse roles  :  beta_hat(X | Y) = sum(x_i y_i) / sum(y_i^2)\n\n")
cat("The numerators are identical. The two estimates are equal iff\n")
cat("    sum(x_i^2) = sum(y_i^2)\n")
cat("that is, iff ||x|| = ||y||  (same Euclidean length),\n")
cat("or in the trivial case sum(x_i y_i) = 0 (both slopes are 0).\n\n")
cat("Special cases: y = x, y = -x, or y a permutation of x (or of -x).\n\n")

## In R, omit the intercept with + 0  or  - 1
##   lm(y ~ x + 0)
##   lm(y ~ x - 1)

n <- 100

## ---------------------------------------------------------------------------
## (b) Example where the two estimates DIFFER
## ---------------------------------------------------------------------------
cat("(b) Example with n = 100 where the two slopes differ\n")
set.seed(12)
x <- rnorm(n)
y <- 2 * x + rnorm(n, sd = 0.8)     # ||y|| is larger than ||x||

fit_yx <- lm(y ~ x + 0)             # Y onto X, no intercept
fit_xy <- lm(x ~ y + 0)             # X onto Y, no intercept

b_yx <- unname(coef(fit_yx))
b_xy <- unname(coef(fit_xy))

cat("    sum(x^2) =", round(sum(x^2), 3), "\n")
cat("    sum(y^2) =", round(sum(y^2), 3), "  (not equal)\n")
cat("    beta_hat(Y | X) =", round(b_yx, 4), "\n")
cat("    beta_hat(X | Y) =", round(b_xy, 4), "\n")
cat("    equal? ", isTRUE(all.equal(b_yx, b_xy)), "\n\n")
print(summary(fit_yx)$coefficients)
print(summary(fit_xy)$coefficients)

## ---------------------------------------------------------------------------
## (c) Example where the two estimates are THE SAME
## ---------------------------------------------------------------------------
cat("\n(c) Example with n = 100 where the two slopes are the same\n")
set.seed(12)
x <- rnorm(n)
y <- sample(x)                      # permutation => sum(x^2) == sum(y^2)

fit_yx2 <- lm(y ~ x + 0)
fit_xy2 <- lm(x ~ y + 0)
b_yx2 <- unname(coef(fit_yx2))
b_xy2 <- unname(coef(fit_xy2))

cat("    sum(x^2) =", round(sum(x^2), 3), "\n")
cat("    sum(y^2) =", round(sum(y^2), 3), "  (equal)\n")
cat("    beta_hat(Y | X) =", round(b_yx2, 6), "\n")
cat("    beta_hat(X | Y) =", round(b_xy2, 6), "\n")
cat("    equal? ", isTRUE(all.equal(b_yx2, b_xy2)), "\n\n")

## Two other constructions that also work:
set.seed(12)
x3 <- rnorm(n)
y3 <- x3                            # y = x  => both slopes = 1
y4 <- -x3                           # y = -x => both slopes = -1
cat("    Extra check, y =  x :",
    round(coef(lm(y3 ~ x3 + 0)), 4), "and",
    round(coef(lm(x3 ~ y3 + 0)), 4), "\n")
cat("    Extra check, y = -x :",
    round(coef(lm(y4 ~ x3 + 0)), 4), "and",
    round(coef(lm(x3 ~ y4 + 0)), 4), "\n")

## ---------------------------------------------------------------------------
## Plot both examples
## ---------------------------------------------------------------------------
png("fig_no_intercept.png", width = 1800, height = 900, res = 160)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

set.seed(12)
x <- rnorm(n); y <- 2 * x + rnorm(n, sd = 0.8)
plot(x, y, pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5),
     main = sprintf("(b) Different slopes\nY~X: %.3f   X~Y: %.3f",
                    b_yx, b_xy),
     xlab = "x", ylab = "y")
abline(0, b_yx, col = "firebrick", lwd = 2)
abline(0, 1 / b_xy, col = "darkgreen", lwd = 2, lty = 2)
legend("topleft", c("Y on X (no intercept)", "implied reverse"),
       col = c("firebrick", "darkgreen"), lwd = 2, lty = c(1, 2), bty = "n")

set.seed(12)
x <- rnorm(n); y <- sample(x)
plot(x, y, pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5),
     main = sprintf("(c) Equal slopes\nY~X = X~Y = %.4f", b_yx2),
     xlab = "x", ylab = "y (permutation of x)")
abline(0, b_yx2, col = "firebrick", lwd = 2)
par(op)
dev.off()
cat("\nSaved fig_no_intercept.png\n")

cat("\nSummary\n")
print(data.frame(
  example     = c("(b) different", "(c) equal"),
  construction = c("y = 2x + noise", "y = permutation of x"),
  sum_x2_eq_sum_y2 = c(FALSE, TRUE),
  beta_Y_on_X = round(c(b_yx, b_yx2), 4),
  beta_X_on_Y = round(c(b_xy, b_xy2), 4),
  stringsAsFactors = FALSE
), row.names = FALSE)
