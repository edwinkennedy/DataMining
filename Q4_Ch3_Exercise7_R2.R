## =============================================================================
## Group Two  |  ISLR 2nd ed.  |  Chapter 3, Exercise 7
## Prove: in simple linear regression, R^2 = [Cor(X, Y)]^2
## (You may assume xbar = ybar = 0.)
## =============================================================================

cat("============================================================\n")
cat("Chapter 3, Exercise 7  |  R^2 = Cor(X, Y)^2\n")
cat("============================================================\n\n")

cat("From ISLR:\n")
cat("  RSS = sum (y_i - yhat_i)^2\n")
cat("  TSS = sum (y_i - ybar)^2\n")
cat("  R^2 = 1 - RSS / TSS                         (3.17)\n")
cat("  Cor(X,Y) = sum (x_i - xbar)(y_i - ybar)\n")
cat("             / sqrt[ sum(x_i-xbar)^2 * sum(y_i-ybar)^2 ]   (3.18)\n\n")

cat("Assume xbar = 0 and ybar = 0.\n")
cat("Then the least-squares intercept is 0 and\n")
cat("  beta1_hat = sum(x_i y_i) / sum(x_i^2)\n")
cat("  yhat_i    = beta1_hat * x_i\n")
cat("  TSS       = sum(y_i^2)\n\n")

cat("Expand RSS:\n")
cat("  RSS = sum (y_i - beta1_hat x_i)^2\n")
cat("      = sum y_i^2 - 2 beta1_hat sum x_i y_i + beta1_hat^2 sum x_i^2\n\n")

cat("Substitute beta1_hat = sum(x y) / sum(x^2):\n")
cat("  RSS = sum y_i^2 - [sum(x_i y_i)]^2 / sum(x_i^2)\n\n")

cat("Therefore\n")
cat("  R^2 = 1 - RSS/TSS\n")
cat("      = [sum(x_i y_i)]^2  /  [ sum(x_i^2) * sum(y_i^2) ]\n\n")

cat("Under the same centering,\n")
cat("  Cor(X,Y) = sum(x_i y_i) / sqrt[ sum(x_i^2) * sum(y_i^2) ]\n")
cat("so\n")
cat("  Cor(X,Y)^2 = [sum(x_i y_i)]^2 / [ sum(x_i^2) * sum(y_i^2) ] = R^2.\n\n")

cat("If the means are not already zero, replace x_i by (x_i - xbar) and\n")
cat("y_i by (y_i - ybar). That is exactly what simple linear regression does.\n\n")

## ---- Numerical verification in R -------------------------------------------
set.seed(7)
n <- 200
x <- rnorm(n)
y <- 1.5 * x + rnorm(n)

## Center so that xbar = ybar = 0, as allowed by the problem
x <- x - mean(x)
y <- y - mean(y)
stopifnot(abs(mean(x)) < 1e-12, abs(mean(y)) < 1e-12)

fit <- lm(y ~ x)                       # intercept will be ~0 after centering
r2   <- summary(fit)$r.squared
cor2 <- cor(x, y)^2

## Direct formula from the proof
r2_formula <- sum(x * y)^2 / (sum(x^2) * sum(y^2))

cat("Numerical check (n = 200, after centering):\n")
cat("  mean(x), mean(y)     :", mean(x), mean(y), "\n")
cat("  intercept of lm(y~x) :", unname(coef(fit)[1]), "\n")
cat("  slope of lm(y~x)     :", unname(coef(fit)[2]), "\n")
cat("  R^2 from summary()   :", r2, "\n")
cat("  Cor(X,Y)^2           :", cor2, "\n")
cat("  algebraic formula    :", r2_formula, "\n")
cat("  all three equal?     :", isTRUE(all.equal(r2, cor2)) &&
                              isTRUE(all.equal(r2, r2_formula)), "\n")

## Also true WITHOUT forcing the means to zero (ordinary SLR)
set.seed(7)
x2 <- rnorm(n)
y2 <- 1.5 * x2 + rnorm(n)
r2_raw  <- summary(lm(y2 ~ x2))$r.squared
cor2_raw <- cor(x2, y2)^2
cat("\nSame identity without centering (ordinary simple linear regression):\n")
cat("  R^2        :", r2_raw, "\n")
cat("  Cor(X,Y)^2 :", cor2_raw, "\n")
cat("  equal?     :", isTRUE(all.equal(r2_raw, cor2_raw)), "\n")

png("fig_r2_equals_cor2.png", width = 1400, height = 1100, res = 160)
plot(x, y, pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5),
     main = sprintf("Simple linear regression: R^2 = Cor(X,Y)^2 = %.3f", r2),
     xlab = "X (centered)", ylab = "Y (centered)")
abline(fit, col = "firebrick", lwd = 2)
legend("topleft",
       legend = c(sprintf("R^2 = %.4f", r2),
                  sprintf("Cor^2 = %.4f", cor2)),
       bty = "n")
dev.off()
cat("\nSaved fig_r2_equals_cor2.png\n")
