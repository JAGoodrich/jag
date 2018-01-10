#' @export
#' @importFrom irr icc
#' @import dplyr
reliability.w <- function(var_1, var_2){ #Requires , irr
  d <-data.frame(var_1, var_2)
  d <- na.exclude(d)
  d$lnv1 <- log(d$var_1)
  d$lnv2 <- log(d$var_2)
  d$delta <- (d$var_2-d$var_1)
  d$pct_chg <- (d$var_2-d$var_1)/d$var_1
  d$lndelta <- d$lnv1-d$lnv2
  TE <- sd(d$delta)/sqrt(2)
  CV <- 100*(exp((sd(d$lndelta)/sqrt(2))/100)-1)
  CD <- mean(d$delta) / sd(d$delta)

  ICC <- icc(d[,c(1,2)], model = "twoway", type = "agreement")$value
  lm_resid <- resid(lm(var_1~var_2, data = d))
  par(mfrow=c(1,3))
  plot(d$var_1, d$var_2); abline(0, 1)  #Residuals vs. fitted of lm(var1, var2)
  #hist(lm_resid)
  boxplot(d[,1:2])
  stripchart(d[,1:2],vertical = TRUE, add = TRUE, method = "jitter")
  hist(d$pct_chg)
  par(mfrow=c(1,1))

  cat("\nDifference in means:")
  print(t.test(d$var_1, d$var_2, paired = TRUE))
  cat("Cohens d:", prettyNum(CD), "\n")
  cat("\nReliability:", "\n")
  cat("\n      Typical error: ", prettyNum(TE))
  cat("\n      Coefficient of Variability: ", prettyNum(CV))
  cat("\n      IntRAclass correlation coefficient: ", prettyNum(ICC))
  cat("\n      Pearson correlation coefficient (r, IntERclass correlation): ", prettyNum(cor(d$var_1, d$var_2)))
  cat("\n      Coefficient of Variability (r^2): ", prettyNum(cor(d$var_1, d$var_2)^2))
}
