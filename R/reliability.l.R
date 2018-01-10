#' @export
#' @importFrom irr icc
#' @import dplyr
#'
reliability.l <- function(fx, id, data, iv_name = NULL){
  data <- droplevels(data)
  fx.terms <- terms(fx)
  response <- all.vars(fx)[attributes(fx.terms)$response]
  iv.names <- attributes(terms(fx))$term.labels[which(attributes(fx.terms)$order == 1)]
  if(length(iv.names) > 1) {return("More than one independent variable not supported")}
  d <- data.frame(dv = data[[response]], id= as.factor(data[[id]]), iv = as.factor(data[[iv.names]]))
  if(length(levels(d$iv)) != 2) {return("indepenent variable must contain exactly two levels")}
  l <- levels(d$iv)
  temp1 <- d[ d$iv == l[1], 1:2]
  temp2 <- d[ d$iv == l[2], 1:2]
  names(temp1)[1] <- paste("dv", l[1], sep = "_")
  names(temp2)[1] <- paste("dv", l[2], sep = "_")
  d <- inner_join(temp1, temp2,  by = "id")
  d <-data.frame(var_1 = d[,1], var_2 = d[,3])
  d$var_1 <- ifelse(d$var_1 <= 0, NA, d$var_1)
  d$var_2 <- ifelse(d$var_2 <= 0, NA, d$var_2)
  d <- na.exclude(d)
  d$lnv1 <- log(d$var_1)
  d$lnv2 <- log(d$var_2)
  d$delta <- (d$var_2-d$var_1)
  d$pct_chg <- 100 * (d$var_2-d$var_1)/d$var_1
  d$lndelta <- d$lnv1-d$lnv2
  TE <- sd(d$delta)/sqrt(2)
  CD <- mean(d$delta) / sd(d$delta)
  CV <- 100*(exp((sd(d$lndelta)/sqrt(2))/100)-1)
  ICC <- irr::icc(d[,c(1,2)], model = "twoway", type = "agreement")$value
  #par(mfrow=c(1,3))
  plot(d$var_1, d$var_2, xlim=range(d$var_1, d$var_2), ylim=range(d$var_1, d$var_2), xlab = l[1], ylab = l[2], main = iv_name)
  abline(0, 1, col = "red", lty = 2); abline(lm(d$var_2 ~ d$var_1), lty = 1)  #Residuals vs. fitted of lm(var1, var2)
  boxplot(d[,1:2], names = c(l[1], l[2]),  main = iv_name)
  stripchart(d[,1:2],vertical = TRUE, add = TRUE, method = "jitter")
  hist(d$pct_chg, xlab="Percent Change (%)", xlim=c(-max(d$pct_chg)-15, max(d$pct_chg)+15), main = NULL, freq = FALSE, main = iv_name)
  lines(density(d$pct_chg, adjust = 1.2), col = "red")
  #par(mfrow=c(1,1))
  cat("\nCall:")
  cat("\n     Variable one: ", l[1], "\n     Variable two: ", l[2], "\n")
  cat("\nDifference in means:")
  print(t.test(d$var_1, d$var_2, paired = TRUE))
  cat("\nCohens d:", prettyNum(CD))
  cat("\nReliability:", "\n")
  cat("\n      Typical error: ", prettyNum(TE))
  cat("\n      Coefficient of Variability: ", prettyNum(CV))
  cat("\n      IntRAclass correlation coefficient: ", prettyNum(ICC))
  cat("\n      Pearson correlation coefficient (r, IntERclass correlation): ", prettyNum(cor(d$var_1, d$var_2)))
  cat("\n      Coefficient of Variability (r^2): ", prettyNum(cor(d$var_1, d$var_2)^2))
}