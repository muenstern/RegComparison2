
reg_comparison <- function(reg1,reg2) {
  est1<-coef(reg1)
  beta1<-est1[2]
  est2<-coef(reg2)
  beta2<-est2[2]
  error1<-sqrt(diag(vcov(reg1)))
  beta1sd<-error1[2]
  error2<-sqrt(diag(vcov(reg2)))
  beta2sd<-error2[2]
  z_value = (beta1 - beta2)/(sqrt(((beta1sd)^2) + ((beta2sd)^2)))
  p <- pnorm(z_value, 0, 1)
  if (p < 0.05) {sig = "*"} else {sig = ""}
  df_reg_comparison <- data.frame(Z = (z_value),p = (p), significant = (sig),
                                  beta_1 = (beta1), beta_2=(beta2),
                                  beta_1SD = (beta1sd), beta_2SD=(beta2sd))
  kable(df_reg_comparison)
}




#' Compare Regression Models
#'
#' This function compares the coefficients of two regression models and performs a significance test.
#'
#' @param reg1 The first regression model.
#' @param reg2 The second regression model.
#' @return A data frame containing the results of the comparison.
#' @examples
#' library(knitr)
#' reg1 <- lm(y ~ x, data = data1)
#' reg2 <- lm(y ~ x, data = data2)
#' comparison <- reg_comparison(reg1, reg2)
#' print(comparison)
#' @import knitr
#' @export
reg_comparison <- function(reg1, reg2) {
  est1 <- coef(reg1)
  beta1 <- est1[2]
  est2 <- coef(reg2)
  beta2 <- est2[2]
  error1 <- sqrt(diag(vcov(reg1)))
  beta1sd <- error1[2]
  error2 <- sqrt(diag(vcov(reg2)))
  beta2sd <- error2[2]
  z_value <- (beta1 - beta2) / sqrt(beta1sd^2 + beta2sd^2)
  p <- pnorm(z_value, 0, 1)
  if (p < 0.05) {
    sig <- "*"
  } else {
    sig <- ""
  }
  df_reg_comparison <- data.frame(
    Z = z_value,
    p = p,
    significant = sig,
    beta_1 = beta1,
    beta_2 = beta2,
    beta_1SD = beta1sd,
    beta_2SD = beta2sd
  )
  kable(df_reg_comparison)
}
