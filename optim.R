

optfun <- function(pars, pr0, preds, model, prices) {
#print(pars)
  if (any(pars < 0)) return(Inf)
  if (any(pars > 400)) return(Inf)#
#  pars <- round(pars, 1)
  preds$N_fertilizer <- pars[1]
  preds$P_fertilizer <- pars[2]
  preds$K_fertilizer <- pars[3]
  pr <- predict(model, preds)
 profit <- (pr - pr0) * prices$Cp - (pars[1] * prices$Np) - (pars[2] * prices$Pp) - (pars[3] * prices$Kp)
 -profit
#print(out)
#out
}

########### estimate optimal fert
gridfun <- function(preds, model) {
  if (any(is.na(preds))) return(rep(NA, 3))
  preds <- data.frame(t(preds))
  preds$N_fertilizer <- preds$P_fertilizer <- preds$K_fertilizer <- 0
#  print(head(preds))
  prices <- data.frame(Cp=.1, Np=1, Pp=1, Kp=1)
  pr0 <- predict(model, preds)
  opt <- optim(c(100, 50, 50), optfun, method="BFGS", pr0=pr0, preds=preds, prices=prices, model=model)
  #opt <- nlm(optfun, c(100, 50, 50), pr0=pr0, preds=preds, prices=prices, model=model)
  #opt <- nlm(optfun, c(100, 150, 50), pr0=pr0, preds=preds, prices=prices, model=model, ndigit=2, stepmax=100, steptol=5)
  round(opt$estimate)
}

# system.time(optimNPK <- terra::app(rp, gridfun, model=rfm, wopt=list(names=c("N", "P", "K"))))
optimNPK <- rast(rp, np)

