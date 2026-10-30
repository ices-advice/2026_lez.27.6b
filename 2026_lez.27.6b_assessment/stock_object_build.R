#### Create stock object ####
library (FLCore)
# library(FLBeia)

spict2flbeia <- function (spict_fit, wt_units = "kg", n_units = "10^3", catch_units = "t",
                          stock_name = "stk", disc = NULL)
{
  Bs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE))
  Bs$time <- as.numeric(rownames(Bs))
  Bs$year <- floor(Bs$time)
  yrs <- sort(unique(Bs$year))
  tmp <- data.frame(year = yrs)
  tmp$B <- Bs$est[match(tmp$year, Bs$time)]
  
  
  
  flq <- FLQuant(tmp$B, dim = c(1, nrow(tmp)), dimnames = list(age = 1,
                                                               year = tmp$year), units = "t")
  stock <- FLStock(stock = flq, name = stock_name)
  stock@stock.wt[1, ] <- 1
  stock@stock.n <- stock@stock/stock@stock.wt
  stock@stock.wt@units <- wt_units
  stock@stock.n@units <- n_units
  Fs <- as.data.frame(get.par("logF", spict_fit, exp = TRUE))
  Fs$time <- as.numeric(rownames(Fs))
  Fs$year <- floor(Fs$time)
  tmp <- aggregate(Fs$est, list(year = Fs$year), FUN = mean)
  names(tmp)[which(names(tmp) == "x")] <- "f"
  stock@harvest[, ac(yrs)] <- tmp$f[match(yrs, tmp$year)]
  stock@harvest@units <- "f"
  
  Cs <- as.data.frame(get.par("logB", spict_fit, exp = TRUE) *
                        get.par("logF", spict_fit, exp = TRUE) * spict_fit$inp$dt)
  Cs$time <- as.numeric(rownames(Cs))
  Cs$year <- floor(Cs$time)
  tmp <- aggregate(Cs$est, list(year = Cs$year), FUN = sum)
  names(tmp)[which(names(tmp) == "x")] <- "catch"
  stock@catch[, ac(yrs)] <- tmp$catch[match(yrs, tmp$year)]
  stock@catch.wt[, ] <- 1
  stock@catch.n[] <- c(stock@catch/stock@catch.wt)
  stock@catch@units <- catch_units
  stock@catch.wt@units <- wt_units
  stock@catch.n@units <- n_units
  if (!is.null(disc)) {
    stock@discards[, ac(yrs)] <- disc
  }
  else {
    stock@discards[, ac(yrs)] <- 0
  }
  stock@discards.wt[1, ] <- 1
  stock@discards.n[] <- c(stock@discards/stock@discards.wt)
  stock@discards@units <- catch_units
  stock@discards.wt@units <- wt_units
  stock@discards.n@units <- n_units
  stock@landings <- stock@catch - stock@discards
  stock@landings.wt[1, ] <- 1
  stock@landings.n[] <- c(stock@landings/stock@landings.wt)
  stock@landings@units <- catch_units
  stock@landings.wt@units <- wt_units
  stock@landings.n@units <- n_units
  stock@mat[1, ] <- 1
  stock@harvest.spwn[1, ] <- 0
  stock@m[1, ] <- 0
  stock@m.spwn[1, ] <- 0
  tab1 <- sumspict.parest(spict_fit)
  tab3 <- sumspict.states(spict_fit)
  tab5 <- sumspict.predictions(spict_fit)
  r.stk <- (get.par("logm", spict_fit, exp = T)[2] * get.par("logn",
                                                             spict_fit, exp = T)[2]^(get.par("logn", spict_fit, exp = T)[2]/(get.par("logn",
                                                                                                                                     spict_fit, exp = T)[2] - 1)))/get.par("logK", spict_fit, exp = T)[2]
  K.stk <- get.par("logK", spict_fit, exp = T)[2]
  p.stk <- get.par("logn", spict_fit, exp = T)[2] - 1
  res <- list()
  res$stk <- stock
  res$BDinfo$par.fixed <- spict_fit$par.fixed
  res$BDinfo$cov.fixed <- spict_fit$cov.fixed
  res$BDinfo$PellaTomlinson_pars <- c(r = r.stk, K = K.stk,
                                      p = p.stk)
  res$BDinfo$refPts <- c(Fmsy = get.par("logFmsy", spict_fit,
                                        exp = TRUE)[, "est"], Bmsy = get.par("logBmsy", spict_fit,
                                                                             exp = TRUE)[, "est"], MSY = get.par("MSY", spict_fit)[,
                                                                                                                                   "est"])
  return(res)
}

load("output/res.RData")


lez.27.6b_StockObj <- spict2flbeia(res)

save(lez.27.6b_StockObj, file = "output/lez.27.6b_StockObj.RData")
















