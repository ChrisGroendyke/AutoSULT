#### Functions to calculate commonly used functions

n.tpx <- function(SULT, age, tt) return(round(SULT[age + tt,2] / SULT[age,2], 5))
n.tqx <- function(SULT, age, tt) return(round(1 - n.tpx(SULT, age, tt),5))
n.ktqx <- function(SULT, age, kk, tt) return(round(n.tpx(SULT, age, kk) * n.tqx(SULT, age + kk, tt),5))
n.nEx <- function(SULT, age, nn, ii = 0.05) return(round(n.tpx(SULT, age, nn) * (1 + ii)^(-nn),5))
n.termins <- function(SULT, age, nn) return(round(SULT[age, 5] - n.nEx(SULT, age, nn) * SULT[age + nn, 5],5))
n.termanndue <- function(SULT, age, nn) if (nn == 1) return(1) else return(round(SULT[age, 4] - n.nEx(SULT, age, nn) * SULT[age + nn, 4],5))
n.defann <- function(SULT, age, uu) return(round(n.nEx(SULT, age, uu) * SULT[age + uu, 4],5))
n.WLannUDD <- function(SULT, age, alpha, beta) return(round(alpha * SULT[age, 4] - beta, 5))
n.WLannW2 <- function(SULT, age, mm) return(round(SULT[age, 4] - ifelse(mm == Inf, 0.5, (mm - 1)/(2*mm)), 5))
n.deftermanndue <- function(SULT, age, nn, uu) return(round(n.nEx(SULT, age, uu) * n.termanndue(SULT, age + uu, nn), 5))