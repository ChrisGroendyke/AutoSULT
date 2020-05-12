
############################################################
# R Code File Containing Problem / Solution Functions      #
############################################################

# Chapter 9

#### Define calculation function types

problistch9 <- list()

problistch9[[1]] <- function(SULT, SULTjt, age, age2, nn) # ${_np_{xy}}$
{
  npxy <- round(n.tpx(SULT, age, nn) *  n.tpx(SULT, age2, nn), 5)
  s.prob <- paste0(s.npxy(age, age2, nn))
  s.ans <- paste0(s.npxy(age, age2, nn),"=", s.times(s.frac(s.ellx(age+nn), s.ellx(age)), s.frac(s.ellx(age2+nn), s.ellx(age2))), "=", s.times(s.frac(SULT[age+nn,2], SULT[age,2]), s.frac(SULT[age2+nn,2], SULT[age2,2])), "=", npxy)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=npxy))  
}

problistch9[[2]] <- function(SULT, SULTjt, age, age2, nn) # ${_nq_{xy}}$
{
  npxy <- do.call(problistch9[[1]], as.list(environment()))
  nqxy <- round(1 - npxy$num.ans, 5)
  s.prob <- paste0(s.nqxy(age, age2, nn))
  s.ans <- paste0(s.nqxy(age, age2, nn),"=", s.minus(1, s.npxy(age, age2, nn)), "=", s.minus(1, npxy$num.ans), "=", nqxy, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nqxy))  
}

problistch9[[3]] <- function(SULT, SULTjt, age, age2, nn) # $\px[n]{\joint{xy}}$
{
  npxy <- do.call(problistch9[[1]], as.list(environment()))
  npxy2 <- n.tpx(SULT, age, nn) + n.tpx(SULT, age2, nn) - npxy$num.ans
  s.prob <- paste0(s.npxy2(age, age2, nn))
  s.ans <- paste0(s.npxy2(age, age2, nn),"=", s.minus(s.plus(s.tpx(age, nn), s.tpx(age2, nn)), s.npxy(age, age2, nn)), "=", s.minus(s.plus(n.tpx(SULT, age, nn), n.tpx(SULT, age2, nn)), npxy$num.ans), "=", npxy2, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=npxy2))  
}

problistch9[[4]] <- function(SULT, SULTjt, age, age2, nn) # $\qx[n]{\joint{xy}}$
{ 
  nqxy2 <- round(n.tqx(SULT, age, nn) * n.tqx(SULT, age2, nn), 5)
  s.prob <- paste0(s.nqxy2(age, age2, nn))
  s.ans <- paste0(s.nqxy2(age, age2, nn),"=", s.times(s.tqx(age, nn), s.tqx(age2, nn)), "=", s.times(n.tqx(SULT, age, nn), n.tqx(SULT, age2, nn)), "=", nqxy2)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nqxy2))  
}

problistch9[[5]] <- function(SULT, SULTjt, age, age2, nn) # ${_nE_{xy}}$
{
  npxy <- do.call(problistch9[[1]], as.list(environment()))
  vn <- round((1.05)^(-nn), 5)
  nExy <- round(npxy$num.ans*vn, 5)
  s.prob <- paste0(s.nExy(age, age2, nn))
  s.ans <- paste0(s.nExy(age, age2, nn),"=", s.times(s.npxy(age, age2, nn), s.exp("v", nn)), "=", s.times(npxy$num.ans, vn), "=", nExy, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nExy))  
}

problistch9[[6]] <- function(SULT, SULTjt, age, age2, nn) # ${_nE_{\joint{xy}}}$
{
  npxy2 <- do.call(problistch9[[3]], as.list(environment()))
  vn <- round((1.05)^(-nn), 5)
  nExy2 <- round(npxy2$num.ans*vn, 5)
  s.prob <- paste0(s.nExy2(age, age2, nn))
  s.ans <- paste0(s.nExy2(age, age2, nn),"=", s.times(s.npxy2(age, age2, nn), s.exp("v", nn)), "=", s.times(npxy2$num.ans, vn), "=", nExy2, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy2$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nExy2))  
}

problistch9[[7]] <- function(SULT, SULTjt, age, age2, nn) # $\\Ax{\\itop{\\overanglebracket{xy}}:\\angl{n}}$
{
  nExy <- do.call(problistch9[[5]], as.list(environment()))
  WLcolnum <- ifelse(age==age2, 3, 7)
  Aterm2 <- round(SULTjt[age, WLcolnum] - nExy$num.ans*SULTjt[age+nn, WLcolnum], 5)
  s.prob <- paste0(s.terminsj2(age, age2, nn))
  s.ans <- paste0(s.terminsj2(age, age2, nn),"=", s.minus(s.WLjt(age, age2), s.times(s.nExy(age, age2, nn), s.WLjt(age+nn, age2+nn))), "=", s.minus(SULTjt[age, WLcolnum], s.times(nExy$num.ans, SULTjt[age+nn, WLcolnum])), "=", Aterm2, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", nExy$s.ans )
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=Aterm2))  
}

problistch9[[8]] <- function(SULT, SULTjt, age, age2, nn) # $\\Ax{\\joint{xy}}$
{
  WLcolnum <- ifelse(age==age2, 3, 7)
  WLjt2 <- round(SULT[age, 5] + SULT[age2, 5] - SULTjt[age, WLcolnum], 5)
  s.prob <- paste0(s.WLjt2(age, age2))
  s.ans <- paste0(s.WLjt2(age, age2),"=", s.minus(s.plus(paste0("A_{", age, "}"), paste0("A_{", age2, "}")), s.WLjt(age, age2)), "=", s.minus(s.plus(SULT[age, 5], SULT[age2, 5]), SULTjt[age, WLcolnum]), "=", WLjt2)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=WLjt2))  
}

problistch9[[9]] <- function(SULT, SULTjt, age, age2, nn) # $\\ax**{x:y:\\angl{20}}$
{
  nExy <- do.call(problistch9[[5]], as.list(environment()))
  anncol <- ifelse(age == age2, 5, 9)
  annduejt <- round(SULTjt[age, anncol] + nExy$num.ans * SULTjt[age + 10, anncol], 5)
  s.prob <- s.termannduejt(age, age2, 20)
  s.ans <- paste0(s.termannduejt(age, age2, 20),"=", s.plus(s.termannduejt(age, age2, 10), s.times(s.nExy(age, age2, 10), s.termannduejt(age+10, age2+10, 10))), "=", s.plus(SULTjt[age, anncol], s.times(nExy$num.ans, SULTjt[age + 10, anncol])) ,"=", annduejt, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", nExy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=annduejt))  
}

problistch9[[10]] <- function(SULT, SULTjt, age, age2, nn) # $\\ax**{x|y}$
{
  anncol <- ifelse(age == age2, 2, 6)
  annduerev <- round(SULT[age2, 4] - SULTjt[age, anncol], 5)
  s.prob <- s.annduerev(age, age2)
  s.ans <- paste0(s.annduerev(age, age2),"=", s.minus(s.WLanndue(age2), s.WLannduejt(age, age2)), "=", s.minus(SULT[age2, 4], SULTjt[age, anncol]) ,"=", annduerev)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=annduerev))  
}
