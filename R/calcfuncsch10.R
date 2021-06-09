
############################################################
# R Code File Containing Problem / Solution Functions      #
############################################################

# Chapter 10

#### Define calculation function types

problistch10 <- list()

problistch10[[1]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # ${_np_{xy}}$
{
  npxy <- round(n.tpx(SULT, age, nn) *  n.tpx(SULT, age2, nn), 5)
  s.prob <- paste0(s.npxy(age, age2, nn))
  s.ans <- paste0(s.npxy(age, age2, nn),"=", s.times(s.frac(s.ellx(age+nn), s.ellx(age)), s.frac(s.ellx(age2+nn), s.ellx(age2))), "=", s.times(s.frac(SULT[age+nn,2], SULT[age,2]), s.frac(SULT[age2+nn,2], SULT[age2,2])), "=", fbox(npxy, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=npxy))  
}

problistch10[[2]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # ${_nq_{xy}}$
{
  npxy <- do.call(problistch10[[1]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = nn, ansbox = FALSE))
  nqxy <- round(1 - npxy$num.ans, 5)
  s.prob <- paste0(s.nqxy(age, age2, nn))
  s.ans <- paste0(s.nqxy(age, age2, nn),"=", s.minus(1, s.npxy(age, age2, nn)), "=", s.minus(1, npxy$num.ans), "=", fbox(nqxy, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nqxy))
}

problistch10[[3]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\px[n]{\joint{xy}}$
{
  npxy <- do.call(problistch10[[1]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = nn, ansbox = FALSE))
  npxy2 <- n.tpx(SULT, age, nn) + n.tpx(SULT, age2, nn) - npxy$num.ans
  s.prob <- paste0(s.npxy2(age, age2, nn))
  s.ans <- paste0(s.npxy2(age, age2, nn),"=", s.minus(s.plus(s.tpx(age, nn), s.tpx(age2, nn)), s.npxy(age, age2, nn)), "=", s.minus(s.plus(n.tpx(SULT, age, nn), n.tpx(SULT, age2, nn)), npxy$num.ans), "=", fbox(npxy2, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=npxy2))
}

problistch10[[4]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\qx[n]{\joint{xy}}$
{ 
  nqxy2 <- round(n.tqx(SULT, age, nn) * n.tqx(SULT, age2, nn), 5)
  s.prob <- paste0(s.nqxy2(age, age2, nn))
  s.ans <- paste0(s.nqxy2(age, age2, nn),"=", s.times(s.tqx(age, nn), s.tqx(age2, nn)), "=", s.times(n.tqx(SULT, age, nn), n.tqx(SULT, age2, nn)), "=", fbox(nqxy2, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nqxy2))
}

problistch10[[5]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # ${_nE_{xy}}$
{
  npxy <- do.call(problistch10[[1]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = nn, ansbox = FALSE))
  vn <- round((1.05)^(-nn), 5)
  nExy <- round(npxy$num.ans*vn, 5)
  s.prob <- paste0(s.nExy(age, age2, nn))
  s.ans <- paste0(s.nExy(age, age2, nn),"=", s.times(s.npxy(age, age2, nn), s.exp("v", nn)), "=", s.times(npxy$num.ans, vn), "=", fbox(nExy, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nExy))
}

problistch10[[6]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # ${_nE_{\joint{xy}}}$
{
  npxy2 <- do.call(problistch10[[3]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = nn, ansbox = FALSE))
  vn <- round((1.05)^(-nn), 5)
  nExy2 <- round(npxy2$num.ans*vn, 5)
  s.prob <- paste0(s.nExy2(age, age2, nn))
  s.ans <- paste0(s.nExy2(age, age2, nn),"=", s.times(s.npxy2(age, age2, nn), s.exp("v", nn)), "=", s.times(npxy2$num.ans, vn), "=", fbox(nExy2, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", npxy2$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=nExy2))
}

problistch10[[7]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\Ax{\\itop{\\overanglebracket{xy}}:\\angl{n}}$
{
  nExy <- do.call(problistch10[[5]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = nn, ansbox = FALSE))
  WLcolnum <- ifelse(age==age2, 3, 7)
  Aterm2 <- round(SULTjt[age, WLcolnum] - nExy$num.ans*SULTjt[age+nn, WLcolnum], 5)
  s.prob <- paste0(s.terminsj2(age, age2, nn))
  s.ans <- paste0(s.terminsj2(age, age2, nn),"=", s.minus(s.WLjt(age, age2), s.times(s.nExy(age, age2, nn), s.WLjt(age+nn, age2+nn))), "=", s.minus(SULTjt[age, WLcolnum], s.times(nExy$num.ans, SULTjt[age+nn, WLcolnum])), "=", fbox(Aterm2, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", nExy$s.ans )
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=Aterm2))
}

problistch10[[8]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\Ax{\\itop{\\overline{xy}}:\\angl{n}}$
{
  jtterm <- do.call(problistch10[[7]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = nn, ansbox = FALSE))
  term1 <- n.termins(SULT, age, nn)
  term2 <- n.termins(SULT, age2, nn)
  lasttermins <- round(term1 + term2 - jtterm$num.ans, 5)
  s.prob <- paste0(s.terminsl2(age, age2, nn))
  s.ans <- paste0(s.terminsl2(age, age2, nn), "=", s.minus(s.plus(s.termins(age, nn), s.termins(age2, nn)), s.terminsj2(age, age2, nn)), "=", s.minus(s.plus(term1, term2), jtterm$num.ans), "=", fbox(lasttermins, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", jtterm$s.ans) 
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=lasttermins))
}

problistch10[[9]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\Ax{\\joint{xy}}$
{
  WLcolnum <- ifelse(age==age2, 3, 7)
  WLjt2 <- round(SULT[age, 5] + SULT[age2, 5] - SULTjt[age, WLcolnum], 5)
  s.prob <- paste0(s.WLjt2(age, age2))
  s.ans <- paste0(s.WLjt2(age, age2),"=", s.minus(s.plus(paste0("A_{", age, "}"), paste0("A_{", age2, "}")), s.WLjt(age, age2)), "=", s.minus(s.plus(SULT[age, 5], SULT[age2, 5]), SULTjt[age, WLcolnum]), "=", fbox(WLjt2, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=WLjt2))
}

problistch10[[10]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\ax**{x:y:\\angl{20}}$
{
  nExy <- do.call(problistch10[[5]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = 10, ansbox = FALSE))
  anncol <- ifelse(age == age2, 5, 9)
  annduejt <- round(SULTjt[age, anncol] + nExy$num.ans * SULTjt[age + 10, anncol], 5)
  s.prob <- s.termannduejt(age, age2, 20)
  s.ans <- paste0(s.termannduejt(age, age2, 20),"=", s.plus(s.termannduejt(age, age2, 10), s.times(s.nExy(age, age2, 10), s.termannduejt(age+10, age2+10, 10))), "=", s.plus(SULTjt[age, anncol], s.times(nExy$num.ans, SULTjt[age + 10, anncol])) ,"=", fbox(annduejt, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", nExy$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=annduejt))  
}

problistch10[[11]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\ax**{x|y}$
{
  anncol <- ifelse(age == age2, 2, 6)
  annduerev <- round(SULT[age2, 4] - SULTjt[age, anncol], 5)
  s.prob <- s.annduerev(age, age2)
  s.ans <- paste0(s.annduerev(age, age2),"=", s.minus(s.WLanndue(age2), s.WLannduejt(age, age2)), "=", s.minus(SULT[age2, 4], SULTjt[age, anncol]) ,"=", fbox(annduerev, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=annduerev))  
}

problistch10[[12]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\ax**{\\overline{xy}}$
{
  anncol <- ifelse(age == age2, 2, 6)
  annduelast <- round(SULT[age, 4] + SULT[age2, 4] - SULTjt[age, anncol], 5)
  s.prob <- s.WLannduelast(age, age2)
  s.ans <- paste0(s.WLannduelast(age, age2),"=", s.minus(s.plus(s.WLanndue(age), s.WLanndue(age2)), s.WLannduejt(age, age2)), "=", s.minus(s.plus(SULT[age, 4], SULT[age2, 4]), SULTjt[age, anncol]) ,"=", fbox(annduelast, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=annduelast))  
}

problistch10[[13]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\ax**{\\overline{xy}:\\angl{10}}$
{
  anncol <- ifelse(age == age2, 5, 9)
  annduelastterm <- round(SULT[age, 7] + SULT[age2, 7] - SULTjt[age, anncol], 5)
  s.prob <- s.termannduelast(age, age2, 10)
  s.ans <- paste0(s.termannduelast(age, age2, 10),"=", s.minus(s.plus(s.termanndue(age, 10), s.termanndue(age2, 10)), s.termannduejt(age, age2, 10)), "=", s.minus(s.plus(SULT[age, 7], SULT[age2, 7]), SULTjt[age, anncol]) ,"=", fbox(annduelastterm, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=annduelastterm))  
}

problistch10[[14]] <- function(SULT, SULTjt, age, age2, nn, ansbox = TRUE) # $\\ax**{\\overline{xy}:\\angl{20}}$
{
  anndue20jt <- do.call(problistch10[[10]], list(SULT = SULT, SULTjt = SULTjt, age = age, age2 = age2, nn = nn, ansbox = FALSE))
  annduelastterm <- round(SULT[age, 9] + SULT[age2, 9] - anndue20jt$num.ans, 5)
  s.prob <- s.termannduelast(age, age2, 20)
  s.ans <- paste0(s.termannduelast(age, age2, 20),"=", s.minus(s.plus(s.termanndue(age, 20), s.termanndue(age2, 20)), s.termannduejt(age, age2, 20)), "=", s.minus(s.plus(SULT[age, 9], SULT[age2, 9]), anndue20jt$num.ans) ,"=", fbox(annduelastterm, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", anndue20jt$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=annduelastterm))  
}
