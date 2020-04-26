
############################################################
# R Code File Containing Problem / Solution Functions      #
############################################################

# Chapter 4

#### Define calculation function types

problistch4 <- list()

problistch4[[1]] <- function(SULT, age, nn, uu, mm) # $\Ex[n]{x}$
{
  nEx <- n.nEx(SULT, age, nn) 
  s.prob <- s.nEx(age, nn)
  s.ans <- paste0(s.nEx(age, nn),"=", s.times(s.tpx(age, nn), s.exp("v", nn)), "=", s.times(s.frac(s.ellx(age+nn), s.ellx(age)), s.exp("v", nn)), "=", s.times(s.frac(SULT[age+nn,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",nn,"}"))), "=", "(", round(SULT[age+nn,2] / SULT[age,2], 5),")(", round((1.05)^(-nn), 5),")","=",nEx)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = nEx))  
}

problistch4[[2]] <- function(SULT, age, nn, uu, mm) # $\Ax{\termxn}$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  termins <- n.termins(SULT, age, nn)
  s.prob <- s.termins(age, nn)
  s.ans <- paste0(s.termins(age, nn), "=", "A_{",age,"} - ", s.nEx(age, nn), "\\cdot A_{",age+nn,"} = ", SULT[age, 5], "- (", n.nEx(SULT, age, nn), ")(", SULT[age+nn, 5],") = ", termins)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = termins))  
}

problistch4[[3]] <- function(SULT, age, nn, uu, mm) # $\Ax*{\termxn}$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  terminsc <- round(n.termins(SULT, age, nn) * 1.0248, 5)
  s.prob <- s.terminsc(age, nn)
  s.ans <- paste0(s.terminsc(age, nn), "\\overset{UDD}{=} \\frac{i}{\\delta} \\cdot", s.termins(age, nn), " = (1.0248)\\left(A_{",age,"} - ", s.nEx(age, nn), "\\cdot A_{",age+nn,"} \\right) = (1.0248)\\left(", SULT[age, 5], "- (", n.nEx(SULT, age, nn), ")(", SULT[age+nn, 5],") \\right) = (1.0248)(", n.termins(SULT, age, nn), ") = ", terminsc)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = terminsc))
}

problistch4[[4]] <- function(SULT, age, nn, uu, mm) # $\Ax{\termxn}[\hspace{0.05in}(m)]$
{ 
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  if (mm == 2) adj <- 1.01235 else if (mm == 4) adj <- 1.01856 else adj <- 1.02271
  terminsm <- round(n.termins(SULT, age, nn) * adj, 5)
  s.prob <- s.terminsm(age, nn, mm)
  s.ans <- paste0(s.terminsm(age, nn, mm), "\\overset{UDD}{=} \\frac{i}{i^{(", mm,")}} \\cdot", s.termins(age, nn), " = (" , adj, ")\\left(A_{",age,"} - ", s.nEx(age, nn), "\\cdot A_{",age+nn,"} \\right) = (", adj, ")\\left(", SULT[age, 5], "- (", n.nEx(SULT, age, nn), ")(", SULT[age+nn, 5],") \\right) = (", adj, ")(", n.termins(SULT, age, nn), ") = ", terminsm)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = terminsm))
}

problistch4[[5]] <- function(SULT, age, nn, uu, mm) # $\Ax{x:\angl{2}}$ or $\Ax{x:\angl{3}}$ for $i \neq 5\%$
{
  ii <- sample(c(2:4,6:10), 1) # generate an int rate that's not 5%
  vv <- round(1/(1 + ii/100), 5)
  if(runif(1) < 0.5) nn <- 3 else nn <- 2
  if (age < 55) age <- age + 35 # 2 and 3 year term EPVs are too small for younger ages
  termins <- 0
  for (k in 0:(nn - 1)) termins <- termins + n.ktqx(SULT, age, k, 1) / (1 + ii/100)^(k + 1)
  s.prob <- paste0(s.termins(age, nn), " \\quad i = ", ii, "\\%")
  s.ans <- paste0(s.termins(age, nn), "=", s.tqx(age, 1), "\\cdot (",1 + ii/100,")^{-1}")
  for (k in 1:(nn - 1)) s.ans <- paste0(s.ans, "+", s.times(s.ktqx(age, k, 1), paste0("(",1 + ii/100, ")^{",-(k + 1),"}")))
  s.ans <- paste0(s.ans, " = (", n.tqx(SULT, age, 1), ")(", vv, ")")
  for (k in 1:(nn - 1)) s.ans <- paste0(s.ans, "+", "(", n.ktqx(SULT, age, k, 1), ")(", round(vv^(k + 1),5), ")")
  termins <- round(termins, 5)
  s.ans <- paste0(s.ans, " = ", termins)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = termins))  
}

problistch4[[6]] <- function(SULT, age, nn, uu, mm) # $\Ax*{x:\angl{2}}$ or $\Ax*{x:\angl{3}}$ for $i \neq 5\%$
{
  ii <- sample(c(2:4,6:10), 1) # generate an int rate that's not 5%
  vv <- round(1/(1 + ii/100), 5)
  if(runif(1) < 0.5) nn <- 3 else nn <- 2
  if (age < 55) age <- age + 35 # 2 and 3 year term EPVs are too small for younger ages
  termins <- 0
  for (k in 0:(nn - 1)) termins <- termins + n.ktqx(SULT, age, k, 1) / (1 + ii/100)^(k + 1)
  adj <- round((ii/100)/log(1 + ii/100), 5)
  terminsc <- round(adj * termins, 5)
  s.prob <- paste0(s.terminsc(age, nn), " \\quad i = ", ii, "\\%")
  s.ans <- paste0(s.terminsc(age, nn), "\\overset{UDD}{=} \\frac{i}{\\delta} \\cdot", s.termins(age, nn), " = (", adj, ")\\left(", s.tqx(age, 1), "\\cdot (",1 + ii/100,")^{-1}")
  for (k in 1:(nn - 1)) s.ans <- paste0(s.ans, "+", s.times(s.ktqx(age, k, 1), paste0("(",1 + ii/100, ")^{",-(k + 1),"}")))
  s.ans <- paste0(s.ans, "\\right)$  \n\\hspace*{0.2in}$\\displaystyle ")
  s.ans <- paste0(s.ans, " = (", adj, ")\\left[", n.tqx(SULT, age, 1), ")(", vv, ")")
  for (k in 1:(nn - 1)) s.ans <- paste0(s.ans, "+", "(", n.ktqx(SULT, age, k, 1), ")(", round(vv^(k + 1),5), ")")
  s.ans <- paste0(s.ans, "\\right] = ", terminsc)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = terminsc))  
}

problistch4[[7]] <- function(SULT, age, nn, uu, mm) # $\Ax*{x:\angl{2}}[(m)]$ or $\Ax*{x:\angl{3}}[(m)]$ for $i \neq 5\%$
{
  ii <- sample(c(2:4,6:10), 1) # generate an int rate that's not 5%
  vv <- round(1/(1 + ii/100), 5)
  if(runif(1) < 0.5) nn <- 3 else nn <- 2
  if (age < 55) age <- age + 35 # 2 and 3 year term EPVs are too small for younger ages
  termins <- 0
  for (k in 0:(nn - 1)) termins <- termins + n.ktqx(SULT, age, k, 1) / (1 + ii/100)^(k + 1)
  im <- ((1 + ii/100)^(1/mm) - 1) * mm
  adj <- round((ii/100)/im, 5)
  terminsm <- round(adj * termins, 5)
  s.prob <- paste0(s.terminsm(age, nn, mm), " \\quad i = ", ii, "\\%") 
  s.ans <- paste0(s.terminsm(age, nn, mm), "\\overset{UDD}{=} \\frac{i}{i^{(", mm, ")}} \\cdot", s.termins(age, nn), " = (", adj, ")\\left(", s.tqx(age, 1), "\\cdot (",1 + ii/100,")^{-1}")
  for (k in 1:(nn - 1)) s.ans <- paste0(s.ans, "+", s.times(s.ktqx(age, k, 1), paste0("(",1 + ii/100, ")^{",-(k + 1),"}")))
  s.ans <- paste0(s.ans, "\\right)$  \n\\hspace*{0.2in}$\\displaystyle ")
  s.ans <- paste0(s.ans, " = (", adj, ")\\left[", n.tqx(SULT, age, 1), ")(", vv, ")")
  for (k in 1:(nn - 1)) s.ans <- paste0(s.ans, "+", "(", n.ktqx(SULT, age, k, 1), ")(", round(vv^(k + 1),5), ")")
  s.ans <- paste0(s.ans, "\\right] = ", terminsm)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = terminsm))  
}

problistch4[[8]] <- function(SULT, age, nn, uu, mm) # $\Ax{\endowxn}$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  s.prob <- s.insend(age, nn)
  
  termins <- do.call(problistch4[[2]], list(SULT = SULT, age = age, nn = nn, uu = uu, mm = mm))
  nEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = nn, uu = uu, mm = mm))
  insend <- round(termins$num.ans + nEx$num.ans, 5)
  
  s.ans <- paste0(s.insend(age, nn), "=", termins$s.prob, "+", nEx$s.prob, "=", termins$num.ans, "+", nEx$num.ans, "=", insend, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", termins$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", nEx$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = insend))  
}

problistch4[[9]] <- function(SULT, age, nn, uu, mm) # $\Ax*{\endowxn}$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  s.prob <- s.insendc(age, nn)
  
  terminsc <- do.call(problistch4[[3]], list(SULT = SULT, age = age, nn = nn, uu = uu, mm = mm))
  nEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = nn, uu = uu, mm = mm))
  insendc <- round(terminsc$num.ans + nEx$num.ans, 5)
  
  s.ans <- paste0(s.insendc(age, nn), "=", terminsc$s.prob, "+", nEx$s.prob, "=", terminsc$num.ans, "+", nEx$num.ans, "=", insendc, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", terminsc$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", nEx$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = insendc))  
}

problistch4[[10]] <- function(SULT, age, nn, uu, mm) # $\Ax{\endowxn}[(m)]$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  s.prob <- s.insendm(age, nn, mm)
  
  terminsm <- do.call(problistch4[[4]], list(SULT = SULT, age = age, nn = nn, uu = uu, mm = mm))
  nEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = nn, uu = uu, mm = mm))
  insendm <- round(terminsm$num.ans + nEx$num.ans, 5)
  
  s.ans <- paste0(s.insendm(age, nn, mm), "=", terminsm$s.prob, "+", nEx$s.prob, "=", terminsm$num.ans, "+", nEx$num.ans, "=", insendm, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", terminsm$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", nEx$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = insendm))  
}

problistch4[[11]] <- function(SULT, age, nn, uu, mm) # $\Ax[u|]{x}$
{
  uu <- max(uu, 2)
  s.prob <- s.defwl(age, uu)
  
  uEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = uu, uu = nn, mm = mm))
  wl <- SULT[age + uu, 5]
  defwl <- round(uEx$num.ans * wl, 5)
  
  s.ans <- paste0(s.defwl(age, uu), "=", s.times(uEx$s.prob, paste0("A_{", age + uu,"}")), "= (", uEx$num.ans, ")(", wl, ") =", defwl, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", uEx$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defwl))  
}

problistch4[[12]] <- function(SULT, age, nn, uu, mm) # $\Ax*[u|]{x}$
{
  uu <- max(uu, 2)
  s.prob <- s.defwlc(age, uu)

  uEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = uu, uu = nn, mm = mm))
  wl <- SULT[age + uu, 5]
  adj <- 1.0248
  defwlc <- round(adj * uEx$num.ans * wl, 5)
  
  s.ans <- paste0(s.defwlc(age, uu), "\\overset{UDD}{=} \\frac{i}{\\delta} \\cdot ", uEx$s.prob, " \\cdot ", paste0("A_{", age + uu,"}"), "= (", adj, ")(", uEx$num.ans, ")(", wl, ") =", defwlc, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", uEx$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defwlc))  
}

problistch4[[13]] <- function(SULT, age, nn, uu, mm) # $\Ax[u|]{x}[(m)]$
{
  uu <- max(uu, 2)
  s.prob <- s.defwlm(age, uu, mm)
  
  uEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = uu, uu = nn, mm = mm))
  wl <- SULT[age + uu, 5]
  if (mm == 2) adj <- 1.01235 else if (mm == 4) adj <- 1.01856 else adj <- 1.02271
  defwlm <- round(adj * uEx$num.ans * wl, 5)
  
  s.ans <- paste0(s.defwlm(age, uu, mm), "\\overset{UDD}{=} \\frac{i}{i^{(", mm, ")}} \\cdot ", uEx$s.prob, " \\cdot ", paste0("A_{", age + uu,"}"), "= (", adj, ")(", uEx$num.ans, ")(", wl, ") =", defwlm, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", uEx$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defwlm))  
}

problistch4[[14]] <- function(SULT, age, nn, uu, mm) # $\Ax[u|]{\termxn}$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  s.prob <- s.deftermins(age, nn, uu) 
  
  termins <- do.call(problistch4[[2]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm))
  uEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = uu, uu = nn, mm = mm))
  deftermins <- round(termins$num.ans * uEx$num.ans, 5)

  s.ans <- paste0(s.deftermins(age, nn, uu), "=", s.times(uEx$s.prob, termins$s.prob),  "= (", uEx$num.ans, ")(", termins$num.ans, ") =", deftermins, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", uEx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", termins$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = deftermins))  
}

problistch4[[15]] <- function(SULT, age, nn, uu, mm) # $\Ax*[u|]{\termxn}$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  s.prob <- s.defterminsc(age, nn, uu) 
  
  terminsc <- do.call(problistch4[[3]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm))
  uEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = uu, uu = nn, mm = mm))
  defterminsc <- round(terminsc$num.ans * uEx$num.ans, 5)
  
  s.ans <- paste0(s.defterminsc(age, nn, uu), "=", s.times(uEx$s.prob, terminsc$s.prob),  "\\overset{UDD}{=} (", uEx$num.ans, ")(", terminsc$num.ans, ") =", defterminsc, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", uEx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", terminsc$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defterminsc))  
}

problistch4[[16]] <- function(SULT, age, nn, uu, mm) # $\Ax[u|]{\termxn}[\hspace{0.05in}(m)]$
{
  nn <- max(nn, 4); if ((nn == 10) || (nn == 20)) nn <- nn + 1
  s.prob <- s.defterminsm(age, nn, uu, mm) 
  
  terminsm <- do.call(problistch4[[4]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm))
  uEx <- do.call(problistch4[[1]], list(SULT = SULT, age = age, nn = uu, uu = nn, mm = mm))
  defterminsm <- round(terminsm$num.ans * uEx$num.ans, 5)
  
  s.ans <- paste0(s.defterminsm(age, nn, uu, mm), "=", s.times(uEx$s.prob, terminsm$s.prob),  "\\overset{UDD}{=} (", uEx$num.ans, ")(", terminsm$num.ans, ") =", defterminsm, "\\text{, where }$  \n\\hspace*{0.2in}$\\displaystyle", uEx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", terminsm$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defterminsm))  
}
