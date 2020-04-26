
############################################################
# R Code File Containing Problem / Solution Functions      #
############################################################

# Chapter 5

#### Define calculation function types

problistch5 <- list()

problistch5[[1]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**{\endowxn}$
{
  anndue <- n.termanndue(SULT, age, nn)
  s.prob <- s.termanndue(age, nn)
  nEx <- n.nEx(SULT, age, nn)
  s.ans.nEx <- paste0(s.times(s.tpx(age, nn), s.exp("v", nn)), "=", s.times(s.frac(s.ellx(age+nn), s.ellx(age)), s.exp("v", nn)), "=", s.times(s.frac(SULT[age+nn,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",nn,"}"))), "=", "(", round(SULT[age+nn,2] / SULT[age,2], 5),")(", round((1.05)^(-nn), 5),")")
  s.ans <- paste0(s.termanndue(age, nn),"=", s.minus(s.WLanndue(age), s.times(s.nEx(age, nn), s.WLanndue(age + nn))),"=", s.minus(SULT[age, 4], s.times(n.nEx(SULT, age, nn), SULT[age + nn, 4])),"=", anndue, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle",  s.nEx(age, nn), "=", s.ans.nEx, "=", nEx)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = anndue))  
}

problistch5[[2]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**{\joint\endowxn}$
{
  ii <- 0.05; dd <- round(ii / (1 + ii), 5); vv <- round(1 - dd, 5)
  annguar <- round((1 - vv^nn)/dd + n.defann(SULT, age, nn), 5)
  s.prob <- s.annguardue(age, nn)
  nEx <- n.nEx(SULT, age, nn)
  s.ans.nEx <- paste0(s.times(s.tpx(age, nn), s.exp("v", nn)), "=", s.times(s.frac(s.ellx(age+nn), s.ellx(age)), s.exp("v", nn)), "=", s.times(s.frac(SULT[age+nn,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",nn,"}"))), "=", "(", round(SULT[age+nn,2] / SULT[age,2], 5),")(", round((1.05)^(-nn), 5),")")
  s.ans <- paste0(s.annguardue(age, nn), "=", s.plus(s.anncertdue(nn), s.times(s.nEx(age, nn), s.WLanndue(age + nn))), "=", s.frac(s.minus(1, round(vv^nn, 5)), round(dd, 5)), "+", s.times(n.nEx(SULT, age, nn), SULT[age + nn, 4]), "=", annguar, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle",  s.nEx(age, nn), "=", s.ans.nEx, "=", nEx)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = annguar))  
}

problistch5[[3]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax*{x}$ under UDD or W2
{
  s.prob <- paste0(s.contWLann(age), " \\text{ under }", FAA)
  if (FAA == "UDD")
  {
    alpha <- 1.0002; beta <- 0.50823
    contWLann <- n.WLannUDD(SULT, age, alpha, beta)
    s.ans <- paste0(s.contWLann(age), " \\overset{UDD}{=}", "\\alpha(\\infty) \\cdot \\ddot{a}_{", age, "} - \\beta(\\infty) = ", s.minus(s.times(alpha, SULT[age, 4]), beta), "=", contWLann)
  }  else
  {
    contWLann <- n.WLannW2(SULT, age, mm = Inf) 
    s.ans <- paste0(s.contWLann(age), " \\overset{W2}{=}", "\\ddot{a}_{", age, "} - 0.5 = ", SULT[age, 4], " - 0.5 = ", contWLann)
  }
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = contWLann))  
}

problistch5[[4]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**{x}[(m)]$ under UDD or W2
{
  s.prob <- paste0(s.mWLann(age, mm), " \\text{ under }", FAA)
  if (FAA == "UDD")
  {
    if (mm == 2) alpha <- 1.00015 else if (mm == 4) alpha <- 1.00019 else alpha <- 1.0002
    if (mm == 2) beta <- 0.25617 else if (mm == 4) beta <- 0.38272 else beta <- 0.46651
    contWLann <- round(alpha * SULT[age, 4] - beta, 5)
    s.ans <- paste0(s.mWLann(age, mm), " \\overset{UDD}{=}", "\\alpha(", mm, ") \\cdot \\ddot{a}_{", age, "} - \\beta(", mm, ") = ", s.minus(s.times(alpha, SULT[age, 4]), beta), "=", contWLann)
  }  else
  {  
    subterm <- round((mm - 1) / (2 * mm), 5)
    contWLann <- SULT[age, 4] - subterm
    s.ans <- paste0(s.mWLann(age, mm), " \\overset{W2}{=}", "\\ddot{a}_{", age, "} - ", s.frac(s.minus(mm, 1), s.times(2, mm))," = ", SULT[age, 4], " - ", subterm, " = ", contWLann)
  }
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = contWLann))  
}

problistch5[[5]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax*{\endowxn}$ under UDD or W2
{
  contWLannx <- do.call(problistch5[[3]], as.list(environment()))
  contWLannxplusn <- do.call(problistch5[[3]], list(SULT = SULT, age = age + nn, uu = uu, mm = mm, FAA = FAA))
  nEx <- n.nEx(SULT, age, nn)
  s.prob <- paste0(s.conttermann(age, nn), " \\text{ under }", FAA)
  s.ans.nEx <- paste0(s.times(s.tpx(age, nn), s.exp("v", nn)), "=", s.times(s.frac(s.ellx(age+nn), s.ellx(age)), s.exp("v", nn)), "=", s.times(s.frac(SULT[age+nn,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",nn,"}"))), "=", "(", round(SULT[age+nn,2] / SULT[age,2], 5),")(", round((1.05)^(-nn), 5),")")
  if (FAA == "UDD")
  {
    alpha <- 1.0002; beta <- 0.50823
    conttermann <- round(n.WLannUDD(SULT, age, alpha, beta) - n.nEx(SULT, age, nn) * n.WLannUDD(SULT, age + nn, alpha, beta), 5)
    s.ans <- paste0(s.conttermann(age, nn), " = ", s.minus(s.contWLann(age), s.times(s.nEx(age, nn), s.contWLann(age + nn))), " \\overset{UDD}{=}", s.minus(contWLannx$num.ans, s.times(nEx, contWLannxplusn$num.ans)),"=", conttermann, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", contWLannx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", contWLannxplusn$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, nn), "=", s.ans.nEx, "=", nEx)
  }  else
  {
    conttermann <- round(n.WLannW2(SULT, age, mm = Inf) - n.nEx(SULT, age, nn) * n.WLannW2(SULT, age + nn, mm = Inf), 5)
    s.ans <- paste0(s.conttermann(age, nn), " = ", s.minus(s.contWLann(age), s.times(s.nEx(age, nn), s.contWLann(age + nn))),  "\\overset{W2}{=}", s.minus(contWLannx$num.ans, s.times(nEx, contWLannxplusn$num.ans)),"=", conttermann, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", contWLannx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", contWLannxplusn$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, nn), "=", s.ans.nEx, "=", nEx)
  }
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = conttermann))  
}

problistch5[[6]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**{\endowxn}[(m)]$ under UDD or W2
{
  mWLannx <- do.call(problistch5[[4]], as.list(environment()))
  mWLannxplusn <- do.call(problistch5[[4]], list(SULT = SULT, age = age + nn, uu = uu, mm = mm, FAA = FAA))
  nEx <- n.nEx(SULT, age, nn)
  s.prob <- paste0(s.mtermann(age, nn, mm), " \\text{ under }", FAA)
  s.ans.nEx <- paste0(s.times(s.tpx(age, nn), s.exp("v", nn)), "=", s.times(s.frac(s.ellx(age+nn), s.ellx(age)), s.exp("v", nn)), "=", s.times(s.frac(SULT[age+nn,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",nn,"}"))), "=", "(", round(SULT[age+nn,2] / SULT[age,2], 5),")(", round((1.05)^(-nn), 5),")")
  if (FAA == "UDD")
  {
    if (mm == 2) alpha <- 1.00015 else if (mm == 4) alpha <- 1.00019 else alpha <- 1.0002
    if (mm == 2) beta <- 0.25617 else if (mm == 4) beta <- 0.38272 else beta <- 0.46651
    mtermann <- round(n.WLannUDD(SULT, age, alpha, beta) - n.nEx(SULT, age, nn) * n.WLannUDD(SULT, age + nn, alpha, beta), 5)
    s.ans <- paste0(s.mtermann(age, nn, mm), " = ", s.minus(s.mWLann(age, mm), s.times(s.nEx(age, nn), s.mWLann(age + nn, mm))), " \\overset{UDD}{=}", s.minus(mWLannx$num.ans, s.times(nEx, mWLannxplusn$num.ans)),"=", mtermann, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", mWLannx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", mWLannxplusn$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, nn), "=", s.ans.nEx, "=", nEx)
  }  else
  {
    mtermann <- round(n.WLannW2(SULT, age, mm) - n.nEx(SULT, age, nn) * n.WLannW2(SULT, age + nn, mm), 5)
    s.ans <- paste0(s.mtermann(age, nn, mm), " = ", s.minus(s.mWLann(age, mm), s.times(s.nEx(age, nn), s.mWLann(age + nn, mm))),  "\\overset{W2}{=}", s.minus(mWLannx$num.ans, s.times(nEx, mWLannxplusn$num.ans)),"=", mtermann, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", mWLannx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", mWLannxplusn$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, nn), "=", s.ans.nEx, "=", nEx)
  }
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = mtermann))  
}

problistch5[[7]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**[u|]{x}$
{
  defanndue <- n.defann(SULT, age, uu) 
  uEx <- n.nEx(SULT, age, uu)
  s.ans.uEx <- paste0(s.times(s.tpx(age, uu), s.exp("v", uu)), "=", s.times(s.frac(s.ellx(age+uu), s.ellx(age)), s.exp("v", uu)), "=", s.times(s.frac(SULT[age+uu,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",uu,"}"))), "=", "(", round(SULT[age+uu,2] / SULT[age,2], 5),")(", round((1.05)^(-uu), 5),")")
  s.prob <- s.defWLanndue(age, uu) 
  s.ans <- paste0(s.defWLanndue(age, uu),"=", s.times(s.nEx(age, uu), s.WLanndue(age + uu)),"=", s.times(n.nEx(SULT, age, uu), SULT[age + nn, 4]),"=", defanndue, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, uu), "=", s.ans.uEx, "=", uEx)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defanndue))  
}

problistch5[[8]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**[u|]{\endowxn}$
{
  termanndue <- do.call(problistch5[[1]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm, FAA = FAA))
  uEx <- n.nEx(SULT, age, uu)
  s.ans.uEx <- paste0(s.times(s.tpx(age, uu), s.exp("v", uu)), "=", s.times(s.frac(s.ellx(age+uu), s.ellx(age)), s.exp("v", uu)), "=", s.times(s.frac(SULT[age+uu,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",uu,"}"))), "=", "(", round(SULT[age+uu,2] / SULT[age,2], 5),")(", round((1.05)^(-uu), 5),")")
  deftermanndue <- n.deftermanndue(SULT, age, nn, uu) 
  s.prob <- s.deftermanndue(age, nn, uu)
  s.ans <- paste0(s.deftermanndue(age, nn, uu),"=", s.times(s.nEx(age, uu), s.termanndue(age + uu, nn)),"=", s.times(n.nEx(SULT, age, uu), termanndue$num.ans),"=", deftermanndue, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, uu), "=", s.ans.uEx, "=", uEx, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", termanndue$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = deftermanndue))  
}

problistch5[[9]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**[u|]{\joint\endowxn}$
{
  guaranndue <- do.call(problistch5[[2]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm, FAA = FAA))
  uEx <- n.nEx(SULT, age, uu)
  s.ans.uEx <- paste0(s.times(s.tpx(age, uu), s.exp("v", uu)), "=", s.times(s.frac(s.ellx(age+uu), s.ellx(age)), s.exp("v", uu)), "=", s.times(s.frac(SULT[age+uu,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",uu,"}"))), "=", "(", round(SULT[age+uu,2] / SULT[age,2], 5),")(", round((1.05)^(-uu), 5),")")
  defguartermanndue <- guaranndue$num.ans * uEx 
  s.prob <- s.defguartermanndue(age, nn, uu) 
  s.ans <- paste0(s.defguartermanndue(age, nn, uu),"=", s.times(s.nEx(age, uu), s.annguardue(age + uu, nn)),"=", s.times(n.nEx(SULT, age, uu), guaranndue$num.ans),"=", defguartermanndue, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, uu), "=", s.ans.uEx, "=", uEx, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", guaranndue$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defguartermanndue))  
}

problistch5[[10]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax*[u|]{x}$ under UDD or W2
{
  contanndue <- do.call(problistch5[[3]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm, FAA = FAA))
  uEx <- n.nEx(SULT, age, uu)
  s.ans.uEx <- paste0(s.times(s.tpx(age, uu), s.exp("v", uu)), "=", s.times(s.frac(s.ellx(age+uu), s.ellx(age)), s.exp("v", uu)), "=", s.times(s.frac(SULT[age+uu,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",uu,"}"))), "=", "(", round(SULT[age+uu,2] / SULT[age,2], 5),")(", round((1.05)^(-uu), 5),")")
  defcontanndue <- round(contanndue$num.ans * uEx, 5) 
  s.prob <- paste0(s.defcontWLann(age, uu)," \\text{ under }", FAA)
  s.ans <- paste0(s.defcontWLann(age, uu),"=", s.times(s.nEx(age, uu), s.contWLann(age + uu)),"\\overset{", FAA, "}{=}", s.times(n.nEx(SULT, age, uu), contanndue$num.ans),"=", defcontanndue, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, uu), "=", s.ans.uEx, "=", uEx, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", contanndue$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = contanndue))  
}

problistch5[[11]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**[u|]{x}[(m)]$ under UDD or W2
{ 
  manndue <- do.call(problistch5[[4]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm, FAA = FAA))
  uEx <- n.nEx(SULT, age, uu)
  s.ans.uEx <- paste0(s.times(s.tpx(age, uu), s.exp("v", uu)), "=", s.times(s.frac(s.ellx(age+uu), s.ellx(age)), s.exp("v", uu)), "=", s.times(s.frac(SULT[age+uu,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",uu,"}"))), "=", "(", round(SULT[age+uu,2] / SULT[age,2], 5),")(", round((1.05)^(-uu), 5),")")
  defmanndue <- round(manndue$num.ans * uEx, 5) 
  s.prob <- paste0(s.defmWLann(age, uu, mm)," \\text{ under }", FAA)
  s.ans <- paste0(s.defmWLann(age, uu, mm),"=", s.times(s.nEx(age, uu), s.mWLann(age + uu, mm)),"\\overset{", FAA, "}{=}", s.times(n.nEx(SULT, age, uu), manndue$num.ans),"=", defmanndue, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, uu), "=", s.ans.uEx, "=", uEx, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", manndue$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defmanndue))  
}

problistch5[[12]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax*[u|]{\endowxn}$ under UDD or W2
{
  conttermann <- do.call(problistch5[[5]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm, FAA = FAA))
  uEx <- n.nEx(SULT, age, uu)
  s.ans.uEx <- paste0(s.times(s.tpx(age, uu), s.exp("v", uu)), "=", s.times(s.frac(s.ellx(age+uu), s.ellx(age)), s.exp("v", uu)), "=", s.times(s.frac(SULT[age+uu,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",uu,"}"))), "=", "(", round(SULT[age+uu,2] / SULT[age,2], 5),")(", round((1.05)^(-uu), 5),")")
  defconttermann <- round(conttermann$num.ans * uEx, 5) 
  s.prob <- paste0(s.defconttermann(age, nn, uu)," \\text{ under }", FAA)
  s.ans <- paste0(s.defconttermann(age, nn, uu),"=", s.times(s.nEx(age, uu), s.conttermann(age + uu, nn)),"\\overset{", FAA, "}{=}", s.times(n.nEx(SULT, age, uu), conttermann$num.ans),"=", defconttermann, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, uu), "=", s.ans.uEx, "=", uEx, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", conttermann$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defconttermann))  
}

problistch5[[13]] <- function(SULT, age, nn, uu, mm, FAA) # $\ax**[u|]{\endowxn}[(m)]$ under UDD or W2
{ 
  mtermanndue <- do.call(problistch5[[6]], list(SULT = SULT, age = age + uu, nn = nn, uu = uu, mm = mm, FAA = FAA))
  uEx <- n.nEx(SULT, age, uu)
  s.ans.uEx <- paste0(s.times(s.tpx(age, uu), s.exp("v", uu)), "=", s.times(s.frac(s.ellx(age+uu), s.ellx(age)), s.exp("v", uu)), "=", s.times(s.frac(SULT[age+uu,2], SULT[age,2]) , s.frac(1, paste0("(1.05)^{",uu,"}"))), "=", "(", round(SULT[age+uu,2] / SULT[age,2], 5),")(", round((1.05)^(-uu), 5),")")
  defmtermanndue <- round(mtermanndue$num.ans * uEx, 5) 
  s.prob <- paste0(s.defmtermann(age, nn, uu, mm)," \\text{ under }", FAA)
  s.ans <- paste0(s.defmtermann(age, nn, uu, mm),"=", s.times(s.nEx(age, uu), s.mtermann(age + uu, nn, mm)),"\\overset{", FAA, "}{=}", s.times(n.nEx(SULT, age, uu), mtermanndue$num.ans),"=", defmtermanndue, ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", s.nEx(age, uu), "=", s.ans.uEx, "=", uEx, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", mtermanndue$s.ans)
  return(list(s.prob = s.prob, s.ans = s.ans, num.ans = defmtermanndue))  
}
