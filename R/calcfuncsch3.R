
############################################################
# R Code File Containing Problem / Solution Functions      #
############################################################

# Chapter 3

#### Define calculation function types

problistch3 <- list()

problistch3[[1]] <- function(SULT, age, tt, kk, rr, ss, ww, FAA, ansbox = TRUE) # ${_tp_x}$
{
  tpx <- n.tpx(SULT, age, tt) 
  s.prob <- paste0(s.tpx(age, tt))
  s.ans <- paste0(s.tpx(age, tt),"=", s.frac(s.ellx(age+tt), s.ellx(age)),"=", s.frac(SULT[age+tt,2], SULT[age,2]), "=",fbox(tpx, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx))  
}

problistch3[[2]] <- function(SULT, age, tt, kk, rr, ss, ww, FAA, ansbox = TRUE) # ${_tq_x}$
{
  tqx <- n.tqx(SULT, age, tt) 
  s.prob <- paste0(s.tqx(age, tt))
  s.ans <- paste0(s.tqx(age, tt),"=", s.frac(s.minus(s.ellx(age),s.ellx(age+tt)),s.ellx(age)), "=", s.frac(s.minus( SULT[age,2], SULT[age+tt,2]), SULT[age,2]), "=",fbox(tqx, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tqx))  
}

problistch3[[3]] <- function(SULT, age, tt, kk, rr, ss, ww, FAA, ansbox = TRUE) # ${_k|_tq_x}$
{
  ktqx <- n.ktqx(SULT, age, kk, tt)
  s.prob <- paste0(s.ktqx(age, kk, tt))
  s.ans <- paste0(s.ktqx(age, kk, tt),"=", s.frac(s.minus(s.ellx(age+kk),s.ellx(age+kk+tt)),s.ellx(age)), "=", s.frac(s.minus( SULT[age+kk,2], SULT[age+kk+tt,2]), SULT[age,2]), "=",fbox(ktqx, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=ktqx))  
}

problistch3[[4]] <- function(SULT, age, tt, kk, rr, ss, ww, FAA, ansbox = TRUE) # ${_{t+s}p_{x+r}},$ ($s, t$ and/or $r$ may be 0), UDD / CF
{
  s.prob <- paste0(s.tpx(age + rr, tt + ss), "\\text{  under  }", FAA)
  endage <- age + rr + tt + ss; startage <- age + rr
  ffe <- round(endage - floor(endage), 1); ffs <- round(startage - floor(startage), 1)
  
  if (FAA == "UDD")
  {
    num <- SULT[floor(endage), 2] * (1-ffe) + SULT[ceiling(endage), 2] * ffe
    den <- SULT[floor(startage), 2] * (1-ffs) + SULT[ceiling(startage), 2] * ffs
    tpx <- round(num/den, 5) 
    s.numans1 <- if(abs(round(endage, 0) - endage) <0.01) s.ellx(round(endage, 0)) else s.plus(paste0("(1-",ffe,")",s.ellx(floor(endage))), paste0("(",ffe,")",s.ellx(ceiling(endage))))
    s.denans1 <- if(rr == 0) s.ellx(floor(startage)) else s.plus(paste0("(1-",ffs,")",s.ellx(floor(startage))), paste0("(",ffs,")",s.ellx(ceiling(startage))))
    s.ans1 <- paste0(s.frac(s.numans1, s.denans1))
    s.ans <- paste0(s.tpx(age + rr, tt + ss),"=", s.frac(s.ellx(endage), s.ellx(startage)),"\\overset{UDD}{=}", s.ans1, "=", s.frac(num, den), "=", fbox(tpx, ansbox))
  } else
  {
    fac1 <- if(rr == 0) 1 else (SULT[ceiling(startage), 2] / SULT[floor(startage), 2])^min(endage-startage, 1-ffs)
    fac2 <- min( SULT[floor(endage), 2] / SULT[ceiling(startage), 2], 1)
    fac3 <- if((ffe == 0) || (floor(endage) < ceiling(startage))) 1 else (SULT[ceiling(endage), 2] / SULT[floor(endage), 2])^(ffe)
    tpx <- round(fac1 * fac2 * fac3, 5)
    s.ans1.fac1 <- if(rr == 0) "" else paste0("(",s.tpx(startage,round(min(endage-startage,1-ffs),1)),")")
    s.ans1.fac2 <- if(floor(endage) <= ceiling(startage)) "" else paste0("(", s.tpx(ceiling(startage), floor(endage) - ceiling(startage) ),")")
    s.ans1.fac3 <- if((ffe == 0) || (floor(endage) < ceiling(startage))) "" else paste0("(",s.tpx(floor(endage),ffe),")")
    s.ans2.fac1 <- if(rr == 0) "" else paste0("(",s.tpx(floor(startage),1),")^{",round(min(endage-startage,1-ffs),1),"}")
    s.ans2.fac2 <- if(floor(endage) <= ceiling(startage)) "" else paste0("(", s.tpx(ceiling(startage), floor(endage) - ceiling(startage) ),")")
    s.ans2.fac3 <- if((ffe == 0) || (floor(endage) < ceiling(startage))) "" else paste0("(",s.tpx(floor(endage),1),")^{",ffe,"}")
    s.ans3.fac1 <- if(rr == 0) "" else paste0("(",round(fac1, 5),")")
    s.ans3.fac2 <- if(floor(endage) <= ceiling(startage)) "" else paste0("(",round(fac2, 5),")")
    s.ans3.fac3 <- if((ffe == 0) || (floor(endage) < ceiling(startage))) "" else paste0("(",round(fac3, 5),")")
    s.ans <- paste0(s.tpx(age + rr, tt + ss),"=", s.ans1.fac1, s.ans1.fac2, s.ans1.fac3,"\\overset{CF}{=}", s.ans2.fac1, s.ans2.fac2, s.ans2.fac3, "$  \n\\hspace*{0.2in}$\\displaystyle =", s.ans3.fac1, s.ans3.fac2, s.ans3.fac3, "=", fbox(tpx, ansbox))
  }
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx))  
}

problistch3[[5]] <- function(SULT, age, tt, kk, rr, ss, ww, FAA, ansbox = TRUE) # ${_{t+s}q_{x+r}},$ ($s, t$ and/or $r$ may be 0), UDD / CF
{
  tpx <- do.call(problistch3[[4]], list(SULT = SULT, age = age, tt = tt, kk = kk, rr = rr, ss = ss, ww = ww, FAA = FAA, ansbox = FALSE))
  s.prob <- paste0(s.tqx(age + rr, tt + ss), "\\text{  under  }", FAA)
  tqx <- round(1 - tpx$num.ans, 5)
  s.ans <- paste0(s.tqx(age + rr, tt + ss), " = 1- ",s.tpx(age + rr, tt + ss), " \\overset{", FAA, "}{=} 1- ", tpx$num.ans, "=", fbox(tqx, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", tpx$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tqx))  
}

problistch3[[6]] <- function(SULT, age, tt, kk, rr, ss, ww, FAA, ansbox = TRUE) # ${_{k+s}|_{t+w}q_{x+r}},$ ($k, s, t, w$ and/or $r$ may be 0), UDD / CF
{
  s.prob <- paste0(s.ktqx(age + rr, kk + ss, tt + ww), "\\text{  under  }", FAA)
  endage <- age + rr + kk + ss + tt + ww; startage <- age + rr; midage <- age + rr + kk + ss
  ffe <- round(endage - floor(endage), 1); ffs <- round(startage - floor(startage), 1); ffm <- round(midage - floor(midage), 1)

  if (FAA == "UDD")
  {
    num <- round((SULT[floor(midage), 2] * (1-ffm) + SULT[ceiling(midage), 2] * ffm) - (SULT[floor(endage), 2] * (1-ffe) + SULT[ceiling(endage), 2] * ffe),2)
    den <- round(SULT[floor(startage), 2] * (1-ffs) + SULT[ceiling(startage), 2] * ffs,2)
    ktqx <- round(num/den, 5) 
    s.numans1a <- if(abs(round(midage, 0) - midage) <0.01) s.ellx(round(midage, 0)) else s.plus(paste0("(1-",ffm,")",s.ellx(floor(midage))), paste0("(",ffm,")",s.ellx(ceiling(midage))))
    s.numans1b <- if(abs(round(endage, 0) - endage) <0.01) s.ellx(round(endage, 0)) else s.plus(paste0("(1-",ffe,")",s.ellx(floor(endage))), paste0("(",ffe,")",s.ellx(ceiling(endage))))
    s.denans1 <- if(rr == 0) s.ellx(floor(startage)) else s.plus(paste0("(1-",ffs,")",s.ellx(floor(startage))), paste0("(",ffs,")",s.ellx(ceiling(startage))))
    s.ans1 <- paste0(s.frac(s.minus(paste0("[",s.numans1a,"]"), paste0("[",s.numans1b,"]")), s.denans1)) 
    s.ans <- paste0(s.ktqx(age + rr, kk + ss, tt + ww)," = ", s.frac(s.minus(s.ellx(midage), s.ellx(endage)), s.ellx(startage))," \\overset{UDD}{=} ", s.ans1, "$  \n\\hspace*{0.2in}$\\displaystyle =", s.frac(num, den), "=", fbox(ktqx, ansbox))
  } else
  {
    tpx <- do.call(problistch3[[4]], list(SULT=SULT, age=age, tt=kk, kk=tt, rr=rr, ss=ss, ww=ww, FAA=FAA, ansbox = FALSE))
    tqxk <- do.call(problistch3[[5]], list(SULT=SULT, age=floor(midage), tt=floor(endage-midage), kk=NULL, rr=midage-floor(midage), ss=(endage-midage)-floor(endage-midage), ww=NULL, FAA=FAA, ansbox = FALSE))
    ktqx <- round(tpx$num.ans*tqxk$num.ans, 5)
    s.ans <- paste0(s.ktqx(age + rr, kk + ss, tt + ww),"=", "\\left(", s.tpx(age+rr, kk+ss), "\\right)", "\\left(", s.tqx(age+rr+kk+ss, tt+ww), "\\right) \\overset{CF}{=} ", "(", round(tpx$num.ans, 5), ")(", round(tqxk$num.ans, 5), ") = ", fbox(ktqx, ansbox),", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", tpx$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", tqxk$s.ans)     
  }
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=ktqx))  
}
