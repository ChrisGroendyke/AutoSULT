
############################################################
# R Code File Containing Problem / Solution Functions      #
############################################################

# Chapter 8

#### Define calculation function types

problistch8 <- list()

problistch8[[1]] <- function(SSD, age, ansbox = TRUE) # ${_{20}p_x^{00}}$
{ 
  tpx00 <- round(SSD[age, 10] * SSD[age+10, 10] + SSD[age, 11] * SSD[age+10, 13], 5)
  s.prob <- paste0(s.tpxij(age, 20, 0, 0))
  s.ans <- paste0(s.tpxij(age, 20, 0, 0),"=", s.plus(s.times(s.tpxij(age, 10, 0, 0), s.tpxij(age+10, 10, 0, 0)), s.times(s.tpxij(age, 10, 0, 1), s.tpxij(age+10, 10, 1, 0))), "=", s.plus(s.times(SSD[age, 10], SSD[age+10, 10]), s.times(SSD[age, 11], SSD[age+10, 13])), "=", fbox(tpx00, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx00))  
}

problistch8[[2]] <- function(SSD, age, ansbox = TRUE) # ${_{20}p_x^{01}}$
{ 
  tpx01 <- round(SSD[age, 10] * SSD[age+10, 11] + SSD[age, 11] * SSD[age+10, 12], 5)
  s.prob <- paste0(s.tpxij(age, 20, 0, 1))
  s.ans <- paste0(s.tpxij(age, 20, 0, 1),"=", s.plus(s.times(s.tpxij(age, 10, 0, 0), s.tpxij(age+10, 10, 0, 1)), s.times(s.tpxij(age, 10, 0, 1), s.tpxij(age+10, 10, 1, 1))), "=", s.plus(s.times(SSD[age, 10], SSD[age+10, 11]), s.times(SSD[age, 11], SSD[age+10, 12])), "=", fbox(tpx01, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx01))  
}

problistch8[[3]] <- function(SSD, age, ansbox = TRUE) # ${_{20}p_x^{02}}$
{ 
  tpx00 <- do.call(problistch8[[1]], list(SSD = SSD, age = age, ansbox = FALSE))
  tpx01 <- do.call(problistch8[[2]], list(SSD = SSD, age = age, ansbox = FALSE))
  tpx02 <- 1 - tpx00$num.ans - tpx01$num.ans
  s.prob <- paste0(s.tpxij(age, 20, 0, 2))
  s.ans <- paste0(s.tpxij(age, 20, 0, 2),"=", s.minus(1, s.minus(s.tpxij(age, 20, 0, 0), s.tpxij(age, 20, 0, 1))), "=", s.minus(1, s.minus(tpx00$num.ans, tpx01$num.ans)), "=", fbox(tpx02, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", tpx00$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", tpx01$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx02))  
}

problistch8[[4]] <- function(SSD, age, ansbox = TRUE) # ${_{20}p_x^{10}}$
{ 
  tpx10 <- round(SSD[age, 13] * SSD[age+10, 10] + SSD[age, 12] * SSD[age+10, 13], 5)
  s.prob <- paste0(s.tpxij(age, 20, 1, 0))
  s.ans <- paste0(s.tpxij(age, 20, 1, 0),"=", s.plus(s.times(s.tpxij(age, 10, 1, 0), s.tpxij(age+10, 10, 0, 0)), s.times(s.tpxij(age, 10, 1, 1), s.tpxij(age+10, 10, 1, 0))), "=", s.plus(s.times(SSD[age, 13], SSD[age+10, 10]), s.times(SSD[age, 12], SSD[age+10, 13])), "=", fbox(tpx10, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx10))  
}

problistch8[[5]] <- function(SSD, age, ansbox = TRUE) # ${_{20}p_x^{11}}$
{ 
  tpx11 <- round(SSD[age, 13] * SSD[age+10, 11] + SSD[age, 12] * SSD[age+10, 12], 5)
  s.prob <- paste0(s.tpxij(age, 20, 1, 1))
  s.ans <- paste0(s.tpxij(age, 20, 1, 1),"=", s.plus(s.times(s.tpxij(age, 10, 1, 0), s.tpxij(age+10, 10, 0, 1)), s.times(s.tpxij(age, 10, 1, 1), s.tpxij(age+10, 10, 1, 1))), "=", s.plus(s.times(SSD[age, 13], SSD[age+10, 11]), s.times(SSD[age, 12], SSD[age+10, 12])), "=", fbox(tpx11, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx11))  
}

problistch8[[6]] <- function(SSD, age, ansbox = TRUE) # ${_{20}p_x^{12}}$
{ 
  tpx10 <- do.call(problistch8[[4]], list(SSD = SSD, age = age, ansbox = FALSE))
  tpx11 <- do.call(problistch8[[5]], list(SSD = SSD, age = age, ansbox = FALSE))
  tpx12 <- 1 - tpx10$num.ans - tpx11$num.ans
  s.prob <- paste0(s.tpxij(age, 20, 1, 2))
  s.ans <- paste0(s.tpxij(age, 20, 1, 2),"=", s.minus(1, s.minus(s.tpxij(age, 20, 1, 0), s.tpxij(age, 20, 1, 1))), "=", s.minus(1, s.minus(tpx10$num.ans, tpx11$num.ans)), "=", fbox(tpx12, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", tpx10$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", tpx11$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=tpx12))  
}

problistch8[[7]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{10}}^{00}$
{
  termann10 <- round(SSD[age, 2] - (1.05^(-10))*SSD[age, 10]*SSD[age+10, 2] - (1.05^(-10))*SSD[age, 11]*SSD[age+10, 5], 5)
  s.prob <- paste0(s.conttermannij(age, 0, 0, 10))
  s.ans.1 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 0, 0)), s.contWLannij(age+10, 0, 0)))
  s.ans.2 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 0, 1)), s.contWLannij(age+10, 1, 0)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 10]), SSD[age+10, 2]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 11]), SSD[age+10, 5]))
  s.ans <- paste0(s.conttermannij(age, 0, 0, 10),"=", s.minus(s.contWLannij(age, 0, 0), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 2], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann10, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann10))  
}

problistch8[[8]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{10}}^{01}$
{
  termann10 <- round(SSD[age, 3] - (1.05^(-10))*SSD[age, 10]*SSD[age+10, 3] - (1.05^(-10))*SSD[age, 11]*SSD[age+10, 4], 5)
  s.prob <- paste0(s.conttermannij(age, 0, 1, 10))
  s.ans.1 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 0, 0)), s.contWLannij(age+10, 0, 1)))
  s.ans.2 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 0, 1)), s.contWLannij(age+10, 1, 1)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 10]), SSD[age+10, 3]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 11]), SSD[age+10, 4]))
  s.ans <- paste0(s.conttermannij(age, 0, 1, 10),"=", s.minus(s.contWLannij(age, 0, 1), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 3], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann10, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann10))  
}

problistch8[[9]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{10}}^{10}$
{
  termann10 <- round(SSD[age, 5] - (1.05^(-10))*SSD[age, 12]*SSD[age+10, 5] - (1.05^(-10))*SSD[age, 13]*SSD[age+10, 2], 5)
  s.prob <- paste0(s.conttermannij(age, 1, 0, 10))
  s.ans.1 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 1, 1)), s.contWLannij(age+10, 1, 0)))
  s.ans.2 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 1, 0)), s.contWLannij(age+10, 0, 0)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 12]), SSD[age+10, 5]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 13]), SSD[age+10, 2]))
  s.ans <- paste0(s.conttermannij(age, 1, 0, 10),"=", s.minus(s.contWLannij(age, 1, 0), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 5], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann10, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann10))  
}

problistch8[[10]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{10}}^{11}$
{
  termann10 <- round(SSD[age, 4] - (1.05^(-10))*SSD[age, 13]*SSD[age+10, 3] - (1.05^(-10))*SSD[age, 12]*SSD[age+10, 4], 5)
  s.prob <- paste0(s.conttermannij(age, 1, 1, 10))
  s.ans.1 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 1, 0)), s.contWLannij(age+10, 0, 1)))
  s.ans.2 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 1, 1)), s.contWLannij(age+10, 1, 1)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 13]), SSD[age+10, 3]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 12]), SSD[age+10, 4]))
  s.ans <- paste0(s.conttermannij(age, 1, 1, 10),"=", s.minus(s.contWLannij(age, 1, 1), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 4], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann10, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann10))  
}

problistch8[[11]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{20}}^{00}$
{
  twentyp00 <- do.call(problistch8[[1]], list(SSD = SSD, age = age, ansbox = FALSE))
  twentyp01 <- do.call(problistch8[[2]], list(SSD = SSD, age = age, ansbox = FALSE))
  termann20 <- round(SSD[age, 2] - (1.05^(-20))*twentyp00$num.ans*SSD[age+20, 2] - (1.05^(-20))*twentyp01$num.ans*SSD[age+20, 5], 5)
  s.prob <- paste0(s.conttermannij(age, 0, 0, 20))
  s.ans.1 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 0, 0)), s.contWLannij(age+20, 0, 0)))
  s.ans.2 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 0, 1)), s.contWLannij(age+20, 1, 0)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp00$num.ans), SSD[age+20, 2]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp01$num.ans), SSD[age+20, 5]))
  s.ans <- paste0(s.conttermannij(age, 0, 0, 20),"=", s.minus(s.contWLannij(age, 0, 0), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 2], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann20, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp00$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp01$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann20))  
}

problistch8[[12]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{20}}^{01}$
{ 
  twentyp00 <- do.call(problistch8[[1]], list(SSD = SSD, age = age, ansbox = FALSE))
  twentyp01 <- do.call(problistch8[[2]], list(SSD = SSD, age = age, ansbox = FALSE))
  termann20 <- round(SSD[age, 3] - (1.05^(-20))*twentyp00$num.ans*SSD[age+20, 3] - (1.05^(-20))*twentyp01$num.ans*SSD[age+20, 4], 5)
  s.prob <- paste0(s.conttermannij(age, 0, 1, 20))
  s.ans.1 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 0, 0)), s.contWLannij(age+20, 0, 1)))
  s.ans.2 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 0, 1)), s.contWLannij(age+20, 1, 1)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp00$num.ans), SSD[age+20, 3]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp01$num.ans), SSD[age+20, 4]))
  s.ans <- paste0(s.conttermannij(age, 0, 1, 20),"=", s.minus(s.contWLannij(age, 0, 1), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 3], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann20, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp00$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp01$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann20))  
}

problistch8[[13]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{20}}^{10}$
{
  twentyp11 <- do.call(problistch8[[5]], list(SSD = SSD, age = age, ansbox = FALSE))
  twentyp10 <- do.call(problistch8[[4]], list(SSD = SSD, age = age, ansbox = FALSE))
  termann20 <- round(SSD[age, 5] - (1.05^(-20))*twentyp11$num.ans*SSD[age+20, 5] - (1.05^(-20))*twentyp10$num.ans*SSD[age+20, 2], 5)
  s.prob <- paste0(s.conttermannij(age, 1, 0, 20))
  s.ans.1 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 1, 1)), s.contWLannij(age+20, 1, 0)))
  s.ans.2 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 1, 0)), s.contWLannij(age+20, 0, 0)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp11$num.ans), SSD[age+20, 5]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp10$num.ans), SSD[age+20, 2]))
  s.ans <- paste0(s.conttermannij(age, 1, 0, 20),"=", s.minus(s.contWLannij(age, 1, 0), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 5], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann20, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp11$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp10$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann20))  
}

problistch8[[14]] <- function(SSD, age, ansbox = TRUE) # $\\ax*{x:\\angl{20}}^{11}$
{
  twentyp11 <- do.call(problistch8[[5]], list(SSD = SSD, age = age, ansbox = FALSE))
  twentyp10 <- do.call(problistch8[[4]], list(SSD = SSD, age = age, ansbox = FALSE))
  termann20 <- round(SSD[age, 4] - (1.05^(-20))*twentyp10$num.ans*SSD[age+20, 3] - (1.05^(-20))*twentyp11$num.ans*SSD[age+20, 4], 5)
  s.prob <- paste0(s.conttermannij(age, 1, 1, 20))
  s.ans.1 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 1, 0)), s.contWLannij(age+20, 0, 1)))
  s.ans.2 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 1, 1)), s.contWLannij(age+20, 1, 1)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp10$num.ans), SSD[age+20, 3]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp11$num.ans), SSD[age+20, 4]))
  s.ans <- paste0(s.conttermannij(age, 1, 1, 20),"=", s.minus(s.contWLannij(age, 1, 1), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 4], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termann20, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp11$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp10$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termann20))  
}

problistch8[[15]] <- function(SSD, age, ansbox = TRUE) # $\\Ax*{x:\\angl{10}}^{02}$
{ 
  termins10 <- round(SSD[age, 7] - (1.05^(-10))*SSD[age, 10]*SSD[age+10, 7] - (1.05^(-10))*SSD[age, 11]*SSD[age+10, 9], 5)
  s.prob <- paste0(s.conttermij(age, 0, 2, 10))
  s.ans.1 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 0, 0)), s.contWLij(age+10, 0, 2)))
  s.ans.2 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 0, 1)), s.contWLij(age+10, 1, 2)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 10]), SSD[age+10, 7]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 11]), SSD[age+10, 9]))
  s.ans <- paste0(s.conttermij(age, 0, 2, 10),"=", s.minus(s.contWLij(age, 0, 2), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 7], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termins10, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termins10))  
}

problistch8[[16]] <- function(SSD, age, ansbox = TRUE) # $\\Ax*{x:\\angl{10}}^{12}$
{ 
  termins10 <- round(SSD[age, 9] - (1.05^(-10))*SSD[age, 13]*SSD[age+10, 7] - (1.05^(-10))*SSD[age, 12]*SSD[age+10, 9], 5)
  s.prob <- paste0(s.conttermij(age, 1, 2, 10))
  s.ans.1 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 1, 0)), s.contWLij(age+10, 0, 2)))
  s.ans.2 <- paste0(s.times(s.times("v^{10}", s.tpxij(age, 10, 1, 1)), s.contWLij(age+10, 1, 2)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 13]), SSD[age+10, 7]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-10)), 5), SSD[age, 12]), SSD[age+10, 9]))
  s.ans <- paste0(s.conttermij(age, 1, 2, 10),"=", s.minus(s.contWLij(age, 1, 2), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 9], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termins10, ansbox))
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termins10))  
}

problistch8[[17]] <- function(SSD, age, ansbox = TRUE) # $\\Ax*{x:\\angl{20}}^{02}$
{ 
  twentyp00 <- do.call(problistch8[[1]], list(SSD = SSD, age = age, ansbox = FALSE))
  twentyp01 <- do.call(problistch8[[2]], list(SSD = SSD, age = age, ansbox = FALSE))
  termins20 <- round(SSD[age, 7] - (1.05^(-20))*twentyp00$num.ans*SSD[age+20, 7] - (1.05^(-20))*twentyp01$num.ans*SSD[age+20, 9], 5)
  s.prob <- paste0(s.conttermij(age, 0, 2, 20))
  s.ans.1 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 0, 0)), s.contWLij(age+20, 0, 2)))
  s.ans.2 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 0, 1)), s.contWLij(age+20, 1, 2)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp00$num.ans), SSD[age+20, 7]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp01$num.ans), SSD[age+20, 9]))
  s.ans <- paste0(s.conttermij(age, 0, 2, 20),"=", s.minus(s.contWLij(age, 0, 2), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 7], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termins20, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp00$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp01$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termins20))  
}

problistch8[[18]] <- function(SSD, age, ansbox = TRUE) # $\\Ax*{x:\\angl{20}}^{12}$
{ 
  twentyp11 <- do.call(problistch8[[5]], list(SSD = SSD, age = age, ansbox = FALSE))
  twentyp10 <- do.call(problistch8[[4]], list(SSD = SSD, age = age, ansbox = FALSE))
  termins20 <- round(SSD[age, 9] - (1.05^(-20))*twentyp10$num.ans*SSD[age+20, 7] - (1.05^(-20))*twentyp11$num.ans*SSD[age+20, 9], 5)
  s.prob <- paste0(s.conttermij(age, 1, 2, 20))
  s.ans.1 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 1, 0)), s.contWLij(age+20, 0, 2)))
  s.ans.2 <- paste0(s.times(s.times("v^{20}", s.tpxij(age, 20, 1, 1)), s.contWLij(age+20, 1, 2)))
  s.num.ans.1 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp10$num.ans), SSD[age+20, 7]))
  s.num.ans.2 <- paste0(s.times(s.times(round((1.05^(-20)), 5), twentyp11$num.ans), SSD[age+20, 9]))
  s.ans <- paste0(s.conttermij(age, 1, 2, 20),"=", s.minus(s.contWLij(age, 1, 2), s.minus(s.ans.1, s.ans.2)), "$  \n\\hspace*{0.5in}$\\displaystyle =", s.minus(SSD[age, 9], s.minus(s.num.ans.1, s.num.ans.2)), "=", fbox(termins20, ansbox), ", \\text{ where }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp11$s.ans, ", \\text{ and }$  \n\\hspace*{0.2in}$\\displaystyle", twentyp10$s.ans)
  return(list(s.prob=s.prob, s.ans=s.ans, num.ans=termins20))  
}
