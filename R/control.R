
Ch3QA <- function(probspertype = 5, probspec = NULL, randomize = FALSE, probsummary = FALSE, qfile = "qfilech3", afile = "afilech3", keeprmd = FALSE)
{
  qfilein <- paste0(qfile,".Rmd"); afilein <- paste0(afile,".Rmd")
  options(scipen = 999)

  # Read in and format SULT table
  #SULT <- read.csv("SingleLife.csv", header = FALSE, col.names = c("x", "lx", "qx", "ax", "Ax", "A2x", "ax10", "Ax10", "ax20", "Ax20", "Ex5", "Ex10", "Ex20"))
  #SULT[, 4] <- round(SULT[, 4], digits = 4)
  #pad <- array(dim = c(19, dim(SULT)[2]), dimnames = list(NULL, colnames(SULT)))
  #SULT <- rbind(pad, SULT) # Pad rows to get row equal to age for convenience 
  
  # Shouldn't need to source/library these once they're in a package
  #source("strings.R"); source("calcs.R"); source("calcfuncsch3.R")
  #library(rmarkdown); library(knitr); library(kableExtra)
  
  # Headers for Q and A files
  cat("\\section*{Chapter 3 SULT Problems}  \n  $~$  \n", file = qfilein)
  cat("\\subsection*{Notes}", file = qfilein, append = TRUE)
  cat("\\begin{itemize}", file = qfilein, append = TRUE)
  cat("\\item $k, t, x$ are integers", file = qfilein, append = TRUE)
  cat("\\item $r, s, w$ are fractional values", file = qfilein, append = TRUE)
  cat("\\end{itemize} \n  $~$  \n", file = qfilein, append = TRUE)
  cat("\\subsection*{Problems} \n  $~$  \n", file = qfilein, append = TRUE)
  
  cat("\\section*{Solutions to Chapter 3 SULT Problems}  \n  $~$  \n", file = afilein)
  
  typecount <- 6
  if (!is.null(probspec))
  {
    if (length(probspec) != typecount) stop(paste("Error in probspec parameter.  Must be a vector of length ", typecount))
  } else
  {
    probspertype <- round(probspertype)
    probspec <- rep(probspertype, times = typecount)
  }
  probs <- sum(probspec)
  
  age <- as.integer(runif(probs, 20, 80))
  tt <- as.integer(runif(probs, 1, 97 - age))
  kk <- as.integer(runif(probs, 1, 99 - age - tt))
  rr <- sample(1:9, probs, replace = TRUE) / 10
  ss <- sample(1:9, probs, replace = TRUE) / 10
  ww <- sample(1:9, probs, replace = TRUE) / 10
  FAA <- ifelse(runif(probs) < 0.5, "UDD", "CF")
  
  probtypes <- rep(1:typecount, times = probspec)
  if (randomize) probtypes <- sample(probtypes)
  
  for (i in 1:probs)
  {
    probtype <- probtypes[i]
    if (probtype >= 4)
    {
      if (runif(1) < 0.3) tt[i] <- 0 else # Occasionally set t to 0 for practice
        if (runif(1) < 0.3) rr[i] <- 0 # Occasionally set r to 0 for practice
        if (runif(1) < 0.3) kk[i] <- 0 # Occasionally set k to 0 for practice
    }
    allargs <- list(SULT, age[i], tt[i], kk[i], rr[i], ss[i], ww[i], FAA[i])
    results <- do.call(problistch3[[probtype]], allargs)
    cat(paste0(i,".  $", results$s.prob,"$  \n  $~$  \n"), file = qfilein, append = TRUE)
    cat(paste0(i,".  $\\displaystyle ", results$s.ans, "$  \n  $~$  \n  $~$  \n"), file = afilein, append = TRUE)
  }
  
  if (probsummary) # Insert summary table if requested
  {
    cat("\\newpage \\subsection*{Summary} \n  $~$  \n", file = qfilein, append = TRUE)

    probdescs <- c("${_tp_x}$", "${_tq_x}$", "${_k|_tq_x}$", "${_{t+s}p_{x+r}}$ under UDD or CF", "${_{t+s}q_{x+r}}$ under UDD or CF", "${_{k+s}|_{t+w}q_{x+r}}$ under UDD or CF")
    probdat <- array(c(1:typecount, probdescs, probspec), dim = c(typecount, 3))
    colnames(probdat) <- c("Problem Type", "Problem Description", "Number Included")
    probdat <- rbind(probdat, c("Total", "", probs))
    
    summtab <- kable(probdat, format = "latex", align = "ccc", booktabs = FALSE, linesep = "", escape = FALSE) %>%
      kable_styling(position = "left", latex_options = c("hold_position")) %>%
        row_spec(typecount, hline_after = TRUE) 
    cat(summtab, file = qfilein, append = TRUE)
    cat(paste0("\n  $~$  \n Question order is ", ifelse(randomize, "", "not"), " randomized."), file = qfilein, append = TRUE)
  }
  
  render(qfilein, pdf_document(extra_dependencies = "actuarialsymbol"))
  render(afilein, pdf_document(extra_dependencies = "actuarialsymbol"))
  if (!keeprmd) {invisible(file.remove(qfilein)); invisible(file.remove(afilein))} #cleanup
}

Ch4QA <- function(probspertype = 5, probspec = NULL, randomize = FALSE, probsummary = FALSE, qfile = "qfilech4", afile = "afilech4", keeprmd = FALSE)
{
  qfilein <- paste0(qfile,".Rmd"); afilein <- paste0(afile,".Rmd")
  options(scipen = 999)
  
  # Read in and format SULT table
  #SULT <- read.csv("SingleLife.csv", header = FALSE, col.names = c("x", "lx", "qx", "ax", "Ax", "A2x", "ax10", "Ax10", "ax20", "Ax20", "Ex5", "Ex10", "Ex20"))
  #SULT[, 4] <- round(SULT[, 4], digits = 4)
  #pad <- array(dim = c(19, dim(SULT)[2]), dimnames = list(NULL, colnames(SULT)))
  #SULT <- rbind(pad, SULT) # Pad rows to get row equal to age for convenience 
  
  # Shouldn't need to source/library these once they're in a package
  #source("strings.R"); source("calcs.R"); source("calcfuncsch4.R")
  #library(rmarkdown); library(knitr); library(kableExtra)
  
  # Headers for Q and A files
  cat("\\section*{Chapter 4 SULT Problems}  \n  $~$  \n", file = qfilein)
  cat("\\subsection*{Notes}", file = qfilein, append = TRUE)
  cat("\\begin{itemize}", file = qfilein, append = TRUE)
  cat("\\item $n, x, \\text{ and } u$ are integers", file = qfilein, append = TRUE)
  cat("\\item $i = 5\\%$ unless stated oherwise", file = qfilein, append = TRUE)
  cat("\\item Use UDD where a fractional age assumption is required", file = qfilein, append = TRUE)
  cat("\\end{itemize} \n  $~$  \n", file = qfilein, append = TRUE)
  cat("\\subsection*{Problems} \n  $~$  \n", file = qfilein, append = TRUE)
  
  cat("\\section*{Solutions to Chapter 4 SULT Problems}  \n  $~$  \n", file = afilein)
  
  typecount <- 16
  if (!is.null(probspec))
  {
    if (length(probspec) != typecount) stop(paste("Error in probspec parameter.  Must be a vector of length ", typecount))
  } else
  {
    probspertype <- round(probspertype)
    probspec <- rep(probspertype, times = typecount)
  }
  probs <- sum(probspec)
  
  age <- as.integer(runif(probs, 20, 80))
  nn <- as.integer(runif(probs, 1, 97 - age))
  uu <- as.integer(runif(probs, 1, 99 - age - nn))
  mm <- sample(c(2, 4, 12), probs, replace = TRUE)
  
  probtypes <- rep(1:typecount, times = probspec)
  if (randomize) probtypes <- sample(probtypes)
  
  for (i in 1:probs)
  {
    probtype <- probtypes[i]
    allargs <- list(SULT, age[i], nn[i], uu[i], mm[i])
    results <- do.call(problistch4[[probtype]], allargs)
    cat(paste0(i,".  $", results$s.prob,"$  \n  $~$  \n"), file = qfilein, append = TRUE)
    cat(paste0(i,".  $\\displaystyle ", results$s.ans, "$  \n  $~$  \n  $~$  \n"), file = afilein, append = TRUE)
  }
  
  if (probsummary) # Insert summary table if requested
  {
    cat("\\newpage \\subsection*{Summary} \n  $~$  \n", file = qfilein, append = TRUE)
    
    probdescs <- c("$\\Ex[n]{x}$", "$\\Ax{\\termxn}$", "$\\Ax*{\\termxn}$", "$\\Ax{\\termxn}[\\hspace{0.05in}(m)]$", "$\\Ax{\\itop{x}:\\angl{2}}$ or $\\Ax{\\itop{x}:\\angl{3}}$ for $i \\neq 5\\%$", 
                    "$\\Ax*{\\itop{x}:\\angl{2}}$ or $\\Ax*{\\itop{x}:\\angl{3}}$ for $i \\neq 5\\%$", "$\\Ax{\\itop{x}:\\angl{2}}[\\hspace{0.05in}(m)]$ or $\\Ax{\\itop{x}:\\angl{3}}[\\hspace{0.05in}(m)]$ for $i \\neq 5\\%$",
                    "$\\Ax{\\endowxn}$", "$\\Ax*{\\endowxn}$", "$\\Ax{\\endowxn}[(m)]$", "$\\Ax[u|]{x}$", "$\\Ax*[u|]{x}$", "$\\Ax[u|]{x}[(m)]$", "$\\Ax[u|]{\\termxn}$",
                    "$\\Ax*[u|]{\\termxn}$", "$\\Ax[u|]{\\termxn}[\\hspace{0.05in}(m)]$")

    probdat <- array(c(1:typecount, probdescs, probspec), dim = c(typecount, 3))
    colnames(probdat) <- c("Problem Type", "Problem Description", "Number Included")
    probdat <- rbind(probdat, c("Total", "", probs))
    
    summtab <- kable(probdat, format = "latex", align = "ccc", booktabs = FALSE, linesep = "", escape = FALSE) %>%
      kable_styling(position = "left", latex_options = c("hold_position")) %>%
      row_spec(typecount, hline_after = TRUE) 
    cat(summtab, file = qfilein, append = TRUE)
    cat(paste0("\n  $~$  \n Question order is ", ifelse(randomize, "", "not"), " randomized."), file = qfilein, append = TRUE)
  }
  
  render(qfilein, pdf_document(extra_dependencies = "actuarialsymbol"))
  render(afilein, pdf_document(extra_dependencies = "actuarialsymbol"))
  if (!keeprmd) {invisible(file.remove(qfilein)); invisible(file.remove(afilein))} #cleanup
  
}

Ch5QA <- function(probspertype = 5, probspec = NULL, randomize = FALSE, probsummary = FALSE, qfile = "qfilech5", afile = "afilech5", keeprmd = FALSE)
{
  qfilein <- paste0(qfile,".Rmd"); afilein <- paste0(afile,".Rmd")
  options(scipen = 999)
  
  # Read in and format SULT table
  #SULT <- read.csv("SingleLife.csv", header = FALSE, col.names = c("x", "lx", "qx", "ax", "Ax", "A2x", "ax10", "Ax10", "ax20", "Ax20", "Ex5", "Ex10", "Ex20"))
  #SULT[, 4] <- round(SULT[, 4], digits = 4)
  #pad <- array(dim = c(19, dim(SULT)[2]), dimnames = list(NULL, colnames(SULT)))
  #SULT <- rbind(pad, SULT) # Pad rows to get row equal to age for convenience 
  
  # Shouldn't need to source/library these once they're in a package
  #source("strings.R"); source("calcs.R"); source("calcfuncsch5.R")
  #library(rmarkdown); library(knitr); library(kableExtra)
  
  # Headers for Q and A files
  cat("\\section*{Chapter 5 SULT Problems}  \n  $~$  \n", file = qfilein)
  cat("\\subsection*{Notes}", file = qfilein, append = TRUE)
  cat("\\begin{itemize}", file = qfilein, append = TRUE)
  cat("\\item $n, x, \\text{ and } u$ are integers", file = qfilein, append = TRUE)
  cat("\\item $i = 5\\%$", file = qfilein, append = TRUE)
  cat("\\item W2 refers to the 2-term Woolhouse formula", file = qfilein, append = TRUE)
  cat("\\end{itemize} \n  $~$  \n", file = qfilein, append = TRUE)
  cat("\\subsection*{Problems} \n  $~$  \n", file = qfilein, append = TRUE)
  
  cat("\\section*{Solutions to Chapter 5 SULT Problems}  \n  $~$  \n", file = afilein)

  typecount <- 13
  if (!is.null(probspec))
  {
    if (length(probspec) != typecount) stop(paste("Error in probspec parameter.  Must be a vector of length ", typecount))
  } else
  {
    probspertype <- round(probspertype)
    probspec <- rep(probspertype, times = typecount)
  }
  probs <- sum(probspec)
  age <- as.integer(runif(probs, 20, 80))
  nn <- as.integer(runif(probs, 1, 97 - age))
  uu <- as.integer(runif(probs, 1, 99 - age - nn))
  mm <- sample(c(2, 4, 12), probs, replace = TRUE)
  FAA <- ifelse(runif(probs) < 0.5, "UDD", "W2")
  
  probtypes <- rep(1:typecount, times = probspec)
  if (randomize) probtypes <- sample(probtypes)
  
  for (i in 1:probs)
  {
    probtype <- probtypes[i]
    allargs <- list(SULT = SULT, age = age[i], nn = nn[i], uu = uu[i], mm = mm[i], FAA = FAA[i])
    results <- do.call(problistch5[[probtype]], allargs)
    cat(paste0(i,".  $", results$s.prob,"$  \n  $~$  \n"), file = qfilein, append = TRUE)
    cat(paste0(i,".  $\\displaystyle ", results$s.ans, "$  \n  $~$  \n  $~$  \n"), file = afilein, append = TRUE)
  }
  
  if (probsummary) # Insert summary table if requested
  {
    cat("\\newpage \\subsection*{Summary} \n  $~$  \n", file = qfilein, append = TRUE)
    
    probdescs <- c("$\\ax**{\\endowxn}$", "$\\ax**{\\joint\\endowxn}$", "$\\ax*{x}$ under UDD or W2", "$\\ax**{x}[(m)]$ under UDD or W2", "$\\ax*{\\endowxn}$ under UDD or W2", 
                   "$\\ax**{\\endowxn}[(m)]$ under UDD or W2", "$\\ax**[u|]{x}$", "$\\ax**[u|]{\\endowxn}$", "$\\ax**[u|]{\\joint\\endowxn}$", "$\\ax*[u|]{x}$ under UDD or W2",
                   "$\\ax**[u|]{x}[(m)]$ under UDD or W2", "$\\ax*[u|]{\\endowxn}$ under UDD or W2", "$\\ax**[u|]{\\endowxn}[(m)]$ under UDD or W2")
    
    probdat <- array(c(1:typecount, probdescs, probspec), dim = c(typecount, 3))
    colnames(probdat) <- c("Problem Type", "Problem Description", "Number Included")
    probdat <- rbind(probdat, c("Total", "", probs))
    
    summtab <- kable(probdat, format = "latex", align = "ccc", booktabs = FALSE, linesep = "", escape = FALSE) %>%
      kable_styling(position = "left", latex_options = c("hold_position")) %>%
      row_spec(typecount, hline_after = TRUE) 
    cat(summtab, file = qfilein, append = TRUE)
    cat(paste0("\n  $~$  \n Question order is ", ifelse(randomize, "", "not"), " randomized."), file = qfilein, append = TRUE)
  }
  
  render(qfilein, pdf_document(extra_dependencies = "actuarialsymbol"))
  render(afilein, pdf_document(extra_dependencies = "actuarialsymbol"))
  if (!keeprmd) {invisible(file.remove(qfilein)); invisible(file.remove(afilein))} #cleanup
  
}
