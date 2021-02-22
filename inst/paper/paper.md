---
title: 'AutoSULT: An R Package for Generating Life Contingencies Problems'
authors:
  - name: Chris Groendyke
  - orcid: 0000-0001-9635-8397
  - affiliation: 1
affiliations:
  - name: Department of Mathematics, Robert Morris University
  - index: 1
date: "21 February 2020"
output:
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
nocite: |
  @dickson2020actuarial
tags:
- R
- life contingencies
- SULT
bibliography: paper.bib
---

# Statement of Need

The actuarial profession has developed a complex system of notation for many of its fundamental concepts[^1], including survival probabilities and expected present values of life insurance and annuity products.  While recent developments in software, such as the \LaTeX\ packages $\tt{actuarialsymbol}$ [@actsymb] and $\tt{actuarialangle}$ [@actangle], have made the typesetting of this notation somewhat less cumbersome, producing a high volume of material that utilizes this notation (e.g., a large set of problems and solutions for a life contingencies course) can still be quite tedious and unwieldy.

[^1]: For a comprehensive review of this notation, see @Bowers.

A key component of many introductory-level life contingencies classes involves computing various quantities from the values given in some standard mortality table (or life table).  These tables typically have some fixed set of calculated values, and rely on the user to be able to calculate any other required values using a set of basic relationships between the quantities.  Because these relationships are fundamental to learning life contingencies, it is important that students in these courses have a firm understanding of them, which is most typically gained through the completion of a large number of problems, ideally with detailed solutions against which the students can check their work.

Given the two considerations above, there is a clear need for an open source software package that can automatically generate an arbitrary number of basic life contingencies problems (including step-by-step solutions), typeset in an organized, nicely formatted manner.  The R [@Rbase] package $\tt{AutoSULT}$ fills this niche with a series of functions that use R Markdown [@rmarkdown; @knitr] to render PDF files with randomly generated problems (and accompanying solutions) of types selected by the user.  The target audience for this software is instructors of university courses covering life contingencies concepts.  Instructors could generate (perhaps individualized) homework assignments, quizzes, or problem sets for use in recitation or tutorial sessions, without having to invest the time to learn all of the corresponding \LaTeX\ code for life contingencies symbols, or to manually typeset each solution.  It is not intended to be a tool for exam preparation for the actuarial professional credentialing exams --- there are many high-quality commercial products filling that role; rather, the goal is to provide an open source solution to generate and typeset problems and solutions covering _basic_ types of life table calculations, as a time-saving tool for life contingencies instructors.

# The Standard Ultimate Life Table

The mortality table (life table) used in this software is the the \textbf{Standard Ultimate Survival Model (SUSM)} as defined in _Actuarial Mathematics for Life Contingent Risks, $3^{rd}$ edition_, by Dickson, Hardy, and Waters (hereafter _AMLCR3e_), a textbook that is very commonly used in undergraduate and introductory graduate life contingencies courses.  This mortality table and its underlying survival model are used in many of the examples and exercises in this textbook.  The same life table is also currently (as of Fall 2020) used as one of the standard tables in the Society of Actuaries (SOA) Long Term Actuarial Mathematics (LTAM) exam, and is referred to there as the \textbf{Standard Ultimate Life Table (SULT)}.

The survival model underlying the mortality table is based on Makeham's law, and an annual effective interest rate of $i=5\%$ is used for all of the insurance and annuity expected present values (EPVs) pre-calculated in the table.  The joint life portion of the table assumes future independent lifetimes. Further details about this survival model can be found on pg. 92 of _AMLCR3e_, and the full tables can be found in Appendix D of this text.  A PDF file containing the SULT (single life and joint life) can be found at the Society of Actuaries website.[^2]

[^2]: https://www.soa.org/globalassets/assets/Files/Edu/2018/ltam-standard-ultimate-life-table.pdf

# Usage

The $\tt{AutoSULT}$ package for R is available for download on GitHub at https://github.com/ChrisGroendyke/AutoSULT and can be installed using the following function $\tt{install\_github()}$ function, which requires[^3] the $\tt{devtools}$ (@devtools) package:

```
devtools::install_github("ChrisGroendyke/AutoSULT", build_vignettes = TRUE)
```

There are four main user-level functions in the package, corresponding to the four chapters in _AMLCR3e_ that the problems are associated with.  These functions are named $\tt{ChXQA()}$, where $\tt{X}$ is 3, 4, 5, or 9.  The problems implemented include 6 types of problems from Ch. 3 of _AMLCR3e_, 16 types of problems from Ch. 4, 13 types of problems from Ch. 5, and 10 types of problems from Ch. 9. Each problem consists of the calculation of one specific probability or EPV, using entries from the life table.  The ages and (if applicable) product term lengths and deferral periods are randomly generated and are different for each problem, so that a very large number of distinct problems can be created.  

[^3]: If the package functions do not appear to be working, the most likely cause is that R cannot find your \LaTeX\ installation; this can usually be fixed with the $\tt{TinyTeX}$ [@tinytex] package.

The output from the $\tt{ChXQA()}$ functions is customized by using the various input parameters, which specify how many of which types of problems should be generated, whether the problem order is randomized, whether a summary table of problems generated is included, etc.  A full demonstration of these functions and their parameters is given in the package vignette and function documentation.  To demonstrate the functionality, using the following code, we could produce a set of questions and solutions containing 3 problems of each of the 10 Ch. 9 problem types, in randomized order:

```
Ch9QA(probspertype = 3, randomize = TRUE)
```

This produces two PDF documents, excerpts of which are shown below:

\begin{center}
\frame{\includegraphics[trim= 0.3in 5.5in 0.6in 0.6in, clip]{qfilech9.pdf}}

\vspace{0.2 in}

\frame{\includegraphics[trim= 0.3in 5.5in 0.6in 0.6in, clip]{afilech9.pdf}}
\end{center}

# Acknowledgements

Many thanks to the following people, who have helped with the implementation and testing of this package: Adam Combs, Brian Hartman, Diana Skrzydlo, and the students in my ASCI 4100 courses at Robert Morris University.

# References
