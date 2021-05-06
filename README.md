# AutoSULT
AutoSULT R Package to Generate Problems and Solutions from the SULT

The target audience for this R package is instructors of university courses covering life contingencies concepts (and perhaps students relatively early in the process of learning life contingencies).  It is not intended to be a tool for exam preparation for the SOA Exam LTAM (or other actuarial exams); rather, the goal is to easily generate problems and solutions covering basic, rudimentary types of actuarial life table calculations.

This package will automatically generate problems of certain specified types, based on the "Standard Ultimate Life Table"(SULT) or "Standard Ultimate Survival Model" (SUSM) currently used by the SOA Exam LTAM, and Actuarial Mathematics for Life Contingent Risks textbook, 3rd edition (AMLCR3e), respectively. The problems considered include 6 types of problems from Ch. 3 of AMLCR3e, 16 types of problems from Ch. 4 of AMLCR3e, 13 types of problems from Ch. 5 of AMLCR3e, and 10 types of problems from Ch. 10 of AMLCR3e.  This package makes use of the actuarialsymbol package in LaTeX.

To install this package from GitHub, you can use the following code (you will need to first install the devtools package if you don't already have it):

devtools::install_github("ChrisGroendyke/AutoSULT", build_vignettes = TRUE)

If the functions in the package aren't working, there's a good chance that R can't find your installation of LaTeX.  This can sometimes be fixed with the tinytex package:

install.packages("tinytex")

tinytex::install_tinytex()

The AutoSULT package has a vignette that explains everything you need to know in order to generate problem and solution files.

This is a work in progress -- more types of problems are expected to be added.  Feedback and ideas for future improvements would be welcomed.
