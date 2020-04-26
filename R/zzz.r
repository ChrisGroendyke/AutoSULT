######################################################################
#
# zzz.r
#
#
######################################################################

.onAttach <- function(lib, pkg){
    info <- utils::packageDescription("AutoSULT")
  packageStartupMessage(
    paste('\nAutoSULT: version ', info$Version, ', created on ', info$Date, '\n', sep="")
    )
}
