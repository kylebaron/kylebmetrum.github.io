##' --- 
##' title: "Simulation with `mclapply` is random"
##' author: "Kyle Baron"
##' 
##' output: 
##'   html_document:
##'     theme: united
##' ---

#+ message=FALSE
knitr::opts_chunk$set(comment='.')

library(mrgsolve)
library(dplyr)
library(magrittr)
library(parallel)
mcRNG()
RNGkind()


##' 
##' # This is tedious to follow ...
##' 
##' but important to note.
##' 

##' ## Some functions to help
sim <- function(i,x,idata=data_frame(ID=1:100)) {
  x %>% idata_set(idata) %>% mrgsim(end=-1) %>% as.tbl
}
#' 
smry <- function(x) {
  x %>% summarise(nETA1 = n_distinct(ETA1), 
                  sETA1 = sum(ETA1),
                  vETA1 = var(ETA1),
                  nETA2 = n_distinct(ETA2),
                  sETA2 = sum(ETA2),
                  vETA2 = var(ETA2),
                  n=n())
}


##' # Simulate some ETAs

#+
code <- '
$OMEGA 1 1 1
$TABLE 
table(ETA1) = ETA(1);
table(ETA2) = ETA(2);
table(ETA3) = ETA(3);

'

#+ message=FALSE
mod <- mread(code=code, "mclapply", tempdir(),warn=FALSE)

##' # `mrgsolve` respects `R` RNG

set.seed(101)
out1 <- mod %>% sim(1,.)

set.seed(202)
out2 <- mod %>% sim(1,.)

#+
identical(out1,out2)

set.seed(101)
out3 <- mod %>% sim(1,.)

#+
identical(out1,out3)


##' # Reproducible results with `lapply`

set.seed(101)
out1 <- lapply(1:10, sim, mod) %>% bind_rows

set.seed(202)
out2 <- lapply(1:10, sim, mod) %>% bind_rows

#+
identical(out1,out2)

set.seed(101) 
out3 <- lapply(1:10, sim, mod) %>% bind_rows
#+
identical(out1,out3)

##' ### Every person was unique
#+
out1 %>% do(smry(.))

out2 %>% do(smry(.))

out3 %>% do(smry(.))


##' # Reproducible results with `mclapply`

options(mc.cores=4)

set.seed(101)
mc.reset.stream()
out1 <- mclapply(1:10, sim, mod) %>% bind_rows

set.seed(202)
mc.reset.stream()
out2 <- mclapply(1:10, sim, mod) %>% bind_rows

#+
identical(out1,out2)

set.seed(101) 
mc.reset.stream()
out3 <- mclapply(1:10, sim, mod) %>% bind_rows

#+
identical(out1,out3)

##' ### The result will change when you change number of cores
set.seed(101) 
mc.reset.stream()
out4 <- mclapply(1:10, mc.cores=2, sim, mod) %>% bind_rows

identical(out3,out4)

##' ### Every person was unique
#+
out1 %>% do(smry(.))

out2 %>% do(smry(.))

out3 %>% do(smry(.))

out4 %>% do(smry(.))


##' `sessionInfo()`
sessionInfo()


