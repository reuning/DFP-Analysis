
library(rstan)
setwd(here::here("WTHH_Classes"))


K <- 6 #### Set number of classes 
n.cores <- 4

df <- read.csv("http://filesforprogress.org/datasets/wthh/DFP_WTHH_release.csv")

favor.names <- grep("favor", names(df), value=T)


policy.df <- df[,c("PATH", "BORDER", 
                   "DEPORT", "BAIL_item", 
                   "WELTEST", "PUBLICINT", 
                   "GREENJOB", "POLFEE", 
                   "PUBLICGEN", "FREECOLL", 
                   "WEALTH", "AVR", "M4A", 
                   "MARREP", "MARAM", "MARLEG", 
                   "YEMEN", "SOLITARY", "GUNS")]



M <- nrow(policy.df)
Q <- ncol(policy.df)

resp <- unlist(policy.df)
que <- c(sapply(1:Q, rep, times=M))
pers <- c(replicate(Q, 1:M))

pers <- pers[!is.na(resp)]
que <- que[!is.na(resp)]
resp <- resp[!is.na(resp)]


N <- length(pers)

set.seed(1)
tmp <- unlist(sapply(table(que), function(x) rep_len(1:10, length.out=x)[sample.int(x)]))
names(tmp) <- NULL


pers.full <- pers
que.full <- que
resp.full <- resp 





alpha <- as.array(rep(1/K, K))
beta <- rep(1/6, 6)

if(K != 1){
  mod.init <- stan(file="Class_Full.stan", cores = 1, chains=1,
                   control = list("adapt_delta"=.99))
  
  out.init <- extract(mod.init, pars=c("theta", "phi"))
  rm(mod.init)
  
  init <- function(chain_id){
    list(theta = as.array(out.init$theta[sample.int(1000, 1),]), 
         phi = as.array(out.init$phi[sample.int(1000, 1),,,]))
    
  }

}



ii <- 1
pers_test <- pers.full[tmp==ii]
que_test <- que.full[tmp==ii]
resp_test <- resp.full[tmp==ii]

pers <- pers.full[tmp!=ii]
que <- que.full[tmp!=ii]
resp <- resp.full[tmp!=ii]

N <- length(pers)
N_test <- length(pers_test)

if(K==1){
  mod <- stan(file="Class.stan", cores = n.cores, chains=n.cores,
              control = list("adapt_delta"=.99))
} else {
  mod <- stan(file="Class.stan", cores = n.cores, chains=n.cores,init=init,
              control = list("adapt_delta"=.99))
}

pp_out <- unlist(extract(mod, "pp_test"), recursive = T, use.names = F)
ll_out <- unlist(extract(mod, "ll_test"), recursive = T, use.names = F)
save(pp_out, ll_out, file=paste("Out_", jj, ".RData", sep=""))
q()





