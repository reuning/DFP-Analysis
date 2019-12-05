library(ggplot2)
library(gridExtra)
library(png)


source(here::here("Style/DFP_Style.R"))
img <- readPNG(here::here("Style/dfp-line-logo-black.png"))
logo <- grid::rasterGrob(img, interpolate=TRUE)

setwd(here::here("M4A_Supp"))


full_df <- read.csv("dfp_m4a.csv")

library(rstanarm)

set.seed(1)

mod1 <- stan_lm(twoway18 ~ twoway16 + twoway16:open_seat  + IE_diff + 
                  fund_diff + 
                  total_funding + 
                  rscandal_ai + dscandal_ai +
                  r_incumbent_running + 
                  d_incumbent_running,
                data=full_df, 
                prior=R2(.9), cores=2)

mod2 <- stan_lm(twoway18 ~ twoway16 + twoway16:open_seat  +  IE_diff + 
                  fund_diff + 
                  total_funding + 
                  rscandal_ai + dscandal_ai + 
                  nnu*r_incumbent_running +
                  nnu*d_incumbent_running ,
                data=full_df, 
                prior=R2(.9), cores=2)


mod3 <- stan_lm(twoway18 ~twoway16   +  IE_diff + 
                  fund_diff + 
                  total_funding + 
                  rscandal_ai + dscandal_ai + 
                  nnu*twoway16*r_incumbent_running +
                  twoway16*nnu*d_incumbent_running ,
                data=full_df,  
                prior=R2(.9),
                cores=2)


mod4 <- stan_lm(twoway18~twoway16 + twoway16:open_seat + IE_diff + 
                 fund_diff + 
                 total_funding + 
                 rscandal_ai + dscandal_ai +
                  r_incumbent_running +
                  d_incumbent_running +
                  nnu*m4a_total_tone_weight*r_incumbent_running +
                  nnu*m4a_total_tone_weight*d_incumbent_running
                  , data=full_df,  
                prior=R2(.9),
               cores=2)



loo_1 <- loo(mod1, cores=2)
loo_2 <- loo(mod2, k_threshold = .7, cores=2)
loo_3 <- loo(mod3, k_threshold = .7, cores=2)
loo_compare(loo_1, loo_2, loo_3)
loos <- as.data.frame(rbind(loo_1$estimates[3,],
      loo_2$estimates[3,],
      loo_3$estimates[3,]))
loos$Type <- c("Base", "With M4A", "M4A and\n2016 Pres Share")

p <- ggplot(loos) + 
  geom_pointrange(aes(x=reorder(Type, Estimate), y=Estimate, 
                      ymin=Estimate+1.96*SE, 
                      ymax=Estimate-1.96*SE)) + 
  theme_minimal() + coord_flip() + theme_dfp() + 
  geom_hline(aes(yintercept=min(Estimate)), linetype='dashed') + 
  theme(plot.caption = element_text(hjust=0, size=6)) + 
  labs(x='', y='LOO Information Criterion \n(lower is better)') + 
  ggtitle_dfp('Including M4A Endorsement does not significantly\nimprove predictive accuraccy') 


p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("m4a_comp.png", p, dpi=320, height = 6, width=8, units="in")


loo_compare(loo_1,
            loo_2,
            loo_3)


out <- as.matrix(mod2)
## Open seat
open_seat <- out[,'nnu'] 

## Rep Incumbent
rep_inc <- out[,'nnu']  + out[,"nnu:r_incumbent_runningYes"]

## Dem Incumbent
dem_inc <- out[,'nnu']  + out[,"nnu:d_incumbent_runningYes"]


full <-as.data.frame(rbind(quantile(open_seat, c(0.025, 0.5, 0.975)), 
                           quantile(rep_inc, c(0.025, 0.5, 0.975)), 
                           quantile(dem_inc, c(0.025, 0.5, 0.975))))

names(full) <- c("Low", "Med", "High")
full$type <- c("Open", "Rep Incumbent", "Dem Incumbent")

p <- ggplot(full) + 
  geom_pointrange(aes(x=type, y=Med, ymin=Low, ymax=High)) + 
  theme_minimal() + coord_flip() + theme_dfp() + 
  theme(plot.caption = element_text(hjust=0, size=6)) + 
  geom_hline(yintercept = 0, linetype='dashed') + 
  labs(x='', y='Effect of M4A Support', 
       caption=stringr::str_wrap('Controls for 2016 Vote Share; Diff in Donations; Total Donations; Rep Scandal; Dem Scandal', 75)) + 
  ggtitle_dfp('Effect of Endorsing M4A On Dem Candidate Support') 


p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("m4a_dir.png", p, dpi=320, height = 6, width=8, units="in")





range(mod3$model$twoway16[mod3$model$d_incumbent_running=='No' & mod3$model$r_incumbent_running=='No'])
range(mod3$model$twoway16[mod3$model$d_incumbent_running=='Yes'])
range(mod3$model$twoway16[mod3$model$r_incumbent_running=='Yes'])

out <- as.matrix(mod3)

## Open seat
open_seat <- t(out[,rep('nnu', 20)]) + 
  as.matrix(seq(24, 86, length.out = 20)) %*% out[,'twoway16:nnu']

## Rep Incumbent
rep_inc <- t(out[,rep('nnu',20)])  + 
  t(out[,rep("nnu:r_incumbent_runningYes", 20)]) + 
  as.matrix(seq(18, 58, length.out = 20)) %*% out[,'twoway16:nnu'] +
  as.matrix(seq(18, 58, length.out = 20)) %*% out[,'twoway16:nnu:r_incumbent_runningYes'] 



## Dem Incumbent
dem_inc <- t(out[,rep('nnu', 20)])  + 
  t(out[,rep("nnu:d_incumbent_runningYes",20)]) + 
  as.matrix(seq(33, 95, length.out = 20)) %*% out[,'twoway16:nnu'] + 
  as.matrix(seq(33, 95, length.out = 20)) %*% out[,'twoway16:nnu:d_incumbent_runningYes'] 


dem_inc <- data.frame(t(apply(dem_inc, 1, quantile, c(0.025, 0.5, 0.975))))
dem_inc$pres <- seq(33, 95, length.out = 20)
rep_inc <- data.frame(t(apply(rep_inc, 1, quantile, c(0.025, 0.5, 0.975))))
rep_inc$pres <- seq(18, 58, length.out = 20)

open_seat <- data.frame(t(apply(open_seat, 1, quantile, c(0.025, 0.5, 0.975))))
open_seat$pres <- seq(24, 86, length.out = 20)

dem_inc$type <- 'Dem Incumbent'
rep_inc$type <- 'Rep Incumbent'
open_seat$type <- 'Open Seat'
full <- rbind(dem_inc, rep_inc, open_seat)
names(full) <- c("low", "med", "high", "pres", 'type')

rug_df <- mod3$model
rug_df <- rug_df[,c("twoway16", "d_incumbent_running", "r_incumbent_running", "nnu")]
rug_df$type <- "Open Seat"
rug_df$type[rug_df$d_incumbent_running=='Yes'] <- 'Dem Incumbent'
rug_df$type[rug_df$r_incumbent_running=='Yes'] <- 'Rep Incumbent'
rug_df$nnu <- factor(rug_df$nnu, levels=c(0,1), labels=c("No", "Yes"))
names(rug_df)[1] <- 'pres'

full$type <- ordered(full$type, levels=c("Rep Incumbent", "Open Seat", "Dem Incumbent"))
rug_df$type <- ordered(rug_df$type, levels=c("Rep Incumbent", "Open Seat", "Dem Incumbent"))

p <- ggplot(full, aes(x=pres, y=med, ymin=low, ymax=high)) + 
  geom_ribbon(fill='gray', alpha=.5) + 
  geom_line() + geom_hline(yintercept=0, lty=2) + 
  theme_dfp() + theme(plot.caption = element_text(hjust=0, size=6), legend.position = 'bottom') + 
  facet_wrap(~type, scale='free_x') +
  scale_color_manual("Dem Endorsed M4A:", values=c("#0078E5", "#BF7A00"), 
                     guide=guide_legend(nrow=1)) + 
  geom_rug(aes(x=pres, color=nnu), data=rug_df, 
           sides='b',inherit.aes = F, alpha=.9) + 
  labs(y='Increase in 2-Way Vote Share', x='Clinton Vote Share',
       caption=stringr::str_wrap('Controls for 2016 Vote Share; Diff in Donations; Total Donations; Rep Scandal; Dem Scandal', 75)) + 
  ggtitle_dfp(stringr::str_wrap('Effect of M4A Support Conditioned on Race Type and 2016 Pres Vote', 40))

p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))


ggsave("m4a_vote.png", p, dpi=320, height = 6, width=8, units="in")








range(mod4$model$m4a_total_tone_weight[mod4$model$d_incumbent_running=='No' & mod4$model$r_incumbent_running=='No'])
range(mod4$model$m4a_total_tone_weight[mod4$model$r_incumbent_running=='Yes'])

range(mod4$model$m4a_total_tone_weight[mod4$model$d_incumbent_running=='Yes'])

out <- as.matrix(mod4)

## Open seat
open_seat <- t(out[,rep('nnu', 20)]) + 
  as.matrix(seq(-1.51, 2.23, length.out = 20)) %*% out[,'nnu:m4a_total_tone_weight']

## Rep Incumbent
rep_inc <- t(out[,rep('nnu',20)])  + 
  t(out[,rep("r_incumbent_runningYes:nnu", 20)]) + 
  as.matrix(seq(-1.58, 2.12, length.out = 20)) %*% out[,'nnu:m4a_total_tone_weight'] +
  as.matrix(seq(-1.58, 2.12, length.out = 20)) %*% out[,'r_incumbent_runningYes:nnu:m4a_total_tone_weight'] 



## Dem Incumbent
dem_inc <- t(out[,rep('nnu', 20)])  + 
  t(out[,rep("d_incumbent_runningYes:nnu",20)]) + 
  as.matrix(seq(-1.18, 2.36, length.out = 20)) %*% out[,'nnu:m4a_total_tone_weight'] + 
  as.matrix(seq(-1.18, 2.36, length.out = 20)) %*% out[,'d_incumbent_runningYes:nnu:m4a_total_tone_weight'] 


dem_inc <- data.frame(t(apply(dem_inc, 1, quantile, c(0.025, 0.5, 0.975))))
dem_inc$media <- seq(-1.18, 2.36, length.out = 20)
rep_inc <- data.frame(t(apply(rep_inc, 1, quantile, c(0.025, 0.5, 0.975))))
rep_inc$media <- seq(-1.58, 2.12, length.out = 20)

open_seat <- data.frame(t(apply(open_seat, 1, quantile, c(0.025, 0.5, 0.975))))
open_seat$media <- seq(-1.51, 2.23, length.out = 20)

dem_inc$type <- 'Dem Incumbent'
rep_inc$type <- 'Rep Incumbent'
open_seat$type <- 'Open Seat'
full <- rbind(dem_inc, rep_inc, open_seat)
names(full) <- c("low", "med", "high", "media", 'type')

rug_df <- mod4$model
rug_df <- rug_df[,c("m4a_total_tone_weight", "d_incumbent_running", "r_incumbent_running", "nnu")]
rug_df$type <- "Open Seat"
rug_df$type[rug_df$d_incumbent_running=='Yes'] <- 'Dem Incumbent'
rug_df$type[rug_df$r_incumbent_running=='Yes'] <- 'Rep Incumbent'
rug_df$nnu <- factor(rug_df$nnu, levels=c(0,1), labels=c("No", "Yes"))

names(rug_df)[1] <- 'media'

full$type <- ordered(full$type, levels=c("Rep Incumbent", "Open Seat", "Dem Incumbent"))
rug_df$type <- ordered(rug_df$type, levels=c("Rep Incumbent", "Open Seat", "Dem Incumbent"))

p <- ggplot(full, aes(x=media, y=med, ymin=low, ymax=high)) + 
  geom_ribbon(fill='gray', alpha=.5) + 
  geom_line() + geom_hline(yintercept=0, lty=2) + 
  theme_dfp() +    theme(plot.caption = element_text(hjust=0, size=6), 
                         legend.position = 'bottom') + 
  facet_wrap(~type, scale='free_x') +
  geom_rug(aes(x=media, color=nnu), data=rug_df, sides='b',inherit.aes = F, 
           alpha=.9) + 
  scale_color_manual("Dem Endorsed M4A:", values=c("#0078E5", "#BF7A00"), 
                     guide=guide_legend(nrow=1)) + 
  labs(y='Increase in 2-Way Vote Share', x='M4A Coverage by Tone\nSentiment * Log(Article Count)',
       caption=stringr::str_wrap('Controls for 2016 Vote Share; Diff in Donations; Total Donations; Rep Scandal; Dem Scandal. Sample only includes districts with at least one M4A article.', 75)) + 
  ggtitle_dfp(stringr::str_wrap('Effect of M4A Support Conditioned on Race Type and Media Coverage', 40))

p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))


ggsave("m4a_media.png", p, dpi=320, height = 6, width=8, units="in")





mod_cands <- stan_lm(d_ideo~ nnu   +
                       dchallengerwoman_ai + 
                       dchallengerpersonofcolor_ai +
                       dchallengermiliarybackground_ai,
               data=full_df,
               prior=R2(.5), cores=2, subset=d_incumbent_running=='No')
summary(mod_cands)
bayesplot::color_scheme_set(c('darkgray'))

p <- plot(mod_cands, plotfun= 'intervals',
     regex_pars = 'dchal|nnu', prob_outer=.95, point_est='mean', prob=0) + 
  theme_dfp() + 
  scale_y_discrete(labels=c("M4A Support", "Woman", "Person of Color", 
                            "Military Background"))+ 
  ggtitle_dfp(stringr::str_wrap("Coefficients from Model of Congressional Candidate Percieved Ideology (Only Challengers)", 50))

p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))



ggsave("ideol_percep_chal.png", p, dpi=320, height = 6, width=8, units="in")

