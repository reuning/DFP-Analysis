
library(ggplot2)
library(rstan)

source(here::here("Style"))
setwd(here::here("WTHH_Classes"))

 
pp_all <- list()
ll_all <- list()
for(jj in 1:8){
  load(paste("Out_", jj, ".RData", sep=""))
  pp_all[[jj]] <- pp_out
  ll_all[[jj]] <- ll_out
  
  rm(mod)
  gc()
}



pp_stats <- lapply(pp_all, quantile, p=c(0.05, .5, .95))
ll_stats <- lapply(ll_all, quantile, p=c(0.05, .5, .95))

pp.df <- as.data.frame(t(matrix(unlist(pp_stats), 3, 8)))
names(pp.df) <- c("lo", "mid", "up")
pp.df$clusters <- 1:8
library(ggplot2)
png("PP_Clusters.png", height=6, width=6, units="in", res=200)
ggplot(pp.df) + 
  geom_pointrange(aes(x=clusters, 
                      ymin=lo, ymax=up, 
                      y=mid)) + 
  xlab("Number\nof Clusters") +
  ylab("Proportion Correct") + 
  # ggtitle_dfp("evaluation of heldout sample using accuracy") + 
  # theme_dfp() + 
  NULL
dev.off()

ll.df <- as.data.frame(t(matrix(unlist(ll_stats), 3, 8)))
names(ll.df) <- c("lo", "mid", "up")
ll.df$clusters <- 1:8
png("LL_Clusters.png", height=6, width=6, units="in", res=200)
ggplot(ll.df) + 
  geom_pointrange(aes(x=clusters, 
                      ymin=lo, ymax=up, 
                      y=mid)) + 
  xlab("Number\nof Clusters") +
  ylab("Log-Likelihood") + 
  # ggtitle_dfp("Evaluation on Heldout Sample using log-likelihood") + 
  # theme_dfp() + 
  NULL
dev.off()

lapply(ll_all, function(x) mean(x < ll_all[[8]]))
lapply(pp_all, function(x) mean(x < pp_all[[8]]))


lapply(ll_all, function(x) mean(x < ll_all[[6]]))
lapply(pp_all, function(x) mean(x < pp_all[[6]]))


load("Full_Out_6.RData")
library(rstan)
out <- extract(mod)
stan_rhat(mod)

phi <- out$phi

mu <- apply(phi, c(2,3,4), median)
hi <- apply(phi, c(2,3,4), quantile, 0.95)
lo <- apply(phi, c(2,3,4), quantile, 0.05)

issues <- names(policy.df)
colnames(mu) <- issues
rownames(mu) <- paste("Group", 1:6, sep="")
dimnames(mu)[[3]] <- c("Strongly Support", "Somewhat Support", "Neither", 
                       "Somewhat Oppose", "Strongly Oppose", "Not Sure")
mu.df <- reshape2::melt(mu)


colnames(hi) <- issues
rownames(hi) <- paste("Group", 1:6, sep="")
dimnames(hi)[[3]] <- c("Strongly Support", "Somewhat Support", "Neither", 
                       "Somewhat Oppose", "Strongly Oppose", "Not Sure")
hi.df <- reshape2::melt(hi)

colnames(lo) <- issues
rownames(lo) <- paste("Group", 1:6, sep="")
dimnames(lo)[[3]] <- c("Strongly Support", "Somewhat Support", "Neither", 
                       "Somewhat Oppose", "Strongly Oppose", "Not Sure")
lo.df <- reshape2::melt(lo)


mu.df <- cbind(mu.df, lo.df$value, hi.df$value)
names(mu.df) <- c("Group", "Question", "Response", "Mean", "Low", "High")
mu.df <- subset(mu.df,  Question %in% c("MARLEG", "WEALTH", "MARREP", 
                                         "M4A", "AVR", "FREECOLL", "GREENJOB", 
                                         "WELTEST", "BAIL_item", "DEPORT", 
                                         "BORDER", "PATH"))
mu.df <- droplevels(mu.df)

levels(mu.df$Question) <- c("Path to citizenship\nfor some undocumented\nimmigrants", 
                            "Increase border security\nwith a Fence", 
                            "Deport undocumented\nimmigrants", 
                            "End cash bail", 
                            "Drug test welfare\nbeneficiaries", 
                            "Green jobs for\nunemployed Americans", 
                            "Free tuition through\ntax on rich", 
                            "Tax on wealth\nover $100 million",
                            "Automatic voter\nregistration", 
                            "Medicare for all", 
                            "Marijuana reparations", 
                            "National marijuana\nlegalization")
# mu.df <- subset(mu.df, Group!="Group6")

png("Group1.png", height=8, width=6, units="in", res=200)
ggplot(subset(mu.df, Group=="Group1"), aes(color=Response)) +
  geom_pointrange(aes(x=Question, y=Mean, ymin=Low, ymax=High), 
                  position=position_dodge(width=.5)) +
  coord_flip() +
  # ggtitle_dfp("Progressives") + 
  # theme_dfp(panel.grid.major.y=element_line(linetype="dashed", size=.05)) +
  scale_color_dfp()  +xlab("") + 
  scale_y_continuous("Probability of Response", limits = c(0, 1)) +
  NULL

dev.off()

png("Group2.png", height=8, width=6, units="in", res=200)
ggplot(subset(mu.df, Group=="Group2"), aes(color=Response)) +
  geom_pointrange(aes(x=Question, y=Mean, ymin=Low, ymax=High), 
                  position=position_dodge(width=.5)) +
  coord_flip() +
  # ggtitle_dfp("Moderate Democrats") + 
  # theme_dfp(panel.grid.major.y=element_line(linetype="dashed", size=.05)) +
  scale_color_dfp()  +xlab("") + 
  scale_y_continuous("Probability of Response", limits = c(0, 1)) +
  NULL
dev.off()

png("Group3.png", height=8, width=6, units="in", res=200)
ggplot(subset(mu.df, Group=="Group3"), aes(color=Response)) +
  geom_pointrange(aes(x=Question, y=Mean, ymin=Low, ymax=High), 
                  position=position_dodge(width=.5)) +
  coord_flip() +
  # ggtitle_dfp("Conservatives") + 
  # theme_dfp(panel.grid.major.y=element_line(linetype="dashed", size=.05)) +
  scale_color_dfp()  +xlab("") + 
  scale_y_continuous("Probability of Response", limits = c(0, 1)) +
  NULL
dev.off()

png("Group4.png", height=8, width=6, units="in", res=200)
ggplot(subset(mu.df, Group=="Group4"), aes(color=Response)) +
  geom_pointrange(aes(x=Question, y=Mean, ymin=Low, ymax=High), 
                  position=position_dodge(width=.5)) +
  coord_flip() +
  # ggtitle_dfp("Low Information") + 
  # theme_dfp(panel.grid.major.y=element_line(linetype="dashed", size=.05)) +
  scale_color_dfp()  +xlab("") + 
  scale_y_continuous("Probability of Response", limits = c(0, 1)) +
  NULL
dev.off()


png("Group5.png", height=8, width=6, units="in", res=200)
ggplot(subset(mu.df, Group=="Group5"), aes(color=Response)) + 
  geom_pointrange(aes(x=Question, y=Mean, ymin=Low, ymax=High), 
                  position=position_dodge(width=.5)) +
  coord_flip() +
  # ggtitle_dfp("Reactionary") + 
  # theme_dfp(panel.grid.major.y=element_line(linetype="dashed", size=.05)) +
  scale_color_dfp()  +xlab("") + 
  scale_y_continuous("Probability of Response", limits = c(0, 1)) +
  NULL

dev.off()

png("Group6.png", height=8, width=6, units="in", res=200)
ggplot(subset(mu.df, Group=="Group6"), aes(color=Response)) +
  geom_pointrange(aes(x=Question, y=Mean, ymin=Low, ymax=High), 
                  position=position_dodge(width=.5)) +
  coord_flip() +
  # ggtitle_dfp("No Opinion") + 
  # theme_dfp(panel.grid.major.y=element_line(linetype="dashed", size=.05)) +
  scale_color_dfp()  +xlab("") + 
  scale_y_continuous("Probability of Response", limits = c(0, 1)) +
  NULL
dev.off()


groups <- factor(c("Progressives", "Moderate Democrats", "Conservatives", 
            "Low Information", "Reactionary", "No Opinion"), 
            levels=c("Progressives", "Moderate Democrats", "Conservatives", 
                    "Reactionary", "Low Information", "No Opinion"))

apply(out$theta, 2, mean)

pred_class <- function(x){
  exp(x - logSumExp(x))
}

classes <- array(NA, dim(out$gamma))
for(ii in 1:nrow(out$gamma)){
  for(jj in 1:ncol(out$gamma)){
    classes[ii,jj,] <- pred_class(out$gamma[ii,jj,])
  }
}

# max <- apply(classes, c(1,2), max)
class <- apply(classes, c(1,2), which.max)
class <- apply(class, 2, function(x) names(table(x))[which.max(table(x))])

class_second <- apply(classes, c(1,2),function(x) which(order(x)==5))
class_second <- apply(class_second, 2, function(x) names(table(x))[which.max(table(x))])


med <- numeric(ncol(classes))
lo <- numeric(ncol(classes))
hi <- numeric(ncol(classes))

med.2 <- numeric(ncol(classes))
lo.2 <- numeric(ncol(classes))
hi.2 <- numeric(ncol(classes))

for(ii in 1:ncol(classes)){
  tmp <- classes[,ii,as.numeric(class[ii])]
  med[ii] <- median(tmp)
  lo[ii] <- quantile(tmp, 0.05)
  hi[ii] <- quantile(tmp, 0.95)
  
  tmp <- classes[,ii,as.numeric(class_second[ii])]
  med.2[ii] <- median(tmp)
  lo.2[ii] <- quantile(tmp, 0.05)
  hi.2[ii] <- quantile(tmp, 0.95)
}

tmp <- data.frame(med, lo, hi, med.2, lo.2, hi.2)
tmp$x <- c(reorder(tmp$med, tmp$med))
tmp$class <- factor(c("Progressives", "Moderate Democrats", "Conservatives", 
                       "Low Information", "Reactionary", "No Opinion")[as.numeric(class)], 
                     levels=c("Progressives", "Moderate Democrats", "Conservatives", 
                              "Reactionary", "Low Information", "No Opinion"))
              
tmp$class.2 <- factor(c("Progressives", "Moderate Democrats", "Conservatives", 
                        "Low Information", "Reactionary", "No Opinion")[as.numeric(class_second)], 
                      levels=c("Progressives", "Moderate Democrats", "Conservatives", 
                               "Reactionary", "Low Information", "No Opinion"))

png("Cluster_Probs.png", height=8, width=8, units="in", res=200)
ggplot(tmp, aes(y=med, ymin=lo, ymax=hi, x=x, color=class)) + 
  geom_pointrange(size=.1, fatten=.25) +
  geom_pointrange(aes(ymin=lo.2, ymax=hi.2, x=x, y=med.2, color=class.2), size=.1, fatten=.25)  + 
  facet_wrap(~class) + 
  # theme_dfp(axis.text.x=element_blank(), 
  #                                axis.ticks.x=element_blank(), 
  #                                strip.background=element_blank()) +
  # scale_color_dfp("Cluster") + 
  thin_y() + 
  xlab("Survey Participant") + ylab("Probability of Class") +
  geom_hline(aes(yintercept=.5), linetype="dashed", size=.25) +
  geom_hline(aes(yintercept=.75), linetype="dashed", size=.1) +
  geom_hline(aes(yintercept=.25), linetype="dashed", size=.1) + 
  NULL
dev.off()



class <- apply(classes, c(1,2), which.max)


tmp <- apply(class, 1, function(x) by(data=df$weight_DFP, x, sum))
tmp <- tmp/sum(df$weight_DFP)

plot.df <- as.data.frame(t(apply(tmp, 1, quantile, c(0.025, 0.5, 0.975))))
names(plot.df) <- c("lo", "mid", "high")
plot.df$group <- groups

png("Cluster_Electorate.png", height=8, width=8, units="in", res=200)
ggplot(plot.df) + 
  geom_pointrange(aes(x=group, y=mid, 
                      ymin=lo, ymax=high,
                      col=group)) + 
  # theme_dfp(axis.text.y=element_blank(), 
  #           axis.ticks.y=element_blank()) + 
  coord_flip() + xlab("") + ylab("Proportion in Electorate") +
  scale_color_dfp(guide=F) + thin_x() + 
  geom_text(aes(x=group, y=mid, label=group, family="mstreg"), size=3, nudge_x=.2) + 
  geom_hline(aes(yintercept=.1), linetype="dashed", size=.1) + 
  geom_hline(aes(yintercept=.2), linetype="dashed", size=.1) +
  geom_hline(yintercept=seq(0.05, 0.25, by=0.05), linetype="dashed", size=.05) +
  # ggtitle_dfp("Progressives are a larger proportion\nof the electorate than moderate democrats") +
  scale_x_discrete("", limits=rev(levels(groups))) + 
  NULL
dev.off()
