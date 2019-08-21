library(ggplot2)
library(survey)
library(gridExtra)
library(png)

source(here::here("Style/DFP_Style.R"))
setwd(here::here("Foreign_Policy"))


img <- readPNG(here::here("Style/dfp-line-logo-black.png"))
logo <- rasterGrob(img, interpolate=TRUE)

df <- read.csv("http://filesforprogress.org/datasets/april_omni/dfp_april2019_return.csv")

df$USoverall_budget <- factor(df$USoverall_budget, levels=1:6, 
                               labels=c("Strongly agree",
                                        "Somewhat agree",
                                        "Neither agree nor disagree",
                                        "Somewhat disagree",
                                        "Strongly disagree",
                                        "Not sure"))

df$pid3 <- factor(df$pid3, levels=1:3, labels=c("Democrat", "Republican", "Independent"))



  
df$age5 <- factor(df$age5, levels=1:5, labels=c("18-29", "30-39", "40-49", "50-64", "65+"))

df <- df[!is.na(df$weight),]
sv_df <- svydesign(~1, weight=~weight, data=df)       

plot_df <- as.data.frame(svymean(~USoverall_budget, design=sv_df, na.rm=T))
labels <- c("Strongly\nagree",
            "Somewhat\nagree",
            "Neither agree\nnor disagree",
            "Somewhat\ndisagree",
            "Strongly\ndisagree",
            "Not sure")
plot_df$var <- ordered(labels, levels=c("Strongly\nagree",
                                        "Somewhat\nagree",
                                        "Neither agree\nnor disagree",
                                        "Somewhat\ndisagree",
                                        "Strongly\ndisagree",
                                        "Not sure"))

plot_df$mean <- 100*plot_df$mean
plot_df$SE <- 100*plot_df$SE

colors <- c("#0A2645","#79A5C6",
"#b3b3b3","#F75757",
"#B71D1A", "#A8BF14")
p <-ggplot(plot_df, aes(x=var, y=mean, 
                    ymin=mean-1.96*SE, 
                    ymax=mean+1.96*SE, color=var, fill=var)) +
  geom_col(alpha=.5, color='white') + geom_pointrange() + 
  theme_dfp() + scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) + guides(color=FALSE, fill=FALSE) +
  labs(y="Percentage", x="") + ggtitle_dfp("Agree/Disagree: 'The US should shift some\nof its budget from the military to diplomacy'")



p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("foreign_budget.png", p, dpi=320, height = 6, width=8, units="in")




plot_df <- as.data.frame(svyby(~USoverall_budget, by=~pid3, FUN=svymean, design=sv_df, na.rm=T))
colnames(plot_df)[-1] <- c(labels, paste("SE", labels))
plot_df <- reshape2::melt(plot_df)
plot_df$value <- 100*plot_df$value
plot_df <- cbind(plot_df[1:18,], plot_df[19:36,3])
names(plot_df) <- c("Party", "var", "mean", "SE")

p <-ggplot(plot_df, aes(x=var, y=mean, 
                        ymin=mean-1.96*SE, 
                        ymax=mean+1.96*SE, color=var, fill=var)) +
  geom_col(alpha=.5, color='white') + geom_pointrange() + 
  theme_dfp() + scale_color_manual(values=colors) + 
  facet_wrap(~Party, ncol=1) + 
  scale_fill_manual(values=colors) + guides(color=FALSE, fill=FALSE) +
  labs(y="Percentage", x="") + ggtitle_dfp("Agree/Disagree: 'The US should shift some\nof its budget from the military to diplomacy'\nPartisan Breakdown")

p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("foreign_budget_pid.png", p, dpi=320, height = 8, width=8, units="in")







plot_df <- as.data.frame(svyby(~USoverall_budget, by=~age5, FUN=svymean, design=sv_df, na.rm=T))
colnames(plot_df)[-1] <- c(labels, paste("SE", labels))
plot_df <- reshape2::melt(plot_df)
plot_df$value <- 100*plot_df$value
plot_df <- cbind(plot_df[1:30,], plot_df[31:60,3])
names(plot_df) <- c("age", "var", "mean", "SE")

p <-ggplot(plot_df, aes(x=var, y=mean, 
                        ymin=mean-1.96*SE, 
                        ymax=mean+1.96*SE, color=var, fill=var)) +
  geom_col(alpha=.5, color='white') + geom_pointrange() + 
  theme_dfp() + scale_color_manual(values=colors) + 
  facet_wrap(~age, ncol=1) + 
  scale_fill_manual(values=colors) + guides(color=FALSE, fill=FALSE) +
  labs(y="Percentage", x="") + ggtitle_dfp("Agree/Disagree: 'The US should shift some\nof its budget from the military to diplomacy'\nAge Breakdown")

p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("foreign_budget_age.png", p, dpi=320, height = 8, width=8, units="in")


