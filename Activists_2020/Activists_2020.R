rm(list=ls())

source(here::here("Style"))
setwd(here::here("Activists_2020"))


library(gridExtra)
library(png)
library(survey)
library(ggplot2)

# img <- readPNG("dfp-line-logo-black.png")
# logo <- rasterGrob(img, interpolate=TRUE)

df <- read.csv("http://filesforprogress.org/datasets/pre_post_debate/FIRSTDEBATE_DATA.csv")

polactions <- grep("polactions", names(df), value=T)[1:6]
ppact <- df[,polactions]


df$activist <- rowSums(ppact)
df$activist[df$activist>4] <- 4
df$activist <- factor(df$activist, labels=c("0", "1", "2", 
                                            "3","4+"))
df$topchoice <- factor(df$DEMPRIM20_horserace, 
                       labels=c("Michael Bennet", "Joe Biden", "Cory Booker", 
                                "Steve Bullock", "Pete Buttigieg", 
                                "Julian Castro", "Bill DeBlasio", "John Delaney", 
                                "Tulsi Gabbard", "Kirsten Gillibrand", "Mike Gravel", 
                                "Kamala Harris", "John Hickenlooper", "Jay Inslee", 
                                "Amy Klobuchar", "Wayne Messam", "Seth Moulton", 
                                "Beto O'Rourke", "Tim Ryan", "Bernie Sanders", 
                                "Eric Swalwell", "Elizabeth Warren", 
                                "Marianne Williamson", "Andrew Yang", "Joe Sestak", 
                                "None"))


cons <- grep("CONSIDERING",names(df), value=T)[1:26]

for(ii in 1:length(cons)){
  df[,cons[ii]] <- factor(df[,cons[ii]], levels=c(0, 1), labels=c("Drop", "Considering"))
}


for(ii in 1:length(polactions)){
  df[,polactions[ii]] <- factor(df[,polactions[ii]], levels=c(0, 1), labels=c("Drop", "Did"))
}


df$weight_comb <- df$weight_postdebate
df$weight_comb[is.na(df$weight_comb)] <- df$weight_predebate[is.na(df$weight_comb)]
df$survey_flag <- factor(df$survey_flag, levels=c(1,3), labels=c("Pre", "Post"))
df$debate <- factor(df$DEMPRIM20_debate_post, labels=c("Watched both", "Watched part", 
                                                       "Read about them", "Did not follow", 
                                                       "Not sure"))

sv_df <- svydesign(~1, weights = ~weight_comb, data=df)

act_name <-  c("Volunteered", "Protested", 
               "Called Elected\nOfficial", "Attended Townhall", 
               "Posted on\nSocial Media", "Donated")

act_stat <- list()
for(ii in 1:length(polactions)){
  tmp <- svyby(~get(polactions[ii]), by=~survey_flag, FUN = svymean, design=sv_df)
  tmp <- tmp[,c(1,3,5)]  
  colnames(tmp) <- c("Flag", act_name[ii], "SE")
  act_stat[[ii]] <- tmp
}

act_stat <- reshape2::melt(act_stat, id=c("Flag"), varnames=c("Considering") )
tmp <- act_stat$value[act_stat$variable=="SE"]
act_stat <- act_stat[act_stat$variable!="SE",]
act_stat$SE <- tmp
p <- ggplot(act_stat, aes(x=reorder(variable, value), y=100*value, ymin=100*(value-1.96*SE), 
                          ymax=100*(value+1.96*SE), 
                          fill=Flag, color=Flag, group=Flag)) + 
  geom_col(alpha=.5, color='white', position = position_dodge(width=.9)) + 
  geom_linerange(position = position_dodge(width=.9)) +
  geom_text(aes(label=paste(round(100*value), "%", sep=""), y=100*(value-1.96*SE)), color="black",
            position = position_dodge(.9), vjust=1) + 
  # theme_dfp() + 
  scale_fill_manual("Pre/Post\nDebate", 
                                  values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  scale_color_manual("Pre/Post\nDebate", 
                     values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  labs(y="Percentage", x="") + 
  # ggtitle_dfp("Which of the following activities have you\nparticipated in over the last 6 months?") +
  NULL

# p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
#                   widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("activ_det.png", p, dpi=320, height = 6, width=8, units="in")

tmp <- svyby(~activist, by=~survey_flag, design=sv_df, FUN=svymean)

tmp <- reshape2::melt(tmp)
tmp$value <- 100*tmp$value

tmp <- cbind(tmp[1:10,], tmp[11:20,3])
levels(tmp$variable) <- c("0", "1", "2", "3", "4+", NA, NA, NA, NA, NA)
names(tmp)[3:4] <- c("mean", "SE")

p <- ggplot(tmp, aes(x=(variable), y=mean, ymin=(mean-1.96*SE), 
                     ymax=(mean+1.96*SE), 
                     fill=survey_flag, color=survey_flag, group=survey_flag)) + 
  geom_col(alpha=.5, color='white', position = position_dodge(width=.9)) + 
  geom_linerange(position = position_dodge(width=.9)) +
  geom_text(aes(label=paste(round(mean), "%", sep=""), y=(mean-1.96*SE)), color="black",
            position = position_dodge(.9), vjust=1) + 
  # theme_dfp() + 
  scale_fill_manual("Pre/Post\nDebate", 
                                  values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  scale_color_manual("Pre/Post\nDebate", 
                     values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  labs(y="Percentage", x="Number of Political Activities") + 
  # ggtitle_dfp("How many political activities have you\nparticipated in over the last 6 months?") +
  NULL

# p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
#                   widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("activ_num.png", p, dpi=320, height = 6, width=8, units="in")


cons_stat <- list()
for(ii in 1:length(cons)){
  tmp <- svyby(~get(cons[ii]), by=~activist + survey_flag, FUN = svymean, design=sv_df)
  tmp <- tmp[,c(1,2,4,6)]  
  colnames(tmp) <- c("Level", "Flag", "Considering", "SE")
  cons_stat[[ii]] <- tmp
}
names(cons_stat) <-  
  c("Michael Bennet", "Joe Biden", "Cory Booker", 
    "Steve Bullock", "Pete Buttigieg", 
    "Julian Castro", "Bill DeBlasio", "John Delaney", 
    "Tulsi Gabbard", "Kirsten Gillibrand", "Mike Gravel", 
    "Kamala Harris", "John Hickenlooper", "Jay Inslee", 
    "Amy Klobuchar", "Wayne Messam", "Seth Moulton", 
    "Beto O'Rourke", "Tim Ryan", "Bernie Sanders", "Joe Sestak",
    "Eric Swalwell", "Elizabeth Warren", 
    "Marianne Williamson", "Andrew Yang", 
    "None")

cons_stat <- reshape2::melt(cons_stat, id=c("Level", "Flag"), varnames=c("Considering") )
cons_stat$value <- cons_stat$value*100
tmp <- cons_stat$value[cons_stat$variable=="SE"]
cons_stat <- cons_stat[cons_stat$variable!="SE",]
cons_stat$SE <- tmp
cons_stat <- cons_stat[cons_stat$L1 %in% names(which(by(cons_stat$value, cons_stat$L1, mean) > 10)),]

(cons_stat$L1) <- factor(cons_stat$L1)
p <- ggplot(cons_stat) + 
  geom_pointrange(aes(x=Level, y=value, ymin=value-1.96*SE, 
                      ymax=value+1.96*SE, 
                      color=Flag, group=Flag), 
                  position = position_dodge(.5)) +
  facet_wrap(~L1, scales = 'free_x') + 
  # theme_dfp() + 
  theme(plot.caption = element_text(hjust=0), 
        panel.grid.major.y =element_line(color='gray80', linetype = 'dashed') ) + 
  scale_fill_manual("Pre/Post\nDebate", 
                    values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  scale_color_manual("Pre/Post\nDebate", 
                     values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  labs(y="Percentage Considering", x="Number of Activities", 
       caption='Only candidates with an average of 10% considering across all groups shown.') + 
  # ggtitle_dfp("Percentage Considering Candidate by Political Involvement") + 
  NULL

p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))

ggsave("considering.png", p, dpi=320, height = 8, width=8, units="in")

tmp <- svyby(~topchoice, by=~activist+survey_flag, design=sv_df, FUN=svymean, na.rm=T)
# tmp <- svytable(~topchoice+activist+survey_flag, design=sv_df)
# tmp <- prop.table(tmp, c(2,3))

tmp <- reshape2::melt(tmp)
tmp$value <- 100*tmp$value



tmp <- cbind(tmp[1:260,], tmp[261:520,4])

tmp <- tmp[tmp$variable %in% names(which(by(tmp$value, tmp$variable, mean) > 1)), ]


levels(tmp$variable) <- gsub("topchoice", "", levels(tmp$variable))
tmp <- droplevels(tmp)
names(tmp)[4:5] <- c("mean", "SE")

tmp$variable <- factor(tmp$variable, levels=unique(sort(as.character(tmp$variable))))

p <- ggplot(tmp, aes(x=(activist), y=mean, ymin=ifelse((mean-1.96*SE)<0, 0, mean-1.96*SE), 
                     ymax=(mean+1.96*SE), 
                     fill=survey_flag, color=survey_flag, group=survey_flag)) + 
  # geom_col(position=position_dodge(.9), alpha=.5, color='white') + 
  geom_pointrange(position = position_dodge(width=.5)) +
  facet_wrap(~variable,scales = 'free_x') +
  # theme_dfp() + 
  theme(plot.caption = element_text(hjust=0),
                      panel.grid.major.y =element_line(color='gray80', linetype = 'dashed') ) + 
  scale_fill_manual("Pre/Post\nDebate",
                    values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  scale_color_manual("Pre/Post\nDebate", 
                     values=c("Pre"='#124073', "Post"="#BC4A11")) + 
  labs(y="Percentage Top Choice", x="Number of Political Activities", 
       caption='Only candidates with an average of 1% support across all groups shown.') + 
  # ggtitle_dfp("Horse Race Support by Political Involvement") +
  NULL

# p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
#                   widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("horserace.png", p, dpi=320, height = 8, width=8, units="in")



tmp <- svyby(~debate,by=~activist, FUN=svymean, design=sv_df, na.rm=T)
tmp <- tmp[,1:6]
tmp <- reshape2::melt(tmp)
tmp$value <- tmp$value*100
levels(tmp$variable) <- gsub("debate", "", levels(tmp$variable))

p <- ggplot(tmp, aes(x=(activist), y=value, 
                     fill=variable, group=variable)) + 
  geom_col() +
  # theme_dfp() +
  geom_text(aes(label=paste(round(value), "%", sep="")), position = position_stack(vjust = 0.5)) + 
  scale_fill_manual("Reported Debate\nAttention", values=c("#0078E5","#79A5C6","#702388","#D688E2", "#b3b3b3")) + 
  labs(y="Percentage", x="Number of Political Activities") + 
  # ggtitle_dfp("Activists Were most likely to have watched the debates") + 
  NULL

# p <- grid.arrange(p, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
#                   widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("debate.png", p, dpi=320, height = 6, width=8, units="in")

