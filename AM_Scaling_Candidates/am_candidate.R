library(ggplot2)
library(rstan)

source(here::here("Style"))
setwd(here::here("AM_Scaling"))

load("CCES2018_OUTPUT.RData")
df <- table
ids <- df[,grep("CC18_334", names(df), value=T)]

ids <- as.matrix(ids)
ids <- ids[,-1]
ids[ids==8] <- NA

df <- df[apply(ids, 1, function(x) !all(is.na(x))),]
ids <- ids[apply(ids, 1, function(x) !all(is.na(x))),]

sens <- unique(c(df$SenCand1Name, df$SenCand2Name, 
                 df$SenCand1Name2, df$SenCand2Name2, 
                 df$CurrentSen1Name, df$CurrentSen2Name))

hous <- unique(c(df$HouseCand1Name, df$HouseCand2Name, 
                 df$CurrentHouseName))

gov <- unique(c(df$CurrentGovName))

all_names <- unique(c(sens, hous, gov))
all_names <- all_names[all_names!=""]


rid <- cbind(df$CurrentGovName,
             "Donald Trump",
             "Democratic Party",
             "Republican Party",
             "Supreme Court",
             df$CurrentSen1Name,
             df$CurrentSen2Name,
             df$SenCand1Name,
             df$SenCand2Name,
             df$SenCand1Name2,
             df$SenCand2Name2,
             df$HouseCand1Name,
             df$HouseCand2Name,
             df$CurrentHouseName)
rid <- c(t(rid))
vid <- c(sapply(df$caseid, rep, times=ncol(ids)))
Y <- c(t(ids))

all_names <- c("Democratic Party", 
               "Republican Party", 
               "Donald Trump",
               "Supreme Court", all_names)

cands <- c("Steve Bullock", "Tulsi Gabbard", 
           "John Hickenlooper", "Michael Bennet", 
           "Amy Klobuchar", "Seth Moulton", 
           "Jay Inslee", "Tim Ryan", "Beto O'Rourke", 
           "Kirsten Gillibrand", "Cory Booker", 
           "Elizabeth Warren", "Kamala Harris", 
           "Eric Swalwell", "Bernie Sanders", 
           "John Delaney")


rid <- rid[!is.na(Y)]
vid <- vid[!is.na(Y)]
Y <- Y[!is.na(Y)]

sel <- as.character(vid) %in% names(which(table(vid)!=1))
rid <- rid[sel]
vid <- vid[sel]
Y <- Y[sel]

# sel <- as.character(vid) %in% unique(vid[rid %in% cands])
# rid <- rid[sel]
# vid <- vid[sel]
# Y <- Y[sel]



sub_names <-c("Democratic Party",
              "Republican Party",
              "Donald Trump",
              "Supreme Court", "Steve Bullock", "Tulsi Gabbard",
              "John Hickenlooper", "Michael Bennet",
              "Amy Klobuchar", "Seth Moulton",
              "Jay Inslee", "Tim Ryan", "Beto O'Rourke",
              "Kirsten Gillibrand", "Cory Booker",
              "Elizabeth Warren", "Kamala Harris",
              "Eric Swalwell", "Bernie Sanders",
              "John Delaney")
# 
# rid[!rid %in% sub_names] <- NA
# 
# 
# rid <- as.numeric(factor(rid, levels=sub_names))
# 
# vid <- vid[!is.na(rid)]
# Y <- Y[!is.na(rid)]
# rid <- rid[!is.na(rid)]

all_names <- all_names[all_names %in% rid]


rid <- as.numeric(factor(rid, levels=all_names))

Y_self <- df$CC18_334A[match(unique(vid), df$caseid)]
vid_self <- df$caseid[match(unique(vid), df$caseid)]

Y_self[Y_self == 8] <- NA
vid_self <- vid_self[!is.na(Y_self)]
Y_self <- Y_self[!is.na(Y_self)]

all_vid <- unique(vid)
vid_save <- vid_self

vid_self <- match(vid_self, all_vid)

vid <- as.numeric(factor(vid, levels=all_vid))
J <- max(rid)

N <- max(vid)
r_N <- length(Y)
s_N <- length(vid_self)

library(rstan)
# mod <- stan(file="A_M.stan", cores=2, chains=2)
# tmp <- stan_plot(mod, "theta")

model <- stan_model("A_M.stan")
mod <- vb(model)

cands_id <- match(sub_names, all_names)
tmp <- stan_plot(mod, paste("theta[", cands_id, "]", sep=""))


tmp <- tmp$data
tmp$params <- sub_names
library(ggplot2)
png("AM_Scaling.png", height=6, width=6, units='in', 
    res=200)
ggplot(tmp, aes(x=reorder(params, m), y=m, ymin=ll, ymax=hh)) +
  geom_pointrange() + coord_flip() +
  # theme_dfp() +
  # ggtitle_dfp("Aldrich-Mckelvey Scaling of Candidates") +
  labs(y="Ideology", x="",
       caption = 'Aldrich-Mckelvey Scaling using CCES 2018 Candidate Placements\nTrump, Democratic Party, Republican Party and Supreme Court used to bridge.')
dev.off()



out <- extract(mod)

cand_est <- apply(out$theta, 2, mean)

key <- cbind(df$CurrentGovParty,
             "Donald Trump",
             "Democratic Party",
             "Republican Party",
             "Supreme Court",
             df$CurrentSen1Party,
             df$CurrentSen2Party,
             df$SenCand1Party,
             df$SenCand2Party,
             df$SenCand1Party2,
             df$SenCand2Party2,
             df$HouseCand1Party,
             df$HouseCand2Party,
             df$CurrentHouseParty)

key <- c(t(key))
key_names <- cbind(df$CurrentGovName,
                   "Donald Trump",
                   "Democratic Party",
                   "Republican Party",
                   "Supreme Court",
                   df$CurrentSen1Name,
                   df$CurrentSen2Name,
                   df$SenCand1Name,
                   df$SenCand2Name,
                   df$SenCand1Name2,
                   df$SenCand2Name2,
                   df$HouseCand1Name,
                   df$HouseCand2Name,
                   df$CurrentHouseName)
key_names <- c(t(key_names))
names(key) <- key_names


party <- key[all_names]

plot_df <- data.frame("estimate"=cand_est, "Party"=party)
plot_df <- plot_df[plot_df$Party %in% c("Democratic", "Republican"),]
tmp <- tmp[!tmp$params %in% c("Democratic Party", "Republican Party", 
                              "Supreme Court"),]
library(ggrepel)
png("ideo_comp_candidates.png", height=8, width=10, units="in", res=200)
ggplot(NULL) +
  geom_density(aes(x=estimate, group=Party, fill=Party, color=Party),
               data=plot_df, alpha=.35, inherit.aes = F, position = "identity") +
  geom_text_repel(aes(x=m, y=0, label=params), data=tmp, inherit.aes = F) + 
  geom_point(aes(x=m, y=0), data=tmp, inherit.aes = F) +
  scale_y_continuous(limits = c(-0.1, 0.8)) + 
  # theme_dfp() + 
  scale_fill_manual(values=c("Democratic"="steelblue3",
                             "Republican"="orangered3")) + 
  scale_color_manual(values=c("Democratic"="steelblue2",
                              "Republican"="orangered2")) +
  # ggtitle_dfp("Perceived Ideology of 2020 Presidential Candidates Compared to\n2018 Congressional and Gubernatorial Candidates/Incumbents") +
  labs(x="Percieved Ideology\n(A-M Scaled)", y="Density", 
       caption="Density plot shows the ideology of incumbent Senators, Representatives,\nand Governors and Senate and Representative Candidates in 2018")
dev.off()






cand_est <- apply(out$theta_self, 2, mean)

pid <- factor(df$pid3[match(vid_save, df$caseid)], levels=c(1,2,3, 4), 
              labels=c("Democrat", "Republican", 
                       "Independent/Other", "Independent/Other"))

plot_df <- data.frame("estimate"=cand_est, 
                      "Party"=pid)
plot_df <-plot_df[!is.na(plot_df$Party),]

# plot_df <- plot_df[plot_df$Party %in% c("Democratic", "Republican"),]

png("ideo_comp_cces.png", height=8, width=10, units="in", res=200)
ggplot(NULL) +
  geom_density(aes(x=estimate, fill=Party, group=Party, color=Party),
               data=plot_df, alpha=.35, inherit.aes = F, position = "identity") +
  geom_text_repel(aes(x=m, y=0, label=params), data=tmp, inherit.aes = F) + 
  geom_point(aes(x=m, y=0), data=tmp, inherit.aes = F) +
  scale_y_continuous(limits = c(-.02, 0.22)) +
  # theme_dfp() + 
  scale_fill_manual(values=c("Democrat"="steelblue3",
                             "Republican"="orangered3", 
                             "Independent/Other"="gray")) + 
  scale_color_manual(values=c("Democrat"="steelblue2",
                              "Republican"="orangered2", 
                              "Independent/Other"="gray80")) +
  # ggtitle_dfp("Perceived Ideology of 2020 Presidential Candidates\nCompared to CCES Respondents") +
  labs(x="Percieved Ideology\n(A-M Scaled)", y="Density", 
       caption="Density curve shows the ideology of CCES respondents after A-M scaling")
dev.off()



