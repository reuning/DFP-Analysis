library(ggplot2)
library(rstan)

source(here::here("Style"))
setwd(here::here("AM_Scaling"))

load("CCES2018_OUTPUT.RData") ### Available from the CCES Dataverse
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

sel <- as.character(vid) %in% unique(vid[rid %in% cands])
rid <- rid[sel]
vid <- vid[sel]
Y <- Y[sel]



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

rid[!rid %in% sub_names] <- NA


rid <- as.numeric(factor(rid, levels=sub_names))

vid <- vid[!is.na(rid)]
Y <- Y[!is.na(rid)]
rid <- rid[!is.na(rid)]

vid <- as.numeric(as.factor(vid))
J <- length(sub_names)

N <- max(vid)
r_N <- length(Y)

mod <- stan(file="A_M.stan", cores=2, chains=2)
# mod <- vb(model)


# cands[!cands %in% all_names]
# 
# cands_id <- match(cands, all_names)

tmp <- stan_plot(mod, "theta")
tmp <- tmp$data
tmp$params <- sub_names

png("AM_Scaling.png", height=6, width=6, units='in', 
    res=200)
ggplot(tmp, aes(x=reorder(params, m), y=m, ymin=ll, ymax=hh)) +
  geom_pointrange() + coord_flip() +
  # theme_dfp() +
  # ggtitle_dfp("Aldrich-Mckelvey Scaling of Candidates") +
  labs(y="Ideology", x="",
       caption = 'Aldrich-Mckelvey Scaling using CCES 2018 Candidate Placements\nTrump, Democratic Party, Republican Party and Supreme Court used to bridge.')
dev.off()

raw <- by(Y, rid, mean)

tmp$raw <- c(raw)
ggplot(tmp, aes(x=raw, y=m, ymin=ll, ymax=hh, label=params)) +
  geom_text() + coord_flip() +
  theme_minimal()

stan_plot(mod, paste("alpha[", 1:40, "]", sep=""))
stan_plot(mod, paste("beta[", 1:40, "]", sep=""))
