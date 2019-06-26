library(ggplot2)

source(here::here("Style"))
setwd(here::here("WTHH_Classes"))


df <- read.csv("http://filesforprogress.org/datasets/june_19_omni/dfp_RV_omni_062019.csv")
cands <- c("Joe Biden", "Bernie Sanders", "Kamala Harris", "Beto O’Rourke", "Cory Booker",
           "Amy Klobuchar", "Elizabeth Warren", "John Hickenlooper", "Kirsten Gillibrand", 
           "John Delaney", "Julián Castro", "Bill DeBlasio", "Tulsi Gabbard", "Pete Buttigieg", 
           "Jay Inslee", "Tim Ryan", "Seth Moulton", "Eric Swalwell", "Andrew Yang", 
           "Marianne Williamson", "Mike Gravel", "Steve Bullock", "Michael Bennet", "Wayne Messam")


# write.csv(data.frame(cands), "tmp.csv")

# sub_df <- df[df$primaryvote %in% c(1,2,3,4,5) & df$partyvote==1,]
sub_df <- df[df$primaryvote %in% c(1,2,3) & df$partyvote==1,]
nrow(sub_df)


issues <- unique(unlist(lapply(strsplit(grep( "issuemulti", names(df), value=T), "_"), function(x) x[2])))
issues <- issues[issues!="BDS"]

counts <- matrix(NA, nrow=length(cands), ncol=length(issues))
stand_error <- matrix(NA, nrow=length(cands), ncol=length(issues))
sub_df$stand_weights <- sub_df$weight/sum(sub_df$weight)
for(ii in 1:length(cands)){
  for(jj in 1:length(issues)){
    tmp <- by(sub_df$stand_weights, sub_df[,paste("issuemulti", issues[jj], ii, sep="_")], sum)
    counts[ii,jj] <- tmp[1]*100
    stand_error[ii,jj] <- sqrt((tmp[1])*(1-tmp[1])*sum(sub_df$stand_weights^2))*100
    
  }
  
}

rownames(counts) <- cands
rownames(stand_error) <- cands

colnames(counts) <- issues
colnames(stand_error) <- issues

counts <- reshape::melt(counts)
names(counts)[3] <- "mu"
stand_error <- reshape::melt(stand_error)
names(stand_error)[3] <- "se"

tot <- merge(counts, stand_error)
supp_scores <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQieu0ROsOnlYJfSrjdnKsbtdx8GFyAnMZoa5t6XMOLO9BBrmdVlccUdG_cwqXfyh-hdkNh_G2mOSpO/pub?gid=0&single=true&output=csv", 
                        na.strings = "", stringsAsFactors = F)
supp_scores <- reshape2::melt(supp_scores, id="cands")
names(supp_scores) <- c("X1", "X2", "Stance")
supp_scores$Stance[is.na(supp_scores$Stance)] <- "Unsure"
tot <-merge(tot, supp_scores)

names(tot)[1:2] <- c("Candidate", "Issue")
levels(tot$Issue) <- c("Free College", "Green New Deal", 
                       "Repealing Hyde", "Abolish ICE", "Medicare for All", "Reparations")
tot$Candidate <- factor(tot$Candidate, levels=rev(levels(tot$Candidate)))

png("voter_issues.png", height=8, width=8, 
    units="in", res=200)
ggplot(tot, aes(y=mu, ymin=mu - 1.96*se, 
                   ymax=mu+1.96*se, x=Candidate, color=Stance)) + coord_flip() +
  geom_pointrange() + 
  # theme_dfp() + ggtitle_dfp("Which Candidate do you think supports...") +
  labs(x="", y="Percentage of Voters Believing a Candidate Supports a Policy") + facet_wrap(~Issue) +
  scale_color_manual("Actual\nStance", breaks=c("Supports", "Unsure", "Opposes"),
                     values=c("Supports"="springgreen4", "Opposes"="orangered3", "Unsure"="yellow3")) +
  NULL

dev.off()






name_id <-  grep("NAMEID_20", names(df), value=T)[1:24]
counts <- matrix(NA, nrow=length(cands), ncol=length(issues))
stand_error <- matrix(NA, nrow=length(cands), ncol=length(issues))
sub_df$stand_weights <- sub_df$weight/sum(sub_df$weight)
for(ii in 1:length(cands)){
  tmp_df <- sub_df[sub_df[,name_id[ii]] %in% c(1,2),]
  tmp_df$stand_weights <- tmp_df$weight/sum(tmp_df$weight)
  if(nrow(tmp_df) < 50) next 
  for(jj in 1:length(issues)){
    tmp <- by(tmp_df$stand_weights, tmp_df[,paste("issuemulti", issues[jj], ii, sep="_")], sum)
    counts[ii,jj] <- tmp[1]*100
    stand_error[ii,jj] <- sqrt((tmp[1])*(1-tmp[1])*sum(tmp_df$stand_weights^2))*100
    
  }
  
}

rownames(counts) <- cands
rownames(stand_error) <- cands

colnames(counts) <- issues
colnames(stand_error) <- issues

counts <- reshape::melt(counts)
names(counts)[3] <- "mu"
stand_error <- reshape::melt(stand_error)
names(stand_error)[3] <- "se"


tot <- merge(counts, stand_error)
supp_scores <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQieu0ROsOnlYJfSrjdnKsbtdx8GFyAnMZoa5t6XMOLO9BBrmdVlccUdG_cwqXfyh-hdkNh_G2mOSpO/pub?gid=0&single=true&output=csv", 
                        na.strings = "", stringsAsFactors = F)
supp_scores <- reshape2::melt(supp_scores, id="cands")
names(supp_scores) <- c("X1", "X2", "Stance")
supp_scores$Stance[is.na(supp_scores$Stance)] <- "Unsure"
tot <-merge(tot, supp_scores)

names(tot)[1:2] <- c("Candidate", "Issue")
levels(tot$Issue) <- c("Free College", "Green New Deal", 
                       "Repealing Hyde", "Abolish ICE", "Medicare for All", "Reparations")
tot$Candidate <- factor(tot$Candidate, levels=rev(levels(tot$Candidate)))
tot <- tot[!is.na(tot$mu),]
tot$Candidate <- droplevels(tot$Candidate)
png("voter_issues_knowledgable.png", height=8, width=8, 
    units="in", res=200)
ggplot(tot, aes(y=mu, ymin=mu - 1.96*se, 
                ymax=mu+1.96*se, x=Candidate, color=Stance)) + coord_flip() +
  geom_pointrange() + 
  # theme_dfp() + ggtitle_dfp("Which Candidate do you think supports...\n(Only Voters who have heard\nsomething about a candidate)") +
  labs(x="", y="Percentage of Voters Believing a Candidate Supports a Policy") + facet_wrap(~Issue) +
  scale_color_manual("Actual\nStance", breaks=c("Supports", "Unsure", "Opposes"),
                     values=c("Supports"="springgreen4", "Opposes"="orangered3", "Unsure"="yellow3")) +
  NULL

dev.off()
