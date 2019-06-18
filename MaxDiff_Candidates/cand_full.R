source(here::here("Style"))
setwd(here::here("MaxDiff_Candidates"))

library(rstan)
library(ggplot2)

df <- read.csv("http://filesforprogress.org/datasets/may_19_omni/data.csv")

key <- c("CHOICE20_MD1_1"="Julián Castro","CHOICE20_MD1_2"="Stacey Abrams",
"CHOICE20_MD1_3"="Seth Moulton","CHOICE20_MD1_4"="Mike Gravel",
"CHOICE20_MD1_5"="Wayne Messam","CHOICE20_MD2_1"="Joe Biden",
"CHOICE20_MD2_2"="Elizabeth Warren","CHOICE20_MD2_3"="Stacey Abrams",
"CHOICE20_MD2_4"="Bill DeBlasio","CHOICE20_MD2_5"="Marianne Williamson",
"CHOICE20_MD3_1"="Beto O’Rourke","CHOICE20_MD3_2"="Elizabeth Warren",
"CHOICE20_MD3_3"="Andrew Yang","CHOICE20_MD3_4"="Michael Bennet",
"CHOICE20_MD3_5"="Wayne Messam","CHOICE20_MD4_1"="Amy Klobuchar",
"CHOICE20_MD4_2"="Kirsten Gillibrand","CHOICE20_MD4_3"="Julián Castro",
"CHOICE20_MD4_4"="Jay Inslee","CHOICE20_MD4_5"="Eric Swalwell",
"CHOICE20_MD5_1"="Cory Booker","CHOICE20_MD5_2"="Bill DeBlasio",
"CHOICE20_MD5_3"="Jay Inslee","CHOICE20_MD5_4"="Andrew Yang",
"CHOICE20_MD5_5"="Steve Bullock","CHOICE20_MD6_1"="Beto O’Rourke",
"CHOICE20_MD6_2"="Amy Klobuchar","CHOICE20_MD6_3"="Stacey Abrams",
"CHOICE20_MD6_4"="Tulsi Gabbard","CHOICE20_MD6_5"="Steve Bullock",
"CHOICE20_MD7_1"="Joe Biden","CHOICE20_MD7_2"="John Hickenlooper",
"CHOICE20_MD7_3"="Pete Buttigieg","CHOICE20_MD7_4"="Jay Inslee",
"CHOICE20_MD7_5"="Wayne Messam","CHOICE20_MD8_1"="Bernie Sanders",
"CHOICE20_MD8_2"="Cory Booker","CHOICE20_MD8_3"="Elizabeth Warren",
"CHOICE20_MD8_4"="Seth Moulton","CHOICE20_MD8_5"="Eric Swalwell",
"CHOICE20_MD9_1"="Kamala Harris","CHOICE20_MD9_2"="John Hickenlooper",
"CHOICE20_MD9_3"="Tim Ryan","CHOICE20_MD9_4"="Seth Moulton",
"CHOICE20_MD9_5"="Steve Bullock","CHOICE20_MD10_1"="John Hickenlooper",
"CHOICE20_MD10_2"="Tulsi Gabbard","CHOICE20_MD10_3"="Eric Swalwell",
"CHOICE20_MD10_4"="Marianne Williamson","CHOICE20_MD10_5"="Michael Bennet",
"CHOICE20_MD11_1"="Joe Biden","CHOICE20_MD11_2"="Cory Booker",
"CHOICE20_MD11_3"="John Delaney","CHOICE20_MD11_4"="Tulsi Gabbard",
"CHOICE20_MD11_5"="Mike Gravel","CHOICE20_MD12_1"="Kamala Harris",
"CHOICE20_MD12_2"="John Delaney","CHOICE20_MD12_3"="Julián Castro",
"CHOICE20_MD12_4"="Andrew Yang","CHOICE20_MD12_5"="Marianne Williamson",
"CHOICE20_MD13_1"="Kirsten Gillibrand","CHOICE20_MD13_2"="Bill DeBlasio",
"CHOICE20_MD13_3"="Tim Ryan","CHOICE20_MD13_4"="Mike Gravel",
"CHOICE20_MD13_5"="Michael Bennet","CHOICE20_MD14_1"="Joe Biden",
"CHOICE20_MD14_2"="Bernie Sanders","CHOICE20_MD14_3"="Kamala Harris",
"CHOICE20_MD14_4"="Beto O’Rourke","CHOICE20_MD14_5"="Kirsten Gillibrand",
"CHOICE20_MD15_1"="Amy Klobuchar","CHOICE20_MD15_2"="Elizabeth Warren",
"CHOICE20_MD15_3"="John Delaney","CHOICE20_MD15_4"="Pete Buttigieg",
"CHOICE20_MD15_5"="Tim Ryan","CHOICE20_MD16_1"="Bernie Sanders",
"CHOICE20_MD16_2"="Pete Buttigieg","CHOICE20_MD16_3"="Marianne Williamson",
"CHOICE20_MD16_4"="Mike Gravel","CHOICE20_MD16_5"="Steve Bullock",
"CHOICE20_MD17_1"="Kirsten Gillibrand","CHOICE20_MD17_2"="Tulsi Gabbard",
"CHOICE20_MD17_3"="Pete Buttigieg","CHOICE20_MD17_4"="Seth Moulton",
"CHOICE20_MD17_5"="Andrew Yang")

sub_df <- df[df$primaryvote %in% c(1,2,3) & df$partyvote %in% c(1,3),]
sub_df_char <- sub_df
levels <- sort(unique(key))
top_cands <- levels[c(1,2,3,4,6,7,9,10,14,15,13,17,19)]
choice_sets <- names(key)

choice_sets <- gsub("_1", "", choice_sets[seq(1,81, by=5)])

sub_df <- sub_df[,names(key)]
sub_df$id <- 1:nrow(sub_df)

out <- NULL
for(jj in 1:length(choice_sets)){
  tmp <- sub_df[,c(paste(choice_sets[jj], 1:5, sep="_"), "id")]
  names(tmp)[1:5] <- 1:5
  tmp$set <- choice_sets[jj]
  out <- rbind(out, tmp)
}



first <- matrix(NA, nrow=20, ncol=length(choice_sets))
second <- matrix(NA, nrow=20, ncol=length(choice_sets))
for(ii in 1:length(choice_sets)){
  tmp <- expand.grid(key[paste(choice_sets[ii], 1:5, sep="_")], 
                     key[paste(choice_sets[ii], 1:5, sep="_")])
  
  tmp <- tmp[tmp$Var1 != tmp$Var2, ]
  first[,ii] <- as.character(tmp$Var1)
  second[,ii] <- as.character(tmp$Var2)
  
}



pos_choice <- apply(out[,1:5], 1, function(x) which(x==1))
neg_choice <- apply(out[,1:5], 1, function(x) which(x==2))

pos_choice <- key[paste(out$set, pos_choice, sep="_")]
neg_choice <- key[paste(out$set, neg_choice, sep="_")]
set <- as.numeric(substr(out$set, 12, 15))

choice <- numeric(length(set))

for(ii in 1:length(set)){
  tmp <- which(pos_choice[ii] == first[,set[ii]] )
  choice[ii] <- tmp[which(tmp %in% which(neg_choice[ii] == second[,set[ii]] ))]
}

N <- length(choice)

first <- apply(first, 2, function(x) match(x, levels))
second <- apply(second, 2, function(x) match(x, levels))

K <- max(first)
J <- ncol(first)

vid <- out$id

race <- sub_df_char$race3
educ <- sub_df_char$educ4
age <- sub_df_char$age5
ideo <- sub_df_char$ideo5
ideo[ideo==5] <- 4
ideo[ideo==6] <- 5

gender <- sub_df_char$gender
inc <- (sub_df_char$faminc5)

M <- length(race)


mod <- stan(file="choice.stan", chains=2, cores=2, iter = 2000)
tmp <- stan_plot(mod, "alpha") 
tmp <- tmp$data
tmp$params <- levels


ggplot(tmp, aes(x=reorder(params, m), y=m, ymin=ll, ymax=hh)) + geom_pointrange() +
  coord_flip() + theme_minimal() + labs(x="", y="Latent Support")
stan_rhat(mod)

ordered_levels <- (levels[order(apply(out$alpha, 2, mean))])

out <- extract(mod)


ideo <- array(NA, dim(out$ideo_beta))
for(ii in 1:5){
  ideo[,ii,] <- out$alpha + out$ideo_beta[,ii,]
  
}

ideo <- apply(ideo, c(2,3),quantile, c(.025, .5, .975))
dimnames(ideo) <- list("Stat"=c("lo", "mid", "hi"), 
                       "Ideo"=c("Very Liberal","Liberal", "Moderate", 
                                "Cons", "Other"), 
                       "Cand"=levels)
ideo <- reshape::melt(ideo)
ideo <- reshape::cast(ideo, Cand+Ideo~Stat)
ideo <- ideo[ideo$Cand %in% top_cands,]
ideo$Cand <- factor(ideo$Cand, levels=ordered_levels)
ideo$Ideo <- factor(ideo$Ideo, levels=rev(c("Very Liberal","Liberal", "Moderate", 
                                        "Cons", "Other")))

png("ideo.png", height=8, width=6, units="in", res=200)
ggplot(ideo, aes(x=Cand, y=mid, group=Ideo, color=Ideo, ymin=lo, ymax=hi)) + 
  geom_pointrange(position = position_dodge2(width=.5)) +
  coord_flip() +  labs(x="", y="Latent Support by Ideology") + 
  # theme_dfp() + 
  scale_color_manual("Ideology", 
                                   c("Very Liberal","Liberal", "Moderate",
                                     "Cons", "Other"), 
                                   values=rev(c("#0A2645","#79A5C6","#b3b3b3",
                                            "#F75757","springgreen4")))  +
  # ggtitle_dfp("Candidate Support by Ideology") + 
  NULL

dev.off()

educ <- array(NA, dim(out$educ_beta))
for(ii in 1:4){
  educ[,ii,] <- out$alpha + out$educ_beta[,ii,]
  
}

educ <- apply(educ, c(2,3),quantile, c(.025, .5, .975))
dimnames(educ) <- list("Stat"=c("lo", "mid", "hi"), 
                       "educ"=c("High School or Less", 
                                "Some College", 
                                "College", "Post Graduate"), 
                       "Cand"=levels)
educ <- reshape::melt(educ)
educ <- reshape::cast(educ, Cand+educ~Stat)
educ <- educ[educ$Cand %in% top_cands,]
educ$Cand <- factor(educ$Cand, levels=ordered_levels)
educ$educ <- factor(educ$educ, levels=c("High School or Less", 
                                        "Some College", 
                                        "College", "Post Graduate"))
png("educ.png", height=8, width=6, units="in", res=200)
ggplot(educ, aes(x=Cand, y=mid, group=educ, color=educ, ymin=lo, ymax=hi)) + 
  geom_pointrange(position = position_dodge2(width=.5)) +
  coord_flip() +  labs(x="", y="Latent Support") + 
  # theme_dfp() + 
  scale_color_manual("Education", 
                                   rev(c("High School or Less", "Some College", 
                                         "College", "Post Graduate")), 
                                   values=c("#0A2645","#79A5C6","#b3b3b3",
                                            "#F75757"))  +
  # ggtitle_dfp("Candidate Support by Education") +
  NULL 
dev.off()




gender <- array(NA, dim(out$gender_beta))
for(ii in 1:2){
  gender[,ii,] <- out$alpha + out$gender_beta[,ii,]
  
}

gender <- apply(gender, c(2,3),quantile, c(.025, .5, .975))
dimnames(gender) <- list("Stat"=c("lo", "mid", "hi"), 
                       "gender"=c("Male", "Female"), 
                       "Cand"=levels)
gender <- reshape::melt(gender)
gender <- reshape::cast(gender, Cand+gender~Stat)
gender <- gender[gender$Cand %in% top_cands,]
gender$Cand <- factor(gender$Cand, levels=ordered_levels)
gender$gender <- factor(gender$gender, levels=c("Male", "Female"))
png("gender.png", height=8, width=6, units="in", res=200)
ggplot(gender, aes(x=Cand, y=mid, group=gender, color=gender, ymin=lo, ymax=hi)) + 
  geom_pointrange(position = position_dodge2(width=.5)) +
  coord_flip() +  labs(x="", y="Latent Support") + 
  # theme_dfp() + 
  scale_color_manual("Sex", 
                                   (c("Female", "Male")), 
                                   values=c("springgreen4",  "darkgoldenrod2"))  +
  ggtitle_dfp("Candidate Support by Sex") + 
  # NULL 
dev.off()





inco <- array(NA, dim(out$inco_beta))
for(ii in 1:6){
  inco[,ii,] <- out$alpha + out$inco_beta[,ii,]
  
}


inco <- apply(inco, c(2,3),quantile, c(.025, .5, .975))
dimnames(inco) <- list("Stat"=c("lo", "mid", "hi"), 
                         "inco"=c("Less Than $30,000","$30,000 - $59,999",
                                  "$60,000 - $99,999","$100,000 - $149,999",
                                  "$150,000+","Prefer not to say"),
                         "Cand"=levels)
inco <- reshape::melt(inco)
inco <- reshape::cast(inco, Cand+inco~Stat)
inco <- inco[inco$Cand %in% top_cands,]
inco$Cand <- factor(inco$Cand, levels=ordered_levels)
inco$inco <- factor(inco$inco, levels=c("Less Than $30,000","$30,000 - $59,999",
                                        "$60,000 - $99,999","$100,000 - $149,999",
                                        "$150,000+","Prefer not to say"))

png("inco.png", height=8, width=6, units="in", res=200)
ggplot(inco, aes(x=Cand, y=mid, group=inco, color=inco, ymin=lo, ymax=hi)) + 
  geom_pointrange(position = position_dodge2(width=.5)) +
  coord_flip() +  labs(x="", y="Latent Support by Income") + 
  # theme_dfp() + 
  scale_color_manual("Household\nIncome", 
                                   rev(c("Less Than $30,000","$30,000 - $59,999",
                                          "$60,000 - $99,999","$100,000 - $149,999",
                                          "$150,000+","Prefer not to say")),
                                   values=c("#0A2645","#79A5C6","#b3b3b3","#F75757","#B71D1A", 
                                            "springgreen4"))  +
  # ggtitle_dfp("Candidate Support by Income") + 
  NULL

dev.off()



age <- array(NA, dim(out$age_beta))
for(ii in 1:5){
  age[,ii,] <- out$alpha + out$age_beta[,ii,]
  
}

  

age <- apply(age, c(2,3),quantile, c(.025, .5, .975))
dimnames(age) <- list("Stat"=c("lo", "mid", "hi"), 
                       "age"=c("18-29", "30-44", "45-54", "55-64", "65+")
,
                       "Cand"=levels)
age <- reshape::melt(age)
age <- reshape::cast(age, Cand+age~Stat)
age <- age[age$Cand %in% top_cands,]
age$Cand <- factor(age$Cand, levels=ordered_levels)
age$age <- factor(age$age, levels=c("18-29", "30-44", "45-54", "55-64", "65+"))

png("age.png", height=8, width=6, units="in", res=200)
ggplot(age, aes(x=Cand, y=mid, group=age, color=age, ymin=lo, ymax=hi)) + 
  geom_pointrange(position = position_dodge2(width=.5)) +
  coord_flip() +  labs(x="", y="Latent Support") + 
  # theme_dfp() + 
  scale_color_manual("Age", 
                                   rev(c("18-29", "30-44", "45-54", "55-64", "65+")),
                                   values=c("#0A2645","#79A5C6","#b3b3b3","#F75757","#B71D1A")) +
  # ggtitle_dfp("Candidate Support by Age") + 
  NULL

dev.off()






race <- array(NA, dim(out$race_beta))
for(ii in 1:3){
  race[,ii,] <- out$alpha + out$race_beta[,ii,]
  
}



race <- apply(race, c(2,3),quantile, c(.025, .5, .975))
dimnames(race) <- list("Stat"=c("lo", "mid", "hi"), 
                      "race"=c("White", "Black", "Hispanic (non-White)")
                      ,
                      "Cand"=levels)
race <- reshape::melt(race)
race <- reshape::cast(race, Cand+race~Stat)
race <- race[race$Cand %in% top_cands,]
race$Cand <- factor(race$Cand, levels=ordered_levels)
race$race <- factor(race$race, levels=c("White", "Black", "Hispanic (non-White)"))

png("race.png", height=8, width=6, units="in", res=200)
ggplot(race, aes(x=Cand, y=mid, group=race, color=race, ymin=lo, ymax=hi)) + 
  geom_pointrange(position = position_dodge2(width=.5)) +
  coord_flip() +  labs(x="", y="Latent Support") + 
  # theme_dfp() + 
  scale_color_manual("Race", 
                                   rev(c("White", "Black", "Hispanic (non-White)")),
                                   values=c("#0A2645","#b3b3b3","#B71D1A")) +
  # ggtitle_dfp("Candidate Support by Race") + 
  NULL
dev.off()





