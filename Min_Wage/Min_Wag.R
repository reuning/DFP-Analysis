library(ggplot2)
library(gridExtra)
library(png)

source(here::here("Style/DFP_Style.R"))
setwd(here::here("Min_Wage"))

img <- readPNG(here::here("Style/dfp-line-logo-black.png"))
logo <- rasterGrob(img, interpolate=TRUE)

df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQuiD_BqRGR-uYSFmbdtu1wgMAxESz3c43dEhIUxAqiXPtwS8UfVdCXAhuM159P_9VNxCVxIsmPsM1T/pub?gid=1604709981&single=true&output=csv", stringsAsFactors = F)

df$Share.of.workers.affected. <- as.numeric(gsub("%", "", df$Share.of.workers.affected.))
df$Number.of.workers.affected <- as.numeric(gsub(",", "", df$Number.of.workers.affected))

df$Trump.Win <- factor(df$Trump.Win, levels=0:1, labels=c("Clinton\nWin", "Trump\nWin"))

p1 <- ggplot(df, aes(y=Share.of.workers.affected., x=Trump.Win, fill=Trump.Win)) + 
  geom_violin(draw_quantiles=.5, alpha=.7)  + 
  scale_fill_manual(values=c("#0078E5", "#D30707")) + guides(fill=F) +  
  # theme_dfp() + 
  labs(x="Congressional District", y="Percentage of Workers Affected") + 
  # ggtitle_dfp("Percentage of Workers Affected\nby Min. Wage Increase") +
  NULL


p2 <- ggplot(df, aes(y=Number.of.workers.affected, x=Trump.Win, fill=Trump.Win)) + 
  geom_col(alpha=.7) +
  # theme_dfp() + 
  labs(x="Congressional District", y="Total Number of Workers Affected") +
  scale_fill_manual(values=c("#0078E5", "#D30707")) + guides(fill=F) +  
  # ggtitle_dfp("Total Workers Affected\nby Min. Wage Increase") +
  NULL


p <- grid.arrange(p2, p1, logo, layout_matrix=matrix(c(1, 1, 4, 1, 1, 4, 2, 2, 4, 2, 2, 3), 3, 4),
                  widths=c(4,4,4,4), heights=c(3,3,.25))
ggsave("min_worker.png", p, dpi=320, height = 6, width=8, units="in")


df$Share.of..Asian.or.other..workers.affected <- as.numeric(gsub("%", "", df$Share.of..Asian.or.other..workers.affected))
df$Share.of.black.workers.affected <- as.numeric(gsub("%", "", df$Share.of.black.workers.affected))
df$Share.of.white.workers.affected <- as.numeric(gsub("%", "", df$Share.of.white.workers.affected))
df$Share.of.Hispanic.workers.affected <- as.numeric(gsub("%", "", df$Share.of.Hispanic.workers.affected))


tmp <- df[,c("Share.of..Asian.or.other..workers.affected",
             "Share.of.black.workers.affected", 
             "Share.of.white.workers.affected",
             "Share.of.Hispanic.workers.affected")]
names(tmp) <- c("Asian or Other Workers", "Black Workers", 
                "White Workers", "Hispanic Workers")
tmp <- reshape2::melt(tmp)

p1 <- ggplot(tmp, aes(y=value, x=variable)) + 
  geom_violin(fill="#124073", draw_quantiles=.5)  + 
  theme_dfp() + labs(x="", y="Percentage of Workers Affected") + 
  ggtitle_dfp("Minimum Wage Impact by Race")


p <- grid.arrange(p1, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("min_worker_race.png", p, dpi=320, height = 6, width=6, units="in")





df$Share.of.working.men.affected <- as.numeric(gsub("%", "", df$Share.of.working.men.affected))
df$Share.of.working.women.affected <- as.numeric(gsub("%", "", df$Share.of.working.women.affected))


tmp <- df[,c("Share.of.working.men.affected",
             "Share.of.working.women.affected")]
names(tmp) <- c("Men Workers", "Women Workers")
tmp <- reshape2::melt(tmp)

p1 <- ggplot(tmp, aes(y=value, x=variable)) + 
  geom_violin(fill="#124073", draw_quantiles=.5)  + 
  # theme_dfp() + 
  labs(x="", y="Percentage of Workers Affected") + 
  # ggtitle_dfp("Minimum Wage Impact by Gender") +
  NULL


p <- grid.arrange(p1, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("min_worker_gen.png", p, dpi=320, height = 6, width=6, units="in")


df$Rep.Margin <-  as.numeric(df$Rep.Margin)
p1 <- ggplot(df, aes(x=Rep.Margin, y=Number.of.workers.affected)) + geom_point() + 
  # theme_dfp() + 
  labs(x="Trump Margin", y="Number of Workers Affected") +
  # ggtitle_dfp("Number of Workers AFfected by Trump Margin\n(Trump Districts Only)") +
  NULL

p <- grid.arrange(p1, logo, layout_matrix=matrix(c(1, 1, 3, 1, 1, 3, 1, 1, 2), 3, 3),
                  widths=c(4,1,2), heights=c(4,.25,.25))
ggsave("trump_marg.png", p, dpi=320, height = 6, width=6, units="in")

