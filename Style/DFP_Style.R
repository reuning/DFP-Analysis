library(ggplot2)

path = fs::path_join(c(here::here(), "Style/"))
extrafont::ttf_import(paths=path)
# extrafont::loadfonts()
quartzFonts(futura=rep("FuturaBT-Heavy", 4))
quartzFonts(mstthin=rep("Montserrat-Thin", 4))
quartzFonts(mstreg=rep("Montserrat-Regular", 4))

### Functions
theme_dfp <- function(){
  theme_classic() + theme(plot.title=element_text(family="futura", hjust=.5),
        text=element_text(family="mstreg"), 
        panel.grid=element_line(size=0), 
        panel.grid.major.y=element_line(size=.5, color=alpha("gray", .5), 
                                        linetype = 2), 
        panel.border=element_rect(size=0, fill=NA))
}

thin_x <- function(){
  theme(axis.text.x = element_text(family="mstthin", color="black"))
}

thin_y <- function(){
  theme(axis.text.y = element_text(family="mstthin", color="black"))
}

ggtitle_dfp <- function(x){
  ggtitle(toupper(x)) 
    
}
pal_dfp <- scales::manual_pal(c("#124073","#0A2645","#A8BF14","#B71D1A","#BF7800","#b3b3b3"))
cont_df <- colorRampPalette(c("#0A2645","#A8BF14"))
scale_color_dfp <- function(...) { discrete_scale("colour", "dfp", pal_dfp, ...) }
scale_fill_dfp <- function(...) { discrete_scale("fill", "dfp", pal_dfp, ...) }
scale_cont_fill_dfp <- function(...) { continuous_scale("fill", "dfp", cont_df, ... )}
