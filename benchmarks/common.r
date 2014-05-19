library(plyr)
library(ggplot2)
library(reshape)
library(doBy) ## For summary statistics
library(grid) ## for 'unit' used in legend.key.size theme setting

theme_defaults = function()
  {
    ## trim plot whitespace
    whitespace <- theme(plot.margin = unit(c(0,0,0,0), "cm"),
                        panel.margin = unit(0.5, "mm"),
                        strip.background = element_rect(fill=NA, ),
                        legend.margin = unit(-0.5, "cm"))

    ## remove grid background
    nogrid <- theme(plot.background = element_blank(),
                    panel.grid.major.y = element_line(colour="grey"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank())
                           
    ## change the fonts to Times
    fonts <- theme(text=element_text(family="Times", size=10),
                   axis.text=element_text(family="Times", size=8, colour="black"))

    return (whitespace + nogrid + fonts)
  }

