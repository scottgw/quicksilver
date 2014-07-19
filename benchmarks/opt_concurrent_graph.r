#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)
library(grid) ## for 'unit' used in legend.key.size theme setting
library(doBy)
library(xtable)

args = commandArgs(trailingOnly = TRUE)
csv_file = args[1]

results = read.csv(csv_file)

## replace with more print friendly names
levels(results$Variant) <- c(levels(results$Variant),
                             'None', 'Dyn.', 'Static', 'QoQ', 'All', 'All/TC')
results$Variant[results$Variant == 'none'] <- 'None'
results$Variant[results$Variant == 'dyn'] <- 'Dyn.'
results$Variant[results$Variant == 'stat'] <- 'Static'
results$Variant[results$Variant == 'qoq'] <- 'QoQ'
results$Variant[results$Variant == 'all'] <- 'All'
results$Variant[results$Variant == 'qstc'] <- 'All/TC'
results <- droplevels(results)

results$TotalTime = as.numeric(as.character(results$TotalTime))
results$CompTime = as.numeric(as.character(results$CompTime))

## Aggregate all the timing columns by the median value.
# Task + Language + Threads,
## results =
##   aggregate(
##     cbind(TotalTime, CompTime, CommTime) ~ . ,
##     data=results,
##     FUN=median)

geom_mean = function (x) {
  return (exp(mean(log(x))))
}

concurrent_graph = function (df)
{
  p <- ggplot(df, aes(x=Variant, y=TotalTime.mean))
  p <- p + scale_fill_brewer()
  p <- p + geom_bar(stat="identity", fill="dodgerblue3")
  p <- p + xlab('Variant')
  p <- p + ylab('Time (s)')
  p <- p + facet_wrap(~ Task, nrow = 3, scales="free")

  p <- p + geom_errorbar(aes(ymin=TotalTime.mean - TotalTime.sd,
                             ymax=TotalTime.mean + TotalTime.sd),
                         width=0.25)

  ## trim plot whitespace
  p <- p + theme(plot.margin = unit(c(0,0,0,0), "cm"),
                 panel.margin = unit(0.5, "mm"),
                 strip.background = element_rect(fill=NA),
                 legend.margin = unit(-0.5, "cm"))
  
  # remove grid background
  p <- p + theme(plot.background = element_blank(),
                 panel.grid.major.y = element_line(colour="grey"),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
  
  p <- p + theme(text=element_text(family="Times", size=10),
                 axis.text=element_text(family="Times", size=8, colour="black"))

  return (p)
}

results = summaryBy(TotalTime + CompTime ~ Variant + Task + Threads,
  data = results,
  FUN = list (mean, sd))

p = concurrent_graph(results)

print (xtable(cast(results, Task ~ Variant, value="TotalTime.mean")))

ggsave('opt_concurrent.pdf', p, height=10, width=10, units="cm")

print ("Geometric means (total):")
print (tapply(results$TotalTime.mean, results$Variant, geom_mean))
