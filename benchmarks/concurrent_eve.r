#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)
library(grid) ## for 'unit' used in legend.key.size theme setting
library(doBy)

args = commandArgs(trailingOnly = TRUE)
csv_file = args[1]

## print(args)
## print(run_type)
## print(csv_file)

results = read.csv(csv_file)

levels(results$Language) <- c(levels(results$Language), 'Qs')
results$Language[results$Language == 'qs'] <- 'Qs'

## Aggregate all the timing columns by the median value.
# Task + Language + Threads,
## results =
##   aggregate(
##     cbind(TotalTime, CompTime, CommTime) ~ . ,
##     data=results,
##     FUN=median)

print(names(results))

geom_mean = function (x) {
  return (exp(mean(log(x))))
}

concurrent_graph = function (df)
{
  p <- ggplot(df, aes(x=Language, y=TotalTime.mean))
  p <- p + scale_fill_brewer()
  p <- p + geom_bar(stat="identity", fill="dodgerblue3")
  p <- p + xlab('Language')
  p <- p + ylab('Time (s)')
  p <- p + facet_wrap(~ Task, nrow = 2, scales="free")

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

results = summaryBy(TotalTime + CompTime ~ Language + Task + Threads,
  data = results,
  FUN = list (mean, sd))

print (results)

p = concurrent_graph(results)

ggsave('concurrent_eve.pdf', p, height=8, width=10, units="cm")

print ("Geometric means (total):")
print (tapply(results$TotalTime.mean, results$Language, geom_mean))
print ("Geometric mean (comp):")
print (tapply(results$CompTime.mean, results$Language, geom_mean))
