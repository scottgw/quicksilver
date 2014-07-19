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

levels(results$Language) <- c(levels(results$Language),
                             'Qs', 'Erl.', 'Hask.', 'C++', 'Go')
results$Language[results$Language == 'qs'] <- 'Qs'
results$Language[results$Language == 'go'] <- 'Go'
results$Language[results$Language == 'cxx'] <- 'C++'
results$Language[results$Language == 'haskell'] <- 'Hask.'
results$Language[results$Language == 'erlang'] <- 'Erl.'


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
  p <- ggplot(df, aes(x=Language, y=TotalTime.mean))
  p <- p + scale_fill_brewer()
  p <- p + geom_bar(stat="identity", fill="dodgerblue3")
  p <- p + xlab('Language')
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

results = summaryBy(TotalTime + CompTime ~ Language + Task + Threads,
  data = results,
  FUN = list (mean, sd))

print (xtable(cast(results, Task ~ Language, value="TotalTime.mean")))


p = concurrent_graph(results)
ggsave('concurrent.pdf', p, height=10, width=10, units="cm")

print ("Geometric mean:")
print (tapply(results$TotalTime.mean, results$Language, geom_mean))
