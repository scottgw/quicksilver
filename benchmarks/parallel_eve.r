#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape2)
library(doBy) ## For summary statistics
library(grid) ## for 'unit' usedin legend.key.size theme setting
library(xtable) ## To print LaTeX tables

source ("common.r")

args = commandArgs(trailingOnly = TRUE)
csv_file = args[1]

## print(args)
## print(csv_file)

results = read.csv(csv_file)

levels(results$Language) <- c(levels(results$Language), 'Qs')
results$Language[results$Language == 'qs'] <- 'Qs'

results$CommTime = results$TotalTime - results$CompTime

non_time_names = setdiff (names(results), c("CompTime", "CommTime"))

print (non_time_names)

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

parallel_summary_graph = function (df)
{
  m = max(df$Threads)

  df = df[df$Threads == m,]
  df = df[df$Language != 'qs',]
  
  print (df)

  p <- ggplot(df, aes(color, x=Language, y=TotalTime.mean, fill=Language))

  ## The combination of these two puts the colour down, but then
  ## removes the black crossbar from the legend.
  p <- p + geom_bar(stat="identity")
  p <- p + geom_errorbar(aes(ymin=TotalTime.mean - TotalTime.sd,
                             ymax=TotalTime.mean + TotalTime.sd),
                         width=0.25)

  p <- p + scale_fill_manual(values=c("dodgerblue3", "dodgerblue3", "dodgerblue3"))
  p <- p + guides(fill=guide_legend(title=""))
  p <- p + ylab("Time (s)")
  p <- p + theme(legend.position = "none")
  p <- p + facet_wrap(~ Task, nrow=2, scales="free")

  p <- p + theme_defaults()
  
  return (p)
}

parallel_speedup_graph = function (df)
{
  singleCore = df[df$Threads == 1,]
  df = merge(x = df,
    y = singleCore,
    by=c("Language", "Task"),
    all=TRUE)

  df = subset(df, select = -c(Threads.y))

  df$TimeScale = df$TotalTime.y / df$TotalTime.x
  
  p <- ggplot(df, aes(x=Threads.x,
                      y=TimeScale,
                      group=Language,
                      shape=Language))

  p <- p + geom_line(stat="identity",
                     aes(x=Threads.x,
                         y=TimeScale,
                         colour=Language,
                         shape=Language))
  p <- p + geom_point()

  p <- p + xlab('Benchmark') + ylab('Speedup')
  p <- p + ylim(0,32)
  p <- p + facet_wrap(~ Task, nrow=3)
# p <- p + theme(legend.position="top")
  
  p <- p + theme_defaults()

  ## only put breaks at where the cores are used.
  p <- p + scale_x_continuous(breaks=c(2,4,8,16,32))

  ggsave('parallel_speedup_eve.pdf', p, height=10, width=10, units="cm", dpi=600)
}

splits = summaryBy(TotalTime ~ Language + Task + Threads,
  data = results,
  FUN = list (mean, sd))

## print (splits[splits$Language == 'Qs',])

results =
  aggregate(
    cbind(TotalTime, CompTime, CommTime) ~ . ,
    data=results,
    FUN=median)

parallel_speedup_graph(results)

p = parallel_summary_graph(splits)

ggsave('parallel_eve.pdf', p, height=7, width=10, units="cm", dpi=600)

print ("Geometric means (total):")
print (tapply(results$TotalTime, results$Language, geom_mean))
print ("Geometric mean (comp):")
print (tapply(results$CompTime, results$Language, geom_mean))

drops = c("CompTime", "CommTime")

results = results[,!(names(results) %in% drops)]
print (xtable(dcast(results, Task + Language ~ Threads, value.var="TotalTime")))
