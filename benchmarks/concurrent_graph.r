#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)

args = commandArgs(trailingOnly = TRUE)
csv_file = args[1]

## print(args)
## print(run_type)
## print(csv_file)

results = read.csv(csv_file)

levels(results$Language) <- c(levels(results$Language), 'Qs')
results$Language[results$Language == 'qs'] <- 'Qs'

results$CommTime = results$TotalTime - results$CompTime

non_time_names = setdiff (names(results), c("TotalTime", "CompTime", "CommTime"))

print (non_time_names)

## Aggregate all the timing columns by the median value.
# Task + Language + Threads,
results =
  aggregate(
    cbind(TotalTime, CompTime, CommTime) ~ . ,
    data=results,
    FUN=median)

print(names(results))

split_comm_time = function (df)
{
  ## drop the total time column
  split_df = subset(df, select = -c(TotalTime))
  ## melt them so that we get the comp and comm times as a column 
  split_df = melt(split_df, id=(non_time_names)) # c("Language", "Task", "Threads")))

  ## rename them because melt has given them bad names
  names(split_df)[names(split_df) == 'variable'] = 'TimeType'
  names(split_df)[names(split_df) == 'value'] = 'Time'

  return (split_df)
}

geom_mean = function (x) {
  return (exp(mean(log(x))))
}

concurrent_graph = function (df)
{
  p <- ggplot(df, aes(x=Language, y=TotalTime))
  p <- p + scale_fill_brewer()
  p <- p + geom_bar(stat="identity", fill="dodgerblue3")
  p <- p + xlab('Language')
  p <- p + ylab('Time (s)')
  p <- p + facet_grid(~ Task, scales="free_y")

  p <- p + theme(text=element_text(family="Times", size=8))

  return (p)
}

p = concurrent_graph(results)

ggsave('concurrent.pdf', p, height=5, width=18, units="cm")

print ("Geometric means (total):")
print (tapply(results$TotalTime, results$Language, geom_mean))
print ("Geometric mean (comp):")
print (tapply(results$CompTime, results$Language, geom_mean))
