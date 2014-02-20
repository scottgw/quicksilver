#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)
library(grid) ## for 'unit' used in legend.key.size theme setting

args = commandArgs(trailingOnly = TRUE)
csv_file = args[1]

## print(args)
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

parallel_summary_graph = function (df)
{
  m = max(df$Threads)

  df = splits[df$Threads == m,]

  p <- ggplot(df, aes(x=Language, y=Time, fill=TimeType))

  # The combination of these two puts the colour down, but then
  # removes the black crossbar from the legend.
  p <- p + geom_bar(stat="identity")
  ## p <- p + geom_bar(stat="identity", colour="black", show_guide=FALSE)

  p <- p + scale_fill_manual(values=c("dodgerblue3", "cornflowerblue"),
                             breaks=c("CompTime", "CommTime"),
                             labels=c("Computation time",
                               "Communication time"))
  p <- p + guides(fill=guide_legend(title=""))
  p <- p + theme(legend.position = "top")
  p <- p + facet_grid(~ Task, scales="free_y")

  
  # change the fonts to Times
  p <- p + theme(text=element_text(family="Times", size=8),
                 axis.text=element_text(family="Times", size=6, colour="black"),
                 legend.key.size = unit(0.3, "cm"))

  return (p)
}

parallel_speedup_graph = function (df)
{
  with_comp_time = df[df$Language == 'Qs' | df$Language == 'erlang',]

  levels(with_comp_time$Language)[levels(with_comp_time$Language) == 'Qs'] <- 'Qs (comp.)'
  levels(with_comp_time$Language)[levels(with_comp_time$Language) == 'erlang'] <- 'erlang (comp.)'
  
  with_comp_time = subset(with_comp_time, select = -c(TotalTime, CommTime))
  names(with_comp_time) = c("Task", "Language", "Threads", "Time")

  with_total_time = subset(df, select = -c(CompTime, CommTime))
  names(with_total_time) = c("Task", "Language", "Threads", "Time")

  df = rbind(with_total_time, with_comp_time)

  singleCore = df[df$Threads == 1,]
  df =
    merge(x = df,
          y = singleCore,
          by=c("Language", "Task"),
          all=TRUE)
  df = subset(df, select = -c(Threads.y))
  df$TimeScale = df$Time.y / df$Time.x
  
  p <- ggplot(df, aes(x=Threads.x,
                      y=TimeScale,
                      group=Language,
                      shape=Language))
  p <- p + geom_line(stat="identity",
                     aes(x=Threads.x,
                         y=TimeScale,
                         colour=Language,
                         shape=Language))
  p <- p + geom_point(size = I(1.3))
  p <- p + xlab('Benchmark')
  p <- p + ylab('Speedup')
  p <- p + facet_grid(~ Task, scales="free_y")
  p <- p + theme(legend.position="top")

  p <- p + scale_x_continuous(breaks=c(2,4,8,16,32))

  # change the fonts to Times
  p <- p + theme(text=element_text(family="Times", size=8),
                 axis.text=element_text(family="Times", size=6, colour="black"),
                 legend.key.size = unit(0.3, "cm"))

  ggsave('parallel_speedup.pdf', p, height=6, width=18, units="cm", dpi=600)
}

splits = split_comm_time(results)
parallel_speedup_graph(results)

p = parallel_summary_graph(splits)

ggsave('parallel.pdf', p, height=6, width=18, units="cm", dpi=600)

print ("Geometric means (total):")
print (tapply(results$TotalTime, results$Language, geom_mean))
print ("Geometric mean (comp):")
print (tapply(results$CompTime, results$Language, geom_mean))
