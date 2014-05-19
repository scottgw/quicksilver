#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)
library(doBy) ## For summary statistics
library(grid) ## for 'unit' used in legend.key.size theme setting

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

split_comm_time = function (df)
{
  ## drop the total time column
  ## split_df = subset(df, select = -c(TotalTime))
  ## melt them so that we get the comp and comm times as a column 
  split_df = melt(df, id=(non_time_names)) # c("Language", "Task", "Threads")))

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

  df = df[df$Threads == m,]
  df = df[df$Language != 'qs',]
  df$TotalTime.mean[df$TimeType == 'CommTime'] <- NA
  df$TotalTime.sd[df$TimeType == 'CommTime'] <- NA
  
  print (df)

  p <- ggplot(df, aes(x=Language, y=Time.mean, fill=TimeType))

  # The combination of these two puts the colour down, but then
  # removes the black crossbar from the legend.
  p <- p + geom_bar(stat="identity")
  p <- p + geom_errorbar(aes(ymin=TotalTime.mean - TotalTime.sd,
                             ymax=TotalTime.mean + TotalTime.sd),
                         width=0.25)

  ## p <- p + geom_bar(stat="identity", colour="black", show_guide=FALSE)

  p <- p + scale_fill_manual(values=c("dodgerblue3", "darkslategray3"),
                             breaks=c("CompTime", "CommTime"),
                             labels=c("Computation time",
                               "Communication time"))
  p <- p + guides(fill=guide_legend(title=""))
  p <- p + ylab("Time (s)")
  p <- p + theme(legend.position = "top")
  p <- p + facet_wrap(~ Task, nrow=3, scales="free")

  ## trim plot whitespace
  p <- p + theme(plot.margin = unit(c(0,0,0,0), "cm"),
                 panel.margin = unit(0.5, "mm"),
                 strip.background = element_rect(fill=NA, ),
                 legend.margin = unit(-0.5, "cm"))

  
  # remove grid background
  p <- p + theme(plot.background = element_blank(),
                 panel.grid.major.y = element_line(colour="grey"),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
  
  
  # change the fonts to Times
  p <- p + theme(text=element_text(family="Times", size=10),
                 axis.text=element_text(family="Times", size=8, colour="black"),
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
  p <- p + geom_point()
  p <- p + xlab('Benchmark')
  p <- p + ylab('Speedup')
  p <- p + facet_wrap(~ Task, nrow=3)
  p <- p + theme(legend.position="top")

  ## trim plot whitespace
  p <- p + theme(plot.margin = unit(c(0,0,0,0), "cm"),
                 panel.margin = unit(0.5, "mm"),
                 strip.background = element_rect(fill=NA, ),
                 legend.margin = unit(-0.5, "cm"))  
  # remove grid background
  p <- p + theme(plot.background = element_blank(),
                 panel.grid.major.y = element_line(colour="grey"),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
  
  # only put breaks at where the cores are used.
  p <- p + scale_x_continuous(breaks=c(2,4,8,16,32))

  # change the fonts to Times
  p <- p + theme(text=element_text(family="Times", size=10),
                 axis.text=element_text(family="Times", size=8, colour="black")
                 )

  ggsave('parallel_speedup.pdf', p, height=15, width=12, units="cm", dpi=600)
}


splits = split_comm_time(results)

splits = summaryBy(TotalTime + Time ~ TimeType + Language + Task + Threads,
  data = splits,
  FUN = list (mean, sd))

print (splits[splits$Language == 'Qs',])

results =
  aggregate(
    cbind(TotalTime, CompTime, CommTime) ~ . ,
    data=results,
    FUN=median)

parallel_speedup_graph(results)

p = parallel_summary_graph(splits)

ggsave('parallel.pdf', p, height=15, width=12, units="cm", dpi=600)

print ("Geometric means (total):")
print (tapply(results$TotalTime, results$Language, geom_mean))
print ("Geometric mean (comp):")
print (tapply(results$CompTime, results$Language, geom_mean))
