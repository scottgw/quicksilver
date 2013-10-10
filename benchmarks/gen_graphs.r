#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)

args = commandArgs(trailingOnly = TRUE)
run_type = args[1]
csv_file = args[2]

## print(args)
## print(run_type)
## print(csv_file)

pdf_file = paste(run_type, '.pdf', sep="")

results = read.csv(csv_file)


if ("Language" %in% names(results)) {
  levels(results$Language) <- c(levels(results$Language), 'SCOOP')
  results$Language[results$Language == 'qs'] <- 'SCOOP'
} else if ("Variant" %in% names(results)) {
  levels(results$Variant) <- c(levels(results$Variant), 'QoQ',
                               'Sync', 'Both')
  results$Variant[results$Variant == 'nolift'] <- 'QoQ'
  results$Variant[results$Variant == 'noqoq'] <- 'Sync'
  results$Variant[results$Variant == 'norm'] <- 'Both'
  results <- droplevels(results)
}

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
  ## geomeans = tapply(df$TotalTime, df$Language, geom_mean)

  ## for (lang in names(geomeans))
  ##   {
  ##     time = geomeans[lang]
  ##     row =
  ##       data.frame(Task = "geometric mean",
  ##                  Language = lang,
  ##                  Threads = 4,
  ##                  TotalTime = time,
  ##                  CompTime = time,
  ##                  CommTime = 0)
  ##     df = rbind(df, row)
  ##   }

  p <- ggplot(df, aes(x=Language, y=TotalTime))
  p <- p + scale_fill_brewer()
  p <- p + geom_bar(stat="identity", fill="dodgerblue3", colour="black")
  p <- p + xlab('Benchmark')
  p <- p + ylab('Time (s)')
  p <- p + facet_wrap(~ Task, scales="free_y")
  return (p)
}

parallel_summary_graph = function (df)
{
  m = max(df$Threads)

  df = splits[df$Threads == m,]

  p <- ggplot(df, aes(x=Language, y=Time, fill=TimeType))
  p <- p + geom_bar(stat="identity", colour="black")
  p <- p + scale_fill_manual(values=c("dodgerblue3", "cornflowerblue"),
                             breaks=c("CompTime", "CommTime"),
                             labels=c("Computation time",
                               "Communication time"))
  p <- p + guides(fill=guide_legend(title=""))
  p <- p + theme(legend.position = "top")
  p <- p + facet_wrap(~ Task, scales="free_y")
  return (p)
}

parallel_speedup_graph = function (df)
{
  singleCore = df[df$Threads == 1,]
  df =
    merge(x = df,
          y = singleCore,
          by=c("Language", "Task"),
          all=TRUE)
  df = subset(df, select = -c(Threads.y))
  df$TotalScale = df$TotalTime.y / df$TotalTime.x
  df$CompScale = df$CompTime.y / df$CompTime.x

  p <- ggplot(df,
              aes(x=Threads.x, y=TotalScale, group=Language, shape=Language))
  p <- p + geom_line(stat="identity",
                     aes(x=Threads.x,
                         y=TotalScale,
                         colour=Language,
                         shape=Language))
  p <- p + geom_point()
  p <- p + xlab('Benchmark')
  p <- p + ylab('Time (s)')
  p <- p + facet_wrap(~ Task, scales="free_y")

  ggsave('parallel_total_speedup.pdf', p, width=9)

  p <- ggplot(df, aes(x=Threads.x, y=CompScale, group=Language, shape=Language))
  p <- p + geom_line(stat="identity",
                     aes(x=Threads.x,
                         y=CompScale,
                         colour=Language,
                         shape=Language))
  p <- p + geom_point()
  p <- p + xlab('Benchmark')
  p <- p + ylab('Time (s)')
  p <- p + facet_wrap(~ Task, scales="free_y")

  ggsave('parallel_computation_speedup.pdf', p, width=9) 
}

variant_graph = function (df, norm_df) {
  ## geomeans = tapply(norm_df$TotalTime, norm_df$Variant, geom_mean)

  ## for (var in names(geomeans))
  ##   {
  ##     time = geomeans[var]
  ##     row1 =
  ##       data.frame(Task = "geometric mean",
  ##                  Variant = var,
  ##                  TimeType = "CompTime",
  ##                  Time = time)
  ##     row2 =
  ##       data.frame(Task = "geometric mean",
  ##                  Variant = var,
  ##                  TimeType = "CommTime",
  ##                  Time = 0)
  ##     df = rbind(df, row1)
  ##     df = rbind(df, row2)
  ##   }
  
  p <- ggplot(df, aes(x=Variant, y=Time, fill=TimeType))
  p <- p + geom_bar(stat="identity", colour="black")
  p <- p + scale_fill_brewer()
  p <- p + xlab('Optimization')
  p <- p + ylab('Time (s)')
  p <- p + scale_fill_manual(values=c("dodgerblue3", "cornflowerblue"),
                             breaks=c("CompTime", "CommTime"),
                             labels=c("Computation time",
                               "Communication time"))
  p <- p + theme(legend.position = "top")
  p <- p + guides(fill=guide_legend(title=""))
  p <- p + facet_wrap(~ Task, scales="free_y")

  return (p)
}

if (run_type == 'concurrent') {
  p = concurrent_graph(results)
} else if (run_type == 'parallel') {
  splits = split_comm_time(results)
  parallel_speedup_graph(results)

  p = parallel_summary_graph(splits)
} else if (run_type == 'variant') {
  splits = split_comm_time(results)
  p = variant_graph(splits, results)
}

ggsave(pdf_file, p, width=9)

if (run_type != 'variant'){
  print ("Geometric means (total):")
  print (tapply(results$TotalTime, results$Language, geom_mean))
  print ("Geometric mean (comp):")
  print (tapply(results$CompTime, results$Language, geom_mean))
} else {
  print ("Geometric means (total):")
  print (tapply(results$TotalTime, results$Variant, geom_mean))
  print ("Geometric mean (comp):")
  print (tapply(results$CompTime, results$Variant, geom_mean))
}
