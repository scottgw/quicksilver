#!/usr/bin/Rscript

library(ggplot2)
library(reshape)

args = commandArgs(trailingOnly = TRUE)
run_type = args[1]
csv_file = paste(run_type, '_results.csv', sep="")
pdf_file = paste(run_type, '.pdf', sep="")

results = read.csv(csv_file)

tasks = unique(results$Task)
langs = unique(results$Language)

results$CommTime = results$TotalTime - results$CompTime
## Aggregate all the timing columns by the median value.
results =
  aggregate(
    cbind(TotalTime, CompTime, CommTime)  ~ Task + Language + Threads,
    data=results,
    FUN=median)
## print(names(results))

split_comm_time = function (df)
{
  ## drop the total time column
  split_df = subset(df, select = -c(TotalTime))

  ## melt them so that we get the comp and comm times as a column 
  split_df = melt(split_df, id=(c("Language", "Task", "Threads")))

  ## rename them because melt has given them bad names
  names(split_df)[names(split_df) == 'variable'] = 'TimeType'
  names(split_df)[names(split_df) == 'value'] = 'Time'
  return (split_df)
}

concurrent_graph = function (df)
{
  p <- ggplot(df, aes(x=Language, y=Time, fill=Language))
  p <- p + geom_bar(stat="identity")
  p <- p + scale_fill_brewer()
  p <- p + facet_wrap(~ Task, scales="free_y")
  return (p)
}

parallel_summary_graph = function (df)
{
  m = max(df$Threads)
  df = splits[df$Threads == m,]
  p <- ggplot(df, aes(x=Language, y=Time, fill=TimeType))
  p <- p + geom_bar(stat="identity", colour="black")
  p <- p + scale_fill_brewer()
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

  p <- ggplot(df, aes(x=Threads.x, y=TotalScale, group=Language))
  p <- p + geom_line(stat="identity", aes(x=Threads.x, y=TotalScale, colour=Language))
  p <- p + facet_wrap(~ Task, scales="free_y")

  ggsave('parallel_total_speedup.pdf', p, width=9)


  p <- ggplot(df, aes(x=Threads.x, y=CompScale, group=Language))
  p <- p + geom_line(stat="identity", aes(x=Threads.x, y=CompScale, colour=Language))
  p <- p + facet_wrap(~ Task, scales="free_y")

  ggsave('parallel_computation_speedup.pdf', p, width=9) 
}


if (run_type == 'concurrent') {
  p = concurrent_graph(results)
} else {
  splits = split_comm_time(results)
  parallel_speedup_graph(results)

  p = parallel_summary_graph(splits)
}

ggsave(pdf_file, p, width=9)

geom_mean = function (x) {
  return (exp(mean(log(x))))
}

print ("Geometric means (total):")
print (tapply(results$TotalTime, results$Language, geom_mean))

print ("Geometric mean (comp):")
print (tapply(results$CompTime, results$Language, geom_mean))
