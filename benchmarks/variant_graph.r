#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)
library(doBy) # For summary statistics

args = commandArgs(trailingOnly = TRUE)
csv_file = args[1]

## print(args)
## print(run_type)
## print(csv_file)

pdf_file = 'variant.pdf'

results = read.csv(csv_file)


levels(results$Variant) <- c(levels(results$Variant),
                             'None', 'Dyn.', 'Static', 'QoQ', 'All')
results$Variant[results$Variant == '___'] <- 'None'
results$Variant[results$Variant == 'd__'] <- 'Dyn.'
results$Variant[results$Variant == '_s_'] <- 'Static'
results$Variant[results$Variant == '__q'] <- 'QoQ'
results$Variant[results$Variant == 'dsq'] <- 'All'
results <- droplevels(results)

results$CommTime = results$TotalTime - results$CompTime

non_time_names = setdiff (names(results), c("TotalTime", "CompTime", "CommTime"))

## print (summaryBy(TotalTime + CompTime + CommTime ~ Variant + Task,
##                  data = results,
##                  FUN = function (x) { summary(x) }))

## ## Aggregate all the timing columns by the median value.
## # Task + Language + Threads,
## results =
##   aggregate(
##     cbind(TotalTime, CompTime, CommTime) ~ . ,
##     data=results,
##     FUN=median)



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

variant_graph = function (df, parallel_data) {
  if (parallel_data)
    {
      df = subset(df, df$TimeType %in% c("CommTime"))
      ## df$Time.FUN3 = df$Time.FUN3 * 10
    }
  
  p <- ggplot(df, aes(x=Variant, y=Time.FUN3, fill="TimeType"))
  # The combination of these two puts the colour down, but then
  # removes the black crossbar from the legend.
  p <- p + geom_bar(stat="identity")
  ## p <- p + geom_bar(stat="identity", colour="black", show_guide=FALSE)
  ## p <- p + geom_errorbar(aes(ymin=Time.FUN1, ymax=Time.FUN5),
  ##                        position=position_dodge(.9))

  p <- p + scale_fill_brewer()
  p <- p + xlab('Optimization')
  p <- p + ylab('Time (s)')

  if (parallel_data)
    {
      p <- p + scale_y_log10()
    }

  p <- p + scale_fill_manual(values=c("dodgerblue3", "cornflowerblue"),
                             breaks=c("CompTime", "CommTime"),
                             labels=c("Computation time",
                               "Communication time"))

  p <- p + theme(legend.position="none")
  p <- p + guides(fill=guide_legend(title=""))
  p <- p + facet_grid(~ Task, scales="free")
  
  # change the fonts to Times
  p <- p + theme(text=element_text(family="Times", size=8))
  
  return (p)
}

splits = split_comm_time(results)

splits = summaryBy(Time ~ TimeType + Variant + Task,
  data = splits,
  FUN = function (x) {return (quantile(x, names=FALSE))})

splits = subset(splits, splits$Variant %in% c("None", "Dyn.", "Static", "QoQ", "All"))

par_tasks = c("randmat", "thresh", "winnow", "outer", "product", "chain")

parallel = subset(splits, splits$Task %in% par_tasks)
concurrent = subset(splits, splits$Task %in% c("mutex", "prodcons", "condition", "threadring", "chameneos"))


# normalize the CommTime for each parallel task
for (t in par_tasks)
  {
    s = parallel$Task == t & parallel$TimeType == 'CommTime'
    parallel[s,]$Time.FUN3 <- parallel[s,]$Time.FUN3 /
      min(parallel[s,]$Time.FUN3)
  }

p = variant_graph(parallel, TRUE)
ggsave('variant_parallel.pdf', p, height=4.5, width=18, units="cm")

p = variant_graph(concurrent, FALSE)
ggsave('variant_concurrent.pdf', p, height=4.5, width = 18, units="cm")

print ("Geometric means (total):")
print (tapply(results$TotalTime, results$Variant, geom_mean))
print ("Geometric mean (comp):")
print (tapply(results$CompTime, results$Variant, geom_mean))
