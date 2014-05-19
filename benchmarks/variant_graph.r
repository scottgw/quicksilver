#!/usr/bin/Rscript

library(plyr)
library(ggplot2)
library(reshape)
library(doBy) ## For summary statistics
library(grid) ## for 'unit' used in legend.key.size theme setting
library(xtable) ## For outputing LaTeX tables

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
  p <- ggplot(df, aes(x=Variant, y=Time.mean, fill=TimeType))

  p <- p + geom_bar(stat="identity")
  
  p <- p + geom_errorbar(aes(ymin=Time.mean - Time.sd,
                             ymax=Time.mean + Time.sd),
                         width=0.25,
                         position=position_dodge(.9))
  
  p <- p + scale_fill_brewer()
  p <- p + xlab('Optimization')

  if (parallel_data)
    {
      p <- p + scale_y_log10()
      p <- p + ylab('Time (normalized)')
    }
  else
    {
      p <- p + ylab('Time (s)')
    }

  p <- p + scale_fill_manual(values=c("dodgerblue3", "cornflowerblue"),
                             breaks=c("CompTime", "CommTime"),
                             labels=c("Computation time",
                               "Communication time"))

  p <- p + theme(legend.position="none")
  p <- p + guides(fill=guide_legend(title=""))
  p <- p + facet_wrap(~ Task, nrow = 3, scales="free")

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
  
  
  # change the fonts to Times
  p <- p + theme(text=element_text(family="Times", size=10, colour="black"),
                 axis.text=element_text(family="Times", size=8, colour="black"))
  
  return (p)
}

splits = split_comm_time(results)
splits = summaryBy(Time ~ TimeType + Variant + Task,
  data = splits,
  FUN = list (mean, sd))

splits = subset(splits, splits$Variant %in% c("None", "Dyn.", "Static", "QoQ", "All"))

par_tasks = c("randmat", "thresh", "winnow", "outer", "product", "chain")

parallel = subset(splits, splits$Task %in% par_tasks)
concurrent = subset(splits, splits$Task %in% c("mutex", "prodcons", "condition", "threadring", "chameneos"))


## normalize the CommTime for each parallel task
for (t in par_tasks)
  {
    s = parallel$Task == t & parallel$TimeType == 'CommTime'
    parallel[s,]$Time.mean <- parallel[s,]$Time.mean /
      min(parallel[s,]$Time.mean)
  }
parallel = subset(parallel, parallel$TimeType %in% c("CommTime"))
## print (xtable(cast(parallel, Task ~ Variant, value='Time.mean')))
p = variant_graph(parallel, TRUE)
ggsave('variant_parallel.pdf', p, height=10, width=12, units="cm")
print ("finished parallel saving")
concurrent = subset(concurrent, concurrent$TimeType %in% c("CompTime"))

## print (xtable(cast(concurrent, Task ~ Variant, value='Time.mean')))
p = variant_graph(concurrent, FALSE)
ggsave('variant_concurrent.pdf', p, height=10, width=12, units="cm")

print ("Geometric means (total):")
print (tapply(results$TotalTime, results$Variant, geom_mean))
print ("Geometric mean (comp):")
print (tapply(results$CompTime, results$Variant, geom_mean))
