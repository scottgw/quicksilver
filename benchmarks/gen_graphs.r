#!/usr/bin/Rscript

library(ggplot2)
library(reshape)

args = commandArgs(trailingOnly = TRUE)
csv_file = paste(args[1], '_results.csv', sep="")
pdf_file = paste(args[1], '.pdf', sep="")
print(args)
print(csv_file)
print(pdf_file)

results = read.csv(csv_file)

tasks = unique(results$Task)
langs = unique(results$Language)

results$CommTime = results$TotalTime - results$CompTime
## results = results[results$Task != "chameneos",]

## drop the total time column
split_results = subset(results, select = -c(TotalTime))

## melt them so that we get the comp and comm times as a column 
split_results = melt(split_results, id=(c("Language", "Task", "Threads")))

#rename them because melt has given them bad names
names(split_results)[names(split_results) == 'variable'] = 'TimeType'
names(split_results)[names(split_results) == 'value'] = 'Time'

p <- ggplot(split_results, aes(x=Language, y=Time, fill=TimeType))
p <- p + geom_bar(stat="identity", colour="black")
p <- p + scale_fill_brewer()
p <- p + facet_wrap(~ Task, scales="free_y")

ggsave(pdf_file, p, width=9)

geom_mean = function (x) {
  return (exp(mean(log(x))))
}

print ("Geometric means:")
print (tapply(results$TotalTime, results$Language, geom_mean))
