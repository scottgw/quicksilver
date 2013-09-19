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

## drop the total time column
results = subset(results, select = -c(TotalTime))

## melt them so that we get the comp and comm times as a column 
results = melt(results, id=(c("Language", "Task", "Threads")))

#rename them because melt has given them bad names
names(results)[names(results) == 'variable'] = 'TimeType'
names(results)[names(results) == 'value'] = 'Time'

p <- ggplot(results, aes(x=Language, y=Time, fill=TimeType))
p <- p + geom_bar(stat="identity", colour="black")
p <- p + scale_fill_brewer()
p <- p + facet_wrap(~ Task, scales="free_y")

ggsave(pdf_file, p)
