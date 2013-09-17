#!/usr/bin/Rscript

library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
print(args)
results = read.csv(paste(args[1], '_results.csv', sep=""))

tasks = unique(results$Task)
langs = unique(results$Language)

p <- ggplot(results, aes(x=Language, y=Time, fill=Language))
p <- p + geom_bar(stat="identity", colour="black")
p <- p + scale_fill_brewer()
## p <- p + scale_fill_hue(c=c(50,100))
p + facet_wrap(~ Task, scales="free_y")
ggsave(paste(args[1], '.pdf', sep="")
