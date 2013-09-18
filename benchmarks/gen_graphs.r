#!/usr/bin/Rscript

library(ggplot2)

args = commandArgs(trailingOnly = TRUE)
csv_file = paste(args[1], '_results.csv', sep="")
pdf_file = paste(args[1], '.pdf', sep="")
print(args)
print(csv_file)
print(pdf_file)

results = read.csv(csv_file)

tasks = unique(results$Task)
langs = unique(results$Language)

p <- ggplot(results, aes(x=Language, y=Time, fill=Language))
p <- p + geom_bar(stat="identity", colour="black")
p <- p + scale_fill_brewer()
p + facet_wrap(~ Task, scales="free_y")

ggsave(pdf_file)
