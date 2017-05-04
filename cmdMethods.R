#!/usr/bin/env Rscript
library("optparse")
source('methodsTest.R')
allMethods <- read.csv('allMethods.txt')
option_list = list(
  make_option(c("-m", "--methods"), type="character", default="1:3"),
  make_option(c("-s", "--seeds"), type="character", default="1:50")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
methods <- allMethods[eval(parse(text = opt$methods)), 1]
seeds <- eval(parse(text=opt$seeds))

methodsTest(methods, seeds, paste(opt$methods,'-',opt$seeds, sep = ''))