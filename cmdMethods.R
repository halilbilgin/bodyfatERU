#!/usr/bin/env Rscript
library("optparse")
source('methodsTest.R')
allMethods <- read.csv('allMethods.txt')
option_list = list(
  make_option(c("-m", "--methods"), type="character", default="glm"),
  make_option(c("-c", "--cores"), type='numeric', default=1)
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
library(doParallel)
registerDoParallel(cores=opt$cores)
#methods <- allMethods[eval(parse(text = opt$methods)), 1]
#seeds <- eval(parse(text=opt$seeds))

methodsTest(strsplit(opt$methods, '-'), fileSuffix=opt$methods)
