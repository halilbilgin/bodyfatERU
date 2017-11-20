for(fm in fms$`2`){
  cat(gsub('+', '\n', strsplit(gsub(' ', '', paste(format(fm), collapse='')), '~')[[1]][2], fixed=T))
  cat('\n\n')
}
names(fms$`1`)
