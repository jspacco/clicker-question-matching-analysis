library(xtable)
library(Hmisc)
library(scales)

course_hash = new.env()

# key of the map have to be strings
course_hash[['1']] = 'UT.CSC108F16-L0101'
course_hash[['2']] = 'UT.CSC108F16-L0102'
course_hash[['3']] = 'UT.CSC108F16-L0104'
course_hash[['4']] = 'KnoxCS141F16-1'
course_hash[['5']] = 'KnoxCS141W17-2'
course_hash[['6']] = 'UIC.CS111S16'
course_hash[['7']] = 'UIC.CS261S17'
course_hash[['8']] = 'UIC.CS361F15'
course_hash[['9']] = 'UIC.CS361S16'
course_hash[['10']] = 'UIC.CS361F16'
course_hash[['11']] = 'UIC.CS361S17'
course_hash[['12']] = 'UIC.CS362F16'
course_hash[['13']] = 'UIC.CS385S16'
course_hash[['14']] = 'UIC.CS385F15'
course_hash[['15']] = 'UIC.CS450F15'
course_hash[['16']] = 'UIC.CS450S17'
course_hash[['17']] = 'KnoxCS142W15'
course_hash[['18']] = 'KnoxCS142S15'
course_hash[['19']] = 'KnoxCS142W16'
course_hash[['20']] = 'KnoxCS142S16'
course_hash[['21']] = 'KnoxCS142W17'
course_hash[['22']] = 'KnoxCS142S17'
course_hash[['23']] = 'UCSD.CSE141F14-A'
course_hash[['24']] = 'UCSD.CSE141F14-B'
course_hash[['25']] = 'UCSD.CSE141F15'
course_hash[['26']] = 'UCSD.CSE141F16'
course_hash[['27']] = 'UCSD.CSE141S17-1'
course_hash[['28']] = 'UCSD.CSE141S17-2'
course_hash[['29']] = 'KnoxCS226F14'
course_hash[['30']] = 'KnoxCS226F15'
course_hash[['31']] = 'KnoxCS214F16'
course_hash[['32']] = 'KnoxCS180S16'
course_hash[['33']] = 'KnoxCS208W17'
course_hash[['34']] = 'KnoxCS309F14'
course_hash[['35']] = 'KnoxCS309W16'

getcourse = function(num) {
  key = as.character(num)
  return(course_hash[[key]])
}

course_type = new.env()
CS1='CS 1'
CS2='CS 2'
COMPORG='Computer Organization'
COMPSYS='Computer Systems'
COMPARCH='Computer Architecture'
MACHORG='Machine Organization'
OS='Operating Systems'
NETWORKS='Networks'
DIGLOGIC='Digital Logic'

course_type[['1']] = CS1
course_type[['2']] = CS1
course_type[['3']] = CS1
course_type[['4']] = CS1
course_type[['5']] = CS1
course_type[['6']] = CS1
course_type[['7']] = MACHORG
course_type[['8']] = COMPSYS
course_type[['9']] = COMPSYS
course_type[['10']] = COMPSYS
course_type[['11']] = COMPSYS
course_type[['12']] = DIGLOGIC
course_type[['13']] = OS
course_type[['14']] = OS
course_type[['15']] = NETWORKS
course_type[['16']] = NETWORKS
course_type[['17']] = CS2
course_type[['18']] = CS2
course_type[['19']] = CS2
course_type[['20']] = CS2
course_type[['21']] = CS2
course_type[['22']] = CS2
course_type[['23']] = COMPARCH
course_type[['24']] = COMPARCH
course_type[['25']] = COMPARCH
course_type[['26']] = COMPARCH
course_type[['27']] = COMPARCH
course_type[['28']] = COMPARCH
course_type[['29']] = 'KnoxCS226F14' # CS 226
course_type[['30']] = 'KnoxCS226F15' # CS 226
course_type[['31']] = 'systems programming' # CS 214
course_type[['32']] = 'KnoxCS180S16'
course_type[['33']] = 'languages'
course_type[['34']] = 'parallel'
course_type[['35']] = 'parallel'

getcoursetype = function(num) {
  return(course_type[[as.character(num)]])
}

# anonymize = function(name) {
#   if (name=='Tong'){
#     return('Alicia')
#   } else if (name=='Peterson'){
#     return('Bob')
#   } else if (name=='Bunde'){
#     return('Carlos')
#   } else if (name=='Kanich'){
#     return('DeAndre')
#   } else if (name=='Porter'){
#     return('Erika')
#   } else if (name=='Taylor'){
#     return('Feng')
#   }
#   return(name)
# }

myplot2 = function(
  df,
  yname,
  xname,
  ylabel,
  xlabel,
  filename,
  main,
  cex=NULL)
{
  lmres = lm(data=df, df[,yname] ~ df[,xname], na.action=na.omit)
  plot(data=df,
       df[,yname] ~ df[,xname],
       ylab=ylabel,
       xlab=xlabel,
       main=main, # append P value and R^2 to the end of the title, if wanted
       cex.main=3,
       cex.axis=3,
       cex.lab=3)
  abline(lmres, col='red')

  summary(lmres)
  pdffilename=paste(filename, 'pdf', sep='.')
  pdfout(pdffilename)
  pngfilename=paste(filename, 'png', sep='.')
  pngout(pngfilename)
}

#
# function for plotting and saving a graph
#
myplot = function(
  frame,
  yname,
  xname,
  ylabel,
  xlabel,
  term,
  figure.location='/Users/jspacco/projects/clickers/sigcse2018/charts',
  filename='chart',
  cex=NULL)
{
  lmres = lm(data=frame, frame[,yname] ~ frame[,xname], na.action=na.omit)
  plot(data=frame,
       frame[,yname] ~ frame[,xname],
       ylab=ylabel,
       xlab=xlabel,
       main=mytitle(term, lmres),
       cex.main=2.0,
       cex.axis=1.8,
       cex.lab=1.8)
  abline(lmres, col='red')

  summary(lmres)
  pdffilename=paste(figure.location, '/', filename, '-', yname, '-', xname, '.pdf', sep='')
  pdfout(pdffilename)
  pngfilename=paste(figure.location, '/', filename, '-', yname, '-', xname, '.png', sep='')
  pngout(pngfilename)
}

mytitle <- function(term, modelobject) {
  # if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  # pval=round(lmp(modelobject), digits=4)
  # rsq=round(lmrsq(modelobject), digits=4)
  # if (pval < 0.001) pval='< 0.001'
  text = modeltotext(modelobject)
  return(paste(term, '\n', text))
}

modeltotext = function(modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  pval=round(lmp(modelobject), digits=4)
  rsq=round(lmrsq(modelobject), digits=4)
  if (pval < 0.001) pval='< 0.001'
  return(paste('p-value', pval, ' | ', "R^2", rsq))
}

pdfpngout = function(filename) {
    dev.copy(pdf, paste(filename, '.pdf', sep=''))
    dev.copy(png, paste(filename, '.png', sep=''))
    dev.off()
}

pngout = function(filename) {
  dev.copy(png, filename)
  dev.off()
}

pdfout = function(filename) {
  dev.copy(pdf, filename)
  dev.off()
}

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

lmrsq <- function(modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  rsq <- summary(modelobject)$r.squared
  return(rsq)
}

qlink = function(row) {
  return(paste('<img width="300" src="https://s3.amazonaws.com/iclickerviewer/',
    row['course_name'], '/Images/', row['class_code'], '_Q',
    trim(row['question_index']), '.jpg"/>',
    sep=''))
}

clink = function(row) {
  return(paste('<img src="https://s3.amazonaws.com/iclickerviewer/',
    row['course_name'], '/Images/', row['class_code'], '_C',
    trim(row['question_index']), '.jpg"/>',
    sep=''))
}

htmltag = function(s, tag){
  return(paste('<', tag, '>', s, '</', tag, '>', sep=''))
}

td = function(s){
  return(htmltag(s, 'td'))
}

tr = function(s){
  return(htmltag(s, 'tr'))
}


tohtmlrow = function(row) {
  res = '<tr>'
  res = paste(res, td(row['instructor']), sep='\n')
  res = paste(res, td(row['course_name']), sep='\n')
  res = paste(res, td(row['class_code']), sep='\n')
  res = paste(res, td(row['pct1st_correct']), sep='\n')
  res = paste(res, td(row['pct2nd_correct']), sep='\n')
  res = paste(res, td(row['num1st_correct']), sep='\n')
  res = paste(res, td(row['num2nd_correct']), sep='\n')
  res = paste(res, td(qlink(row)), sep='\n')
  res = paste(res, td(row['normalized_gain']), sep='\n')
  res = paste(res, td(as.double(row['pct2nd_correct']) - as.double(row['pct1st_correct'])), sep='\n')
  return(paste(res, '</tr>', sep=''))
}

tohtmltable = function(df) {
  tmp = apply(df, 1, tohtmlrow)
  return(paste('<table border=1>', paste(tmp, collapse=''), '</table>', sep='\n'))
}

stringtofile = function(string, filename) {
  fileConn <- file(filename)
  writeLines(string, fileConn)
  close(fileConn)
}

maketable = function(df, filename) {
  s = tohtmltable(df)
  stringtofile(s, filename)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


mybuckets = function(df) {
  res = c()
  for (x in seq(0, .99, by=.1)) {
    tmp = nrow(subset(df, pct1st_correct >= x & pct1st_correct < x+.1)) / nrow(df) * 100
    res = c(tmp, res)
  }
  return(res)
}

#
# HACK
# Leaves things like \foo alone to be output to latex
# from xtable.
#
mysanitize = function(text) {
  # HACK I still don't understand how some functions in R take lists
  #   as parameters.
  # TODO Parse through each word in a string, and leave \foo alone, but
  #   sanitize the rest of the input.
  ifelse(grepl("\\\\", text), text, sanitize(text))
}

mytable = function(df, digits, filename) {
  align = paste('c', 'l', paste(rep('r', length(digits)-1), collapse=''), sep='')
  # align = paste('l', paste(rep('r', length(digits)-1), collapse=''), sep='')
  format = list(sapply(digits,
      function(z) ifelse(z==0, 'd', 'f')
    )
  )
  tmptab = xtable(df,
    align=align,
    latex.environments='center',
    digits=c(0, digits),
#    digits=digits,
    format.args=format
  )
  print(tmptab,
    file=filename,
    comment=FALSE,
    include.rownames=FALSE,
#    sanitize.rownames.function = identity,
    sanitize.text.function = mysanitize,
    floating=FALSE
  )
}

inrange = function(low, hi, val) {
  if (low == 0){
    return(low <= val & val <= hi)
  }
  return(low < val & val <= hi)
}

debugmatrix = function() {
  #
  # Use this when you can't remember how to create a matrix
  # using a grid of rows and columns
  #
  res = c()
  step = 0.2
  x = 0
  for (c in seq(0, .99, step)) {
    for (r in seq(0, .99, step)) {
      tmp = x
      x = x + 1
      res = c(tmp, res)
      #res = c(tmp, res)
    }
  }
  m = matrix(rev(res), ncol=5)
  return(m)
}

checkpct = function(df, r, c) {
  step = 0.2
  return(
    round(nrow(
      subset(df,
        inrange(c, c+step, pct1st_correct) &
        inrange(r, r+step, pct2nd_correct)
      )
    ) / nrow(df) * 100, 1)
  )
}
colpct = function(df, colnum, precision=2) {
  df[colnum] = percent(t(round(df[colnum], precision)))
  return(df)
}

#
# Based on formula from Marx and Cummings
#
gain = function(pre, post) {
  ifelse(post > pre, (post - pre) / (1 - pre), (post - pre) / pre)
}
