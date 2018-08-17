source('utils.r')
library(data.table)
library(ggplot2)
library(xtable)
require(plyr)
# source(gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz')))

"
R Notes:
* Can't get R to run through the Makefile. Not sure why. Probably somethign to do
  with the interactivity of the terminal or something like that.
"

#
# Anonymize for conference submission?
#
ANONYMOUS = FALSE

# Should we use the original normalized gain formula?
# FIXME I think this variable doesn't do anything anymore
ORIG_NG = FALSE

#
# Turn on error traceback
#
options(error=traceback)

# Where to output charts and graphs
outdir = '/Users/jspacco/projects/clickers/clicker-question-matching-analysis'
# Helper function to build a the path /path/to/outdir/path
mydir = function(path) {
  return(paste(outdir, '/', path, sep=''))
}
charts = function(filename) mydir(paste('charts', filename, sep='/'))
chart.location = mydir('charts')
EASY = 0.70
HARD = 0.35


"---
question_types are
0: unknown
1: quiz
2: single
3: paired
4: non-MCQ
5: error
"

#
# Read data into data frame qs (short of "questions")
# and then put only the matches into data frame mat
#
filename = 'courses.csv'
qs = read.csv(filename)
mat = subset(qs, match_cluster != -1)

# FIXME figure out course type
# add a column for the type of course (i.e. cs1, cs2, etc)
# qs = cbind(qs, course_type=apply(qs[1], 1, getcoursetype))

# FIXME figure out anonymous instructors for new data set
if (ANONYMOUS){
  qs$instructor = sapply(qs$instructor, function(name) {
    if (name=='Tong'){
      return('\\tong')
    } else if (name=='Petersen'){
      return('\\petersen')
    } else if (name=='Bunde'){
      return('\\bunde')
    } else if (name=='Kanich'){
      return('\\kanich')
    } else if (name=='Porter'){
      return('\\porter')
    } else if (name=='Taylor'){
      return('\\taylor')
    } else if (name=='Dema'){
      return('\\dema')
    }
    return(name)
  })
}

paired =
  subset(qs, question_type == 3 & num_correct_answers == 1)
# HACK Fix NG here until we fix webapp.
# NG only makes sense for paired questions, so this is where we set it.
paired$normalized_gain = gain(paired$pct1st_correct, paired$pct2nd_correct)
paired$raw_gain = paired$pct2nd_correct - paired$pct1st_correct

# Paired questions with more than 1 correct answer
paired_gt1c =
  subset(qs, question_type == 3 & num_correct_answers > 1)
# Single questions with 1 correct answer
single =
  subset(qs, question_type == 2 & num_correct_answers == 1)
# Single questions with more than 1 correct answer
single_gt1c =
  subset(qs, question_type == 2 & num_correct_answers > 1)
# The non-multiple choice questions
nonmcq =
  subset(qs, question_type == 4)
# Only quiz questions
quiz =
  subset(qs, question_type == 1)
# Only non-quiz questions
nonquiz =
  subset(qs, question_type != 1)
# All paired-plus-single questions
paired_plus_single =
  subset(qs, num_correct_answers == 1 & (question_type == 2 | question_type == 3))
# Only paired questions with a match and 1 correcta nswer
paired_mat =
  subset(mat, num_correct_answers == 1 & question_type == 3)
# Matching questions with 1 correct answer and exactly 2 matches
# FIXME figure out how to handle matches with more than 2 matches
tmp = subset(
  aggregate(q1id ~ match_cluster),
    data = subset(mat, num_correct_answers == 1)


# OK, now finally figure out some things
tmp = subset(
  # get the questions with exactly 2 matches
  setorder(
    aggregate(q1id ~ match_cluster, data = subset(qs, match_cluster != -1), FUN = length),
  q1id),
q1id == 2)
# now get the full data for things with only 2 matches
subset(mat, match_cluster %in% tmp$match_cluster)
# OK, now compare the 1st and 2nd questions for all of the pairs of questions
tmp3 = tmp2[order(tmp2$match_cluster, tmp2$class_code),]
for (i in 1:(nrow(tmp3)/2)) {

}
