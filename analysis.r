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

# The aggregate() function calls puts rows with the same match_cluster together
# the FUN = length part is an aggregation function which in this case counts
# the number of rows for each match_cluster.
# The setorder() function call then sorts by the count that was computed by
# the aggregate call. The reason we are doing setorder() with q1id is that
# we can aggregate basically any field over match_cluster, since we aren't
# doing anything with the field; we are just counting the number of rows.
# Finally, we subset where q1id (which is actually the count for the aggregate)
# is 2.
tmp = subset(
  setorder(
    aggregate(q1id ~ match_cluster, data = mat, FUN = length),
    q1id),
q1id == 2)

# Now get the full data for things with only 2 matches.
# This gets all of the rows of matches, but only where the match_cluster
# is in the tmp we just produced. So we recover all of the original data in mat,
# but for a subset of rows.
tmp2 = subset(mat, match_cluster %in% tmp$match_cluster)
# This sorts by match_cluster, then by class_code, which means it
# puts the pairs of questions in chronological order.
twomat = tmp2[order(tmp2$match_cluster, tmp2$class_code),]

#
# How many times do we see the 2nd use of a question get better?
#
count = 0
for (i in 1:(nrow(tmp)/2)) {
  if (twomat[i*2,]$pct1st_correct < twomat[i*2+1,]$pct1st_correct){
    count = count + 1
  }
}
print(paste('There are ',count,' out of ', nrow(twomat)/2,' row pairs where pct1st_correct goes up',sep=''))

count = 0
for (i in 1:(nrow(tmp)/2)) {
  if (twomat[i*2,]$pct1st_correct < twomat[i*2+1,]$pct1st_correct){
    count = count + 1
  }
}
print(paste('There are ',count,' out of ', nrow(twomat)/2,' row pairs where pct1st_correct goes up',sep=''))
