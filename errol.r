#
# To read the entire R file
#

mat = read.csv('matches.csv')

mat$ngdiff = mat$normalized_gain.2 - mat$normalized_gain.1

z = nrow(subset(mat, ngdiff > 0))

print(paste('the number is', z, 'and I think that is great'))

#
# I don't remember the ~ thing. Lots of Google.
# ngdiff ~ match_cluster * instructor
# Other useful aggregate:
# max, min, mean, median, length
#
ag1 = aggregate(ngdiff ~ match_cluster, data = mat, FUN = mean)

ag2 = aggregate(ngdiff ~ match_cluster, data = mat, FUN = length)

# merging
# also can rename columns when doing the merge
m1 = merge(ag1, ag2, by = "match_cluster")

# size of different match clusters
# using match_type because we aren't really applying the aggregate function to match_type
aggregate(match_type ~ match_cluster, data = mat, FUN = length)
