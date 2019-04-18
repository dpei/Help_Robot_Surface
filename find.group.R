# clean env
rm(list = setdiff(ls(), c("X_train", "X_test", "y_train")))


if(!exists(c("X_train"))){
  X_train <- read.csv("X_train.csv")
}
if(!exists(c("y_train"))){
  y_train <- read.csv("y_train.csv")
}
if(!exists(c("X_test"))){
  X_test <- read.csv("X_test.csv")
}


# helper function for below two
subset.df <- function(input.df, id.vec.tf, cols){
  
  head.n.tail.tf <- input.df$measurement_number %in% c(0,127)
  
  # subset
  df <- input.df[id.vec.tf & head.n.tail.tf, ]
  # add new column
  df$position <- gsub("[0-9]*_","", df$row_id)
  df$series.id <- gsub("_[0-9]*","", df$row_id)
  
  # order
  df.ordered <- df[order(df$orientation_X), cols]
  
  return(df.ordered)
}

# look ar one specific group_id in train
# Given a group id number 
# return dataframe contains all series in train along with label
get.df.train <- function(raw.df = X_train, group.id.vec, 
                         cols = c("row_id", "orientation_X", "orientation_Y", 
                                  "orientation_Z", "orientation_W","position", "series.id")){
  
  group_id.ind <- match(raw.df$series_id, y_train$series_id)
  raw.df$group_id <- y_train$group_id[group_id.ind]
  
  # T/F  
  id.vec.tf <- raw.df$group_id %in% group.id.vec
  
  df.ordered <- subset.df(raw.df, id.vec.tf, cols)
  return(df.ordered)
}


# look ar one specific series id in test
get.df.test <- function(raw.df = X_test, series.id.vec, cols = c("row_id", "orientation_X", "orientation_Y", 
                                                 "orientation_Z","orientation_W", "position", "series.id")){
  # T/F  
  id.vec.tf <- raw.df$series_id %in% series.id.vec
  
  df.ordered <- subset.df(raw.df, id.vec.tf, cols)
  return(df.ordered)
}


# Given a series id, find the series id that has its series_0 most close to the given series_127
# If no id in the data frame then return -1 
# this is one step
find.nearest <- function(one.series.id, input.df, downstream = TRUE){
  
  # get series value
  series.tf <- input.df$series.id == one.series.id
  
  # get the 127 position value
  if(downstream){
    position.tf <- input.df$position == 127
  } else if (!downstream){
    position.tf <- input.df$position == 0  
  }
  
  # Pairwise.diff.X = a pair of row has difference
  rank <- which(series.tf & position.tf)
  
  broader.rank <- (rank-6) : (rank+6)
  potential.rows.ind <- intersect(broader.rank, which(!position.tf))
  
  if(length(potential.rows.ind) == 0){
    return(list(NULL, 999))
  }
  
  query.df <- input.df[rep(rank, length(potential.rows.ind)), 2:5]
  difference <- abs((query.df - input.df[potential.rows.ind, 2:5]))
  difference[,3:4] <- difference[,3:4]/0.15
  difference.sum <- apply(difference, 1, sum)
  min.row.ind <- order(difference.sum)[1]
  min.difference <- difference.sum[min.row.ind]
  
  ret.series.id <- input.df[potential.rows.ind[min.row.ind], "series.id"]  
  if (ret.series.id == one.series.id){
    return(list(NULL, 999))
  }
  return(list(as.numeric(ret.series.id), min.difference))
}




# find.group is a function to calculate iteratively series ids in one chained group.
# this uses the find.nearest() function to find steps in one group
# start series id is not necessarily the start of a group
get.chained.group <- function(start.series.id, input.df, threshold = 0.0001){
  chained.group <- c(start.series.id)
  query.id <- start.series.id
  next.id.error <- 0
  start.series.df <- input.df[input.df$series.id == start.series.id, ]
  
  for (direction in c(TRUE, FALSE)){
    
    while(next.id.error < threshold & !is.null(query.id)){
      # get both error and result series id
      next.series <- find.nearest(query.id, input.df, downstream = direction)
      
      next.id <- next.series[[1]]
      next.id.error <- next.series[[2]]
      
      if(direction){
        chained.group <- c(chained.group, next.id)  
      } else if (!direction){
        chained.group <- c(next.id, chained.group)
      }
      
      # slice out the query id rows
      input.df <- input.df[!input.df$series.id %in% query.id,]
      # update query id
      query.id <- next.id
    }
    # after looking at downstream, before look at upstream, I should 
    # reset some values below
    if(direction){
      input.df <- rbind(start.series.df, input.df)
      next.id.error <- 0
      query.id <- start.series.id
    }
      
  }
  # return updated (sliced) input.df
  return(list(chained.group, input.df))
}

# get.chained.group(2973, df, 0.01)[[1]] == c(3635, 3256, 3806, 2973, 3643, 2902)



# define a input.df first
# input df should have xyz 
# use a loop to find good chained groups iteratively
find.all.group <- function(df.input, threshold = 0.0001){
  # result should be a list, num length is the number of groups, vector in each 
  # position is a vector containing series id
  # depending on number of groups may get more of number 100
  result <- list()
  
  i <- 1
  chained.groups.ind <- c()
  while(nrow(df.input)>10){
    first.position <- max(which(df.input$position == 0))
    first.series.id <- as.numeric(df.input$series.id[first.position])
    one.chained.group <- get.chained.group(first.series.id, df.input, threshold = threshold)
    
    result <- c(result, list(one.chained.group[[1]]))
    if(length(result[[i]]) > 5){
      chained.groups.ind <- c(chained.groups.ind, i)
    }
    # update df and i
    keep <- !one.chained.group[[2]]$series.id %in% one.chained.group[[1]]
    df.input <- one.chained.group[[2]][keep,]
    
    print(dim(df.input)[1])
    i <- i + 1
  }
  
  return(list(result, chained.groups.ind))
}

# tester
test.find.nearest <- function(){
  df <- get.df.train(X_train, c(67))
  tf.1 <- find.nearest(3635, df)[[1]] == 3256
  tf.2 <- find.nearest(3256, df)[[1]] == 3806
  tf.3 <- find.nearest(2973, df)[[1]] == 3643
  if(tf.1 & tf.2 &tf.3){
    print("passed test find nearest")
  } else {
    print("failed test find nearest!!!!")
  }
}



# use find all group function result (a list ) as inpute
# print the percentage of chained group
# print the successfully chained group
# nothing being returned
check.result <- function(find.group.result, df, train = TRUE){
  cat("Here are group id: ", find.group.result[[2]], "\n")
  cat("frequency table for length: \n")
  print(table(sapply(find.group.result[[1]], length)))
  print(sum(sapply(find.group.result[[1]], length)))
  
  
  # calculate how many series get a group vs not getting a group
  chained.group.length <- sapply(find.group.result[[1]][find.group.result[[2]]], length)
  total.chained.series.num <- sum(chained.group.length)
  chained.percentage <- total.chained.series.num *2  / nrow(df) *100
  
  # print some result
  percentage <- paste("There are", format(round(chained.percentage, 2), nsmall = 2), "% of series among",  nrow(df)/2, "series getting into chained group.", "\n\n")
  cat(percentage)
  
  
  if(train){
    # Among those grouped, how many of them get corrected check correctness 
    y_train <- read.csv("y_train.csv")
    # evaluate
    correct.num <- 0
    for (i in find.group.result[[2]]){
      surface.in.chain <- y_train$surface[y_train$series_id %in% find.group.result[[1]][[i]]]
      unique.tf <- length(unique(surface.in.chain)) == 1   
      
      if(!unique.tf){
        correct.num <- correct.num + max(table(surface.in.chain))
      } else {
        correct.num <- correct.num + length(surface.in.chain)
      }
    }
    correctness <- paste("Among them,", format(round((correct.num/total.chained.series.num*100), 2), nsmall = 2), "% of the labels are correct")
    cat(correctness)
  }
}

#############################
#############################
# Train data
#############################
#############################

# some testers
test.find.nearest()

# get a data frame can be used for find.all.group() function.
df.train <- get.df.train(X_train, 0:72)
# make some checks 
if(all(dim(df.train)[2] == c(7))){
  print("passed test")
} else {
  print("failed test!!!")
}


all.chained.groups <- find.all.group(df.train, threshold = 0.005)
# check result
check.result(all.chained.groups, df.train)



#############################
#############################
# Test data
#############################
#############################

# preprocess test data
df.test <- get.df.test(X_test, 0:(nrow(X_test)/128))
# get chains
all.chained.groups.test <- find.all.group(df.test, threshold = 0.005)
# checks
check.result(all.chained.groups.test, df.test, train = FALSE)


# readin and modify submission file 
# edit some result based on the all.chained.groups.test
submission <- read.csv("best_submission_edit.csv")
submission2 <- submission

percent.pool <- NULL
# for (i in 1:length(all.chained.groups.test[[2]])){
for (i in all.chained.groups.test[[2]]){
  one.chained.group.tf <- submission$series_id %in% all.chained.groups.test[[1]][[i]]
  current.surface <- submission$surface[one.chained.group.tf]
  stat.table <- table(current.surface)
  
  percentage <- (sum(stat.table) - max(stat.table)) / sum(stat.table) *100
  percent.pool <- c(percent.pool, percentage)
  message <- paste("Percent of class changed: ", percentage, "\n", sep = "")
  cat(message)
  
  most.common.surface <- names(stat.table)[order(stat.table, decreasing = TRUE)][1]
  if(percentage < 40){
    submission$surface[one.chained.group.tf] <- most.common.surface  
  }
  
}

table(submission2$surface == submission$surface)[1]
write.csv(submission, "Apr6_2.csv", row.names = FALSE, quote = FALSE)
