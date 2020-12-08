##########################################################################################
################################# Part 1##################################################
##########################################################################################

### Prepping data
## Let's make our matrix of seats..
seats <- matrix(data = 0, nrow = 128, ncol = 8)
head(seats, 10)

## And load in our input data
boarding_passes <- read.delim("task_5_data.txt", header = FALSE)

### We'll need some input-variables, such as the maximum number of rows and columns of our seating-matrix.
max_row_num <- nrow(seats)
max_col_num <- ncol(seats)

##### Making a function to loop through the first 7 elements of the boarding pass and find our row.
#### Defining the function
row_finder <- function(boarding_pass, max_row_tracker){
  ### First we'll make a sequence from 1 (R starts with 1) to the maximum row number (which will later be altered, unless this is a full "B"-sequence)
  row_seq <- 1:(max_row_tracker)
  ### For each of the 7 row-indicators in the boarding pass..
  for(i in 1:7){
    ## ..take the first element from the boarding pass string.
    sub_str <- substring(boarding_pass, i, i)
    ## If it be an F...
    if(sub_str == "F"){
      # ...divide our maximum row_number by half...
      max_row_tracker <- max_row_tracker / 2
      # Then SUBTRACT that from the maximum of our sequence of rows and, from that, make a sequence from the minimal row up to the cut maximum.
      row_seq <- seq(min(row_seq), max(row_seq) - max_row_tracker)
    }
    ## If it be a B (hehe)...
    if(sub_str == "B"){
      # ...divide our maximum row_number by half...
      max_row_tracker <- max_row_tracker / 2
      # Then ADD that to the minimum of our sequence of rows and, from that, make a sequence from the newly boosted minimal row up to the maximum.
      row_seq <- seq((min(row_seq) + max_row_tracker), max(row_seq))
    }
  }
  ### Give us back our row number minus 1, since R starts with the number 1, 'cause it's the number one programming language(...?). 
  return(row_seq - 1)
}

##### Now to make a second function to find ourselves the correct column with the last 3 elements of each pass.
#### Defining our column-function
col_finder <- function(boarding_pass, max_col_tracker){
  ### First we'll make a sequence from 1 (R starts with 1) to the maximum columnn number (8)
  col_seq <- seq(1, max_col_tracker)
  ### For each of the 3 column-indicators in the boarding pass (elements 8 thru 10)..
  for(n in 8:10){
    ## ..take the first element from the boarding pass string.
    sub_str <- substring(boarding_pass, n, n)
    ## If it be an L...
    if(sub_str == "L"){
      # ...divide our maximum row_number by half...
      max_col_tracker <- max_col_tracker / 2
      # Then SUBTRACT that from the maximum of our sequence of columns and, from that, make a sequence from the minimal column up to the cut maximum.
      col_seq <- seq(min(col_seq), max(col_seq) - max_col_tracker)
    }
    ## if that substring happens to be my favourite programming language's name...
    if(sub_str == "R"){
      # ...divide our maximum row_number by half...
      max_col_tracker <- max_col_tracker / 2
      # Then ADD that to the minimum of our sequence of columns and, from that, make a sequence from the newly boosted minimal columns up to the maximum.
      col_seq <- seq(min(col_seq) + max_col_tracker, max(col_seq))
    }
  }
  ### Give us back our columns number minus 1, since R starts with the number 1, 'cause it's the number one programming language(...?). 
  return(col_seq - 1)
}

### Now it's time to combine all that into one overarching function.
Seat_finder <- function(boarding_pass, max_row_tracker, max_col_tracker){
  ## Find our row-value
  row_value <- row_finder(boarding_pass, max_row_num)
  ## Find our column
  col_value <- col_finder(boarding_pass, max_col_num)
  
  ## Give us our Seat-ID...
  Seat_id <- row_value * 8 + col_value
  ## ...and return them all in a dataframe.
  return(data.frame(row_value, col_value, Seat_id))
}



### Now we'll use R's version of lambda (apply) to apply our new omnibus-function to all boarding passes in the data.
results <- mapply(Seat_finder, boarding_passes$V1, max_row_num, max_col_num)
## We'll just stuff them into a transposed dataframe so that the boarding-passes are the rows and the extracted values are the columns.
results <- data.frame(t(results))

## Since mapply adds list elements to a dataframe, we´ll just unlist the column before unleahsing the max-function on it to find our riddle's answer.
cat("The seat with the highest number in the list is: \n*drumroll*\n",max(unlist(results$Seat_id)))


##########################################################################################
################################# Part 2##################################################
##########################################################################################

### We need to find the seat ID that's missing in our sequence. Let's find out which that is.
## We'll start out by determining a maximum and a minimum potential seat number based on the coding system we interpreted above.
min_seat_id <- Seat_finder("FFFFFFFLLL", max_row_num, max_col_num)$Seat_id
max_seat_id <- Seat_finder("BBBBBBBRRR", max_row_num, max_col_num)$Seat_id

#### Now it's loop-time.
### We list all assigned seats_id's from our previous results and unlist them, as mapply has that weird tendency to return values in lists.
unlisted_results <- sort(unlist(results$Seat_id))
### Right... back to the ol' loopin'.
for(i in min_seat_id:max_seat_id){
  ## If a possible seat_id between the minimal possible number and maximum possible number is not (designated by "!") in our list of assigned seat_id's...
  if(!i %in% unlisted_results){
    # FIND THEM!! Uh.. Sorry. I meant check if i+1 and i-1 are in our unlisted_results. When we've found that, we'll have found our seat_id!
    if((i+1) %in% unlisted_results & (i-1) %in% unlisted_results){
      cat("Finally, we have found our elusively hidden seat. As it turns out, ours is seat", i)
    }
  }
}
