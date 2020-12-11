######################## 
######## DAY 10 ######## 
######## PART 1 ######## 
######################## 
options(scipen = 999)

### Loading data
data <- read.delim("task_10_data.txt", header = FALSE)

### First, we'll sort our data from low to high and turn it into a vector (flattening the dataframe) by calling on its only column
sorted_vector <- sort(data$V1)
### Now we compute the differences between each element in the vector.
differences <- diff(sorted_vector, 1)

### All that remains is to look for the number of ones...
num_ones <- length(differences[differences == 1]) + 1
### ...and twos.
num_threes <- length(differences[differences == 3]) + 1 # same here.

### And multiply them.
final_answer <- num_ones * num_threes
cat("The final answer to our grand question of ones and threes is, well, this:", final_answer, ". I know! I'm shocked too.")

######## PART 2 ######## 

#### This is going to be a little more complicated. No more cheap tricks like the one I pulled above... ####
### First up, we're going to expand our old sorted vector with a starting 0 and the old vector's maximum value +3.  
### That's our device's built-in joltage adapter. 
new_sorted_vector <- c(0, sorted_vector, (max(sorted_vector)+3))

### Time to get weird. We'll make a vector of empty values (FALSE) with the length of our new vector -1. This vector
### called arrangements will measure... well... the number of possible arrangements we encounter.
vec_length <- length(new_sorted_vector)-1
arrangements <- vector(length = vec_length)
## Then we designate the first line as '1'. That's where we'll kickstart our magical math off of. 
arrangements[1] <- 1

#### Go time!
### For each element in our vector...
for(i in 1:(length(new_sorted_vector))){
  ## ...add 1 and store it as j...
  j = i + 1
  ## a...s long as j does not exceed the length of our vector containing adapters AND
  ## the differences between element j - element i on it is less than 4...
  while(j < length(new_sorted_vector) & new_sorted_vector[j] - new_sorted_vector[i] < 4){
    # ,,,the next line in our possible arrangements will be expanded. 
    arrangements[j] = arrangements[j] + arrangements[i]
    # After that little party's done, add +1 to j, to keep things moving. 
    j = j + 1
    # Give us a sneak peak into what you're doing.
    print(arrangements)
  }
}

### Aaaaand we're done!
cat(
"\nAnd now, all in my own countree,
I stood on the firm land!
The Hermit stepped forth from the boat,
And scarcely he could stand.

'O shrieve me, shrieve me, holy man!'
The Hermit crossed his brow.
'Say quick,' quoth he, 'I bid thee say-
What manner of man art thou?'

Forthwith this frame of mine was wrenched
With a woful agony,
Which forced me to begin my tale;
And then it left me free.


Since then, at an uncertain hour,
That agony returns:
And till my ghastly tale is told,
This heart within me burns.

I pass, like night, from land to land;
I have strange power of speech;
That moment that his face I see,
I know the man that must hear me:
To him my tale I teach.
    
Nah. Just kidding. This is the actual answer:\n", 
tail(arrangements, 1), 
"\n ...holy crap. That's a lot.
Anyway, if you're interested, check out the rest of the epic poem that is the Rhyme of the Ancient Mariner by Samuel Taylor Coleridge that you see above. 
It's a personal favourite: https://www.poetryfoundation.org/poems/43997/the-rime-of-the-ancient-mariner-text-of-1834")
