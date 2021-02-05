# QMEE
Stats 708

# Assignment 1 #

About my data: I want to know if social groups improve their collective decision-making as they gain experience. To do this, I am providing 20 ant colonies (choice group) with experience making a choice between a good- and poor-quality nest during emigration, and 20 colonies (no choice group) with a single nest during emigration, leaving no room for decision-making. After six emigrations during the training phase, I challenged all colonies to decide between two nest options during a final test emigration. I predict that the choice group with be faster and more efficient at decision-making than the no choice group. I also predict both groups will improve performance throughout the training phase.

# Assignment 2 #

This is only a small portion of my data so far, I will be getting more soon. I want to look at changes in my variables over time (training sessions). I also want to compare the variables in the final session (will add it as training 7) between the choice group and no choice group. I run 10 colonies at a time, 5 of each group, so each ten is considered a replicate.

# Assignment 3 #

With the test data from my experiment (different that what I used in assignment 1 and 2), I wanted to compare three variables between my two groups to show the differences. 
After trying out some different types (violin, jitter, scatter, dotplot) I found the boxplot best represented what I wanted, and the range and quartiles are informative. Geom_point was also nice to see individual data points, but I liked seeing the median in the boxplots. Combining the geom_point and boxplot didn't look good either. I omitted most of the trial and error in the script to clean it up.
The Cleveland hierarchy: My three variables are measured in different units so I kept the plots separate. I think I followed the rules well, it's not very hard with a simple boxplot. Except that I have colour because it looks nicer than plain bars, alhtough without colour the plots would give the same info.



