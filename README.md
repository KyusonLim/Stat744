# STATS744_task 1

Double donut chart (double pie chart): https://stackoverflow.com/questions/26748069/ggplot2-pie-and-donut-chart-on-same-plot

I think this is creative and useful for showing each sub-proportion of the proportion so to see in beautiful colored mapped chart.  
In this graph, people are able to compare for medical conditions of age groups.
Also, notice very evidently for which groups are volnuerable to COVID-19 of health conditions.

Cons: relative comparison are very obvious.
Age groups of people will be compared to find which groups are volunerable to ICU, intubated hospitalization.
Not numbers, but pie chart of probability comparison is very obvious and critically compared. 
Most importanly, within group comparison is highly comparable. 

## JD comments

This is really nice work, and visually very attractive. I am not convinced by all of your points why you like it, though.

I agree that the double-pie is a creatively pretty way of looking at sub-proportions, but (as we learned in class and from Rauser) it is not easy to process quantitative information from pie charts. There is also a problem of visual misleading: it is not clear what we are meant to infer from the relative size of the outer donut. There is no natural reason here for the pie shape, and it makes a lot of the comparisons harder to make.

"Con" means a disadvantage. Why would obvious relative comparisons be a disadvantage? Also, why do you think that the relative comparisons are obvious here? 

This code is very "ticky", by which I mean there is a lot of difficult detail that it would be easy to get wrong. Where did you get the data? You should start from there and make a nice, readable table and use R tools to read the data. It would also be nice to try to make a plot like this in a ggplot framework; you might start with some sort of bar chart instead of a pie chart

A good example of why to use code instead of typing ticky details: the very first number you calculate (51) is wrong (should be 61). It is very hard to read this code to find out what you did or to see what mistakes might be there.

Grade: 2.4/3
