library(ggplot2); library(tidyverse); library(forcats); library(RColorBrewer)
library(plotly); library(tidyquant); library(ggrepel); library(plyr)

# https://www.najms.org/article.asp?issn=1947-2714;year=2012;volume=4;issue=9;spage=405;epage=410;aulast=Kumar 
# title: High satisfaction rating by users of private-for-profit healthcare providers-evidence from a cross-sectional survey among inpatients of a private tertiary level hospital of north India

# table 4.
# Notice names of questions are too long to be omitted as q1-q22 in the table.

# table from the paper
da <- read.table(header=TRUE,text="
question 4 3 2 1 0 NA
q1 70 27 3 2 0 0
q2 8 55 19 17 2 1
q3 40 47 12 0 1 2
q4 22 68 7 5 0 0
q5 40 55 4 3 0 0
q6 14 55 23 9 0 1
q7 20 71 7 3 0 1
q8 28 69 2 1 1 0
q9 30 71 1 0 0 0
q10 24 68 10 0 0 0
q11 49 41 8 3 0 1
q12 47 50 3 2 0 0 
q13 32 57 9 3 1 0
q14 22 74 6 0 0 0
q15 12 62 22 6 0 0
q16 35 60 5 1 1 0
q17 32 67 2 1 0 0
q18 23 67 8 4 0 0
q19 17 73 9 1 1 1
q20 16 69 14 3 0 0
q21 21 75 4 2 0 0
q22 17 78 5 1 1 0
")

## might use, color choices
bluecols <- brewer.pal(5, 'Blues')

## setting of the table with values and names
rownames(da)<-c(da[,1])
colnames(da)<-c('ques',4,3,2,1,0)
head(da)

## separate data and not answered values
n_a<-da[,7];da<-da[,-7]

## save arrangement of questions
qq<-rownames(da)

# formulize as table with values arranged
p<-da %>% 
  pivot_longer(c(`0`, `1`, `2`, `3`, `4`), names_to = "rating", values_to = "cases")%>%
  mutate(per=paste0(ceiling(100*cases/102),'%'))%>%
  arrange(match(ques, qq, desc(rating), desc(cases)))%>%
  mutate(key = forcats::fct_inorder(ques))

## checkpoint: find if there is any error
p

## erase unnecessary numbers: 0%
p$per[p$per == '0%']<-c('')


# plot as likert type graph
p4<-ggplot(mapping=aes(x=fct_inorder(key), y=cases, fill=as.factor(rating)), p) + 
  geom_col(width=0.575) + theme_tq()+
  geom_text(aes(label=ifelse(cases>=2, per, "")), position=position_stack(.5), size=3.25) + ## label big value
  geom_text_repel(aes(label=ifelse(cases<2, per, "")), direction="y", min.segment.length = 0, position=position_stack(.5), size=3.25)+ ## label small values
  coord_flip()+ theme(legend.position='bottom')+ 
  scale_fill_manual(values = bluecols, guide = guide_legend(reverse = TRUE), labels = c('Strongly disagree', 'Disagree',
      'Niether agree nor disagree', 'Agree', 'Strongly agree'))+
  labs(y = "Total number",fill = "Type", title = 'Table 4. Overall Ratings')+ xlab(NULL)

p4

# save as picture gfile
ggsave(file='graph_viz.png')



## Kyuson:comments
# Notice names of questions are too long to be omitted as q1-q22 in the table.
# I am trying to visualize the table into stacked bars of proportions for comparisons and an identification for a majority of ratings in the survey. 
# Notice names of questions are too long to be omitted as q1-q22 in the table.
# This would give the long tables of numbers into one unified graph in a order of question numbers as same the paper portrays. 

# Moreover, this feature delivers a simple and fast recognition of the raw table for readers to find out a satifaction rate of particular ratings as well as all opinions.
# The total number of participants is located at the x-axis to find out with close number of 102 participants. 
# The ratings of legend is located at the bottom and the blue tone colors are chosen manually to convey the comparisons of ratings from worst of strongly disagree to the best ratings of strongly agree.
# Note the legend is manually ordered as to be consistent with the graph of visual rating colors that portrays from strongly agree to strongly disagree for matching between colors and the legend. 
# Also, questions are ordered in inorder to notice the first question on the bottom and the last question at the top.
# This is intended for the case questionaire are not ordered to be changed by the proportion, but to identify question by question from the bottom to the top to compare each question, going through each question. 
# Using repel function, minority proptions are not overlapped and seen at the end while others are intended to be inscribed in bars.
# Meanwhile, I find the thin bars (with size 0.575) to be most sophisticated for a view.

# However, some questions with very minor surve ratings (eg. 1%/ 2%) to be stacked together is hard to recognize and find in the plot for the comparison as well as labeling is very perfectly fitted. (Which could be manually labelled or omitted)
# Furthemore, to preserve the original paper's table where some proportion are round up, the graph is intended to show with the ceiling function to portray the minor proportions. 
# The unanswered questions are omitted from the stacked proportion to be failed to account with. 
# The not answered pariticipant proportion is omiitted from the graph to visually find at the end of stacked bar as a missing part, but it could raise misunderstanding which I tried to account them into a separate graph to differentiate with survey ratings. 
# Note that this is intended to differenciate from the survey ratings strictly, where the table from the original paper failed to show with (I think, the paper conveys them in one row which is an misleading for one to find coomplete comparison with).
# Hence, I tried to creat a buble chart and combine two graphs in one graphs, but the bubble chart does not seem to catch the quantative importance of not answered category to help with. 



# --------------- additional 'not answered component' ----------------------- #

## agregate values
categ<-c(rep('Not answered', 22))
n_aa<-as.data.frame(cbind(da$ques, categ, n_a))
colnames(n_aa)<-c('question','cate','number')

## bubble chart for not answered data
m<-ggplot(n_aa, aes(x = cate, y = question, size = number))+ theme_tq() +xlab(NULL)+ ylab(NULL)+
  geom_point() + scale_size_manual(values = c('1'=1.5, '2'=4)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position='bottom')+
  labs(title = 'Not answered items')
m

## combined (not successful)
library(cowplot)
plot_grid(p4, m, #labels = c('ratings', 'not answered'),
              align = "h", rel_widths = c(5,1))





# Here, I tried to show with bubble chart for the not answered proportion of questions, by creating a bubble chart to show within a size of bubbles.
# However, this is a quite a failure for the consistency of quantitive aspect, when combined with stacked bar as a one visualization to show at the end. 
# Also, the size of bubble does not match with bars and it was failed also for the size of bubble to match with no answer numbers, where 1 and 2 does not seem to differentiate a lot.
# This portrays failure of the two graphs and no answer proportion to be misleading and quantitatively hard to show as a one visualization with 2 graphs. 
# Stil at the end of each question, the readers would find out the for the not asnwered participant numbers and match with rest of ratings to find the complete total number of participants. 
# Also, in this way of combining graph I tried to separate the missing part to be different from ratigns of survey by the participants to convey the opinions and voluntary unanswered proportions.
# Therefore, I think it is the best to show the just one proportion of stacked ggplot to be consistent and most suitable plot for the paper to substitute, as we still have the unanswered numbers to be omitted and explained to readers in the paper as lines. 






## --------------- Optional: In the case 'blue tone' colors are not visual to find for the comparison ----------------------- ##

# For the case that the colors are not appealing to find differentiable, the viridis color choices are used to visually find distinct between ratings of the survey. 


p4<-ggplot(mapping=aes(x=fct_inorder(key), y=cases, fill=as.factor(rating)), p) + 
  geom_col(width=0.575) + theme_tq()+
  geom_text(aes(label=ifelse(cases>=2, per, "")), position=position_stack(.5), size=3.25) + ## label big value
  geom_text_repel(aes(label=ifelse(cases<2, per, "")), direction="y", min.segment.length = 0, position=position_stack(.5), size=3.25)+ ## label small values
  coord_flip()+ theme(legend.position='bottom')+ 
  scale_fill_viridis_d(guide = guide_legend(reverse = TRUE), labels = c('Strongly disagree', 'Disagree',
                                                                                        'Niether agree nor disagree', 'Agree', 'Strongly agree'))+
  labs(y = "Total number",fill = "Type", title = 'Table 4. Overall Ratings')+xlab(NULL)

p4

# In this way, the blue tone for coherent color is changed to be more visually distinct as for readers who want to colors to be different.


# More story:

# The graph tells the proportions of questions in a best a way to compare and understand each ratings as well as see the differences of rating proportions for readers. 
# Also, the visualization is appealing to convey the overview of survey within proportions, as to be appealing for readers.
# The best overview is the comparisons of proportion within questions and the proportions to compare between questions, where questions are well ordered in order followed from the original questions. 
# Also, the color and the comparison of blocks are greatly noticeable and distinct to be identified from the plot to deliver for the meanings of opinions of the survey as much possible. 
# In other words, the overview of complete ratings must deliver within the portray of graph for participant's overall opinions as much as possible and the graph by its characteristics could most suitably inform in terms of stacked proportions.

