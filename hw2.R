## BMB: why include both hw2.txt and hw2.R?
## if you want to intersperse text and R code, best to use an Rmarkdown file

## BMB: note that tidyverse automatically loads dplyr, ggplot2
library(ggplot2); library(tidyverse); library(plotly); library(gifski)
library(dplyr); library(tidyquant)
library(gganimate); library(magick)

## BMB: prefer spaces around "<-"
dat <- readr::read_csv("https://mac-theobio.github.io/DataViz/data/vaccine_data_online.csv")

## first plot: local smoothing spline and its comparison for different types of disease
plot_1 <- ggplot(dat, aes(x=year, y=cases/1000, color=disease)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05)),
                     limits=c(0,NA),
                     oob = scales::squish) +
  scale_x_continuous(expand=expansion(mult=c(0,0.05))) +
  geom_smooth(size=0.3, method='loess', span=0.175) +
  geom_point(size=0.3) +
  facet_wrap(~disease, ncol = 3, nrow=3, scales = 'free') +
  geom_area(aes(color = disease, fill = disease),
            alpha = 0.5, position = position_dodge(0.45))+
  theme_tq()+
  theme(legend.position='none')+
  labs(title = 'The case study of different diseases by years')


## BMB: this is interesting and attractive (tidyquant theme, etc.). Problematic that loess fit confidence intervals go below zero (could use axis limits (coord_cartesian or as above)).  Also eliminated space between polygon and left/bottom axes (see above).  I would make loess curves thicker?

## output: interactive
ggplotly(plot_1)
## BMB: nice. Hovertext could be improved (remove redundant disease entry); format date as month/year?)

# second plot: animation on cases of disease
plot_2 <- ggplot(dat, aes(x=year, y=cases/1000, color=disease,
                        group = disease)) + geom_line()+
  geom_point(aes(group=disease), size=0.5)+
  facet_wrap(~disease, ncol = 3, nrow=3, scales = 'free_y')+
  coord_cartesian(clip = 'off') +
  transition_reveal(year)
## BMB: use scales="free_y"

# animation plot
plot_2
animation <- animate(plot_2, nframes=70, renderer=magick_renderer())
animation

# save animation
image_write_gif(animation, 'animation.gif')

## BMB: might want to save GIF to your repo (if it's not too big) since this is a slow process
## (later we can talk about using 'make' or the 'targets' package or something else to make this more
## automated/skip unnecessary rendering)

## I think the author is trying to portray the mitigation of vaccine for the disease brought a positive effect on the decrease of cases. Especially, the emphasis is on the policy for possible weak society including mothers, children and high-risk communities for the vaccination that brought a decrease of cases in different types of disease. Along the timeline of disease, the discovery of vaccine is clearly portrayed to show that the cases are decreased for each type of disease that emphasize the important the policy for implementation of vaccines on vulnerable societies including children, high-risk societies and mothers.

## BMB: your graphs seem to be missing the 'onset of vaccination' indicator, which is one of the main points of the original graph????


## I am pleased to display for the comparisons of types of disease in different facets within local splines as well as the animation in ggplot for the time line of cases. This would show simultaneously by animation for the decrease of cases in all 9 types of diseases and overall patterns by the local geom_smooth. In both studies of ggplots, the uniqueness of visualization would deliver the pattern of decrease by geom_smooth and facets while the other would deliver for the animation of timeline in disease from 1945 to 2015 for each plots to enjoy the effect of vaccines.

## BMB: these are loess curves, not spline curves.

## The graph of 9 types of diseases with smoothing splines and its cases conveys the comparisons of cases from 1945 to 2015 with different colors. The original graph is hard to recognize the different quantitative changes of cases but the ggplot with smoothing spline quantitively conveys the change of numbers and its patterns of cases, comparatively. This is more precise and accurate to find from the reader's perspective how the changes occur and see the comparison of 9 different disease. Also, all 9 diseases are conveyed in a different scales where each one of them are shown for its own scales of ranges. This is also easier to recognize for the readers to find the quantitive increase and decrease of the numbers for its own cases. It is also better to estimate also from the reader's point of view for the future timeline as well.
## Also, by the ggplotly, the interactive plot enables for the reader to accurately access the timeline of each plots to find precise information on each number of cases for particular number of years as well as click to zoom in for particular years for visualization, which overwhelms the original plot.

## BMB: I like the splines. the shading of the areas under the plots is visually effective. It can be good to show raw data as points (only) + smooth curves for the trend in some cases.  Colours are redundant but visually interesting; would you want to use a less arbitrary range of colours?

# However, the second plot of animation shows the timeline of its graph changes in cases and dynamically attract readers to identify the point that leads each plot. In other words, the readers would identify naturally for the decrease of cases on each diseases to think with, until the animation ends. Also, the line graph with point to animate accurately shows the change of cases for years and dynamically conveys even the small changes in the timeline. The dynamics of timeline for yearly changes for time series data are very well portrayed in the animation of point to observe with.

## BMB: The animation is cool, but it's pretty hard to pay attention to all of the lines changing at the same time.  Maybe it would work better in this case if the graph were *not* faceted?

# extra credit: The most popular and notorious COVID-19 disease for Ontario is portrayed in the https://health-infobase.canada.ca/covid-19/vaccination-coverage/.
## As the coverage of vaccine increase over time to be also estimated, the number of cases decreases similar to plots visualize. The vaccine coverage is analogous to plots where interactive plots shows better for the importance of vaccine in a comparative manners. Also, different age groups are shown with different colors to show that there are different result, to be similar to 9 facet graphs I have created.

## BMB: the graphs on this page are nice, but (1) I don't think you get (much) extra credit simply for pointing it out; (2) it only shows vax coverage, not disease incidence - it would be cool to try to draw analogous plots for COVID-19 (i.e. include both incidence and vaccine coverage in the same graphic. (Strictly speaking to be analogous to Jia You's plot we would only have to include the *start date* of COVID vaccination ...).

## BMB: mark: 2.4/3
