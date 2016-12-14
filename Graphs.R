#################
##figures 
###############

#Setting working directory
try(setwd("/Users/rafalopezv/Dropbox/ACADEMIC TRANSITION/NUS/writing.proof.1/"), silent = TRUE)

#Sourcing external file with preliminary analyses to data
try(source("/Users/rafalopezv/Dropbox/ACADEMIC TRANSITION/NUS/writing.proof.1/Preliminary_analyses.R"))

#Loading packages
library(ggplot2)
library(scales)

#Creating third figure: trend in gdp, bottom 90% and top 10% average income
IV <- ggplot2::ggplot(data.final, aes(x= date)) +
  geom_line(aes(y=data.final$top, color= "top")) +
  geom_line(aes(y=data.final$bottom, color = "bottom")) + 
  geom_line (aes(y=data.final$gdp.per.capita, color = "gdp.per.capita")) +
  theme_light()

#Changing the colours of lines and labels of the legend
IV <- IV + scale_color_manual(values=c("#000066", "#3399FF", "#33CCFF"),
                              name = NULL,
                              breaks=c("bottom", "gdp.per.capita", "top"),
                              labels=c("Bottom 90% average income", "GDP per capita", "Top 10% average income"))

#Changing the name of the variables in the axis
IV <- IV + labs(x = "years", y = NULL)

#Changing the position of the legend 
IV <- IV + theme(legend.position="bottom")

#Changing the colour of the background of the plot
IV <- IV + theme(panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.line.x = element_line(size = 0.25, linetype = "solid",
                                            colour = "black"), 
                 axis.line.y = element_line(size = 0.25, linetype = "solid", 
                                            colour = "black"))
#Adding title
IV <- IV + ggtitle("Figure 1 - Income distribution in Singapore (1995-2014)") + theme(plot.title = element_text(lineheight= 1, size = 10))

#Calling the figure
IV

#Creating the second figure: trend in inequality
INEQUALITY <- ggplot2::ggplot(data.final, aes(x=data.final$date, y=data.final$inequality)) + geom_line(aes(group=1), colour="#3399FF") + theme_classic()

#Changing the name of the variables in the y axis
INEQUALITY <- INEQUALITY + labs(x = "years", y = NULL)

#Changing the colour of the background of the plot
INEQUALITY <- INEQUALITY + theme(panel.border = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 axis.line.x = element_line(size = 0.25, linetype = "solid",
                                                            colour = "black"), 
                                 axis.line.y = element_line(size = 0.25, linetype = "solid",
                                                            colour = "black"))
#Adding title
INEQUALITY <- INEQUALITY + ggtitle("Figure 2 - Income Inequality gap (1995-2014)") + theme(plot.title = element_text(lineheight= 1, size = 10))

#Calling the figure
INEQUALITY

#Creating the final graph: trend in population
POPULATION <- ggplot2::ggplot(data.final, aes(x=data.final$date)) + 
  geom_line(aes(y=data.final$residents, color= "residents")) +
  geom_line(aes(y=data.final$non.residents, color= "non.residents")) + 
  theme_classic()

#Changing the colours of lines and labels of the legend

POPULATION <- POPULATION + scale_color_manual(values=c("#000066", "#3399FF"),
                                              name = NULL,
                                              breaks=c("residents", "non.residents"),
                                              labels=comma)

POPULATION <- POPULATION + scale_y_continuous(name= NULL, labels = comma)

#Changing the name of the variables in the y axis
POPULATION <- POPULATION + labs(x = "years", y = NULL)

#Changing the colour of the background of the plot
POPULATION <- POPULATION + theme(panel.border = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 axis.line.x = element_line(size = 0.25, linetype = "solid",
                                                            colour = "black"), 
                                 axis.line.y = element_line(size = 0.25, linetype = "solid",
                                                            colour = "black"))

#Changing the position of the legend

POPULATION <- POPULATION + theme(legend.position="bottom")

#adding title
POPULATION <- POPULATION + ggtitle("Figure 3 - Population growth (1995-2014)") + theme(plot.title = element_text(lineheight= 1, size = 10))

#Calling the figure
POPULATION

#Creating the fourth figure: trend in vehicles utilization           
PT <- ggplot2::ggplot(data.final, aes(x= date)) +
  geom_line(aes(y=data.final$mrt.u.pop, color= "mrt.u")) +
  geom_line(aes(y=data.final$lrt.u.pop, color = "lrt.u")) + 
  geom_line (aes(y=data.final$bus.u.pop, color = "bus.u")) +
  theme_light()

#Changing the colours of lines and labels of the legend
PT <- PT + scale_color_manual(values=c("#000066", "#3399FF", "#33CCFF"),
                              name = NULL,
                              breaks=c("mrt.u", "lrt.u", "bus.u"),
                              labels=c("MRT", "LRT", "Buses"))

#Changing the name of the variables in the axis
PT <- PT + labs(x = "years", y = NULL)

#Changing the position of the legend 
PT <- PT + theme(legend.position="bottom")

#Changing the colour of the background of the plot
PT <- PT + theme(panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.line.x = element_line(size = 0.25, linetype = "solid",
                                            colour = "black"), 
                 axis.line.y = element_line(size = 0.25, linetype = "solid", 
                                            colour = "black"))
#Adding title
PT <- PT + ggtitle("Figure 4 - Public Transportation use in Singapore (1995-2014)") + theme(plot.title = element_text(lineheight= 1, size = 10))

#Calling the figure
PT

#Creating the first figure: trend in number of cars
CARS <- ggplot2::ggplot(data.final, aes(x=data.final$date, y=data.final$car.u.pop)) +
  geom_line(aes(group=1), colour="#3399FF") + 
  theme_classic()

#Changing the name of the variables in the y axis
CARS <- CARS + labs(x = "years", y = "cars per 100 people")

#Changing the colour of the background of the plot
CARS <- CARS + theme(panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line.x = element_line(size = 0.25, linetype = "solid",
                                                colour = "black"), 
                     axis.line.y = element_line(size = 0.25, linetype = "solid",
                                                colour = "black"))

CARS <- CARS + scale_y_continuous(name= NULL, labels = comma)

#giving a title to the graph
CARS <- CARS + ggtitle("Figure 5 - Private cars in Singapore (1995-2014)") + theme(plot.title = element_text(lineheight= 1, size = 10))

#Calling the figure
CARS


