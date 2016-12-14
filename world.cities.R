##########################
#Key Transport Statistics of World Cities
##########################

# Setting working directory
getwd()
try(setwd("/Users/emiliasicari/Desktop/Final_assignment/Final_Academic_Paper/"), silent = TRUE)
try(setwd("/Users/rafalopezv/Dropbox/R/Final_assignment/"), silent = TRUE)


# Importing a preliminary cleaned table found here: http://www.lta.gov.sg/ltaacademy/doc/13Sep105-Pan_KeyTransportStatistics.pdf
# The first steps of the cleaning were made in excel
world.cities <- as.data.frame(read.csv("cities.transport.statistics.csv", 
                                       header = TRUE, sep = ";"))


#Creating new variables
world.cities["M.population"] <- world.cities$M.Population..000*1000 #population in absolute numbers
world.cities["C.population"] <- world.cities$C.Population..000.*1000 # population in absolute numbers
world.cities["M.population.millions"] <- world.cities$M.population/1000000 #population in millions
world.cities["C.population.millions"] <- world.cities$C.population/1000000 #population in millions
world.cities["metropolitan.density"] <- world.cities$M.population/world.cities$M.Area.km2 #population density in metropolitan areas 
world.cities["city.density"] <- world.cities$C.population/world.cities$C.Area.km2  #population density in main city areas
world.cities["rail.density"] <- world.cities$Rail.length.km/world.cities$M.population.millions #rail kilometers by million people
world.cities["metropolitan.rail.stations.density"] <- world.cities$No.of.rail.stations/world.cities$M.Area.km2 #density of rails in the metropolitan area
world.cities["city.rail.stations.density"] <- world.cities$No.of.rail.stations/world.cities$C.Area.km2 #density of rails in the city area
world.cities["public.buses.per.million.m"] <- world.cities$Public.bus.feet.size/world.cities$M.population.millions #buses per million in metropolitan area
world.cities["public.buses.per.million.c"] <- world.cities$Public.bus.feet.size/world.cities$C.population.millions #buses per million in metropolitan area
world.cities["road.density.c"] <- world.cities$Road.length.km/world.cities$C.Area.km2 # road density in city area (km per km2)
world.cities["cars.100.hab.m"] <- (world.cities$Private.cars..000./world.cities$M.Population..000.)*100 #private cars per 100 inhabitahts in the metropolitan area
world.cities["cars.100.hab.c"] <- (world.cities$Private.cars..000./world.cities$C.Population..000.)*100 #private cars per 100 inhabitabts in the city area
world.cities["daily.railridership.average.c"] <- (world.cities$MRT.LRT.usage..000./world.cities$C.Population..000.)*100
world.cities["daily.railridership.average.m"] <- (world.cities$MRT.LRT.usage..000./world.cities$M.Population..000.)*100
world.cities["daily.busridership.average.c"] <- (world.cities$Public.bus.usage..000./world.cities$C.Population..000.)*100
world.cities["daily.busridership.average.m"] <- (world.cities$Public.bus.usage..000./world.cities$M.Population..000.)*100
world.cities["average.ridership"] <- (world.cities$daily.busridership.average.m + world.cities$daily.railridership.average.m)/2
# Ploting variables relations
# density of rail stations vs daily rail ridership in metropolitan areas
library(ggplot2)
cities1 <- ggplot(world.cities, aes(x=world.cities$metropolitan.rail.stations.density, 
                                    y=world.cities$daily.railridership.average.m)) + 
  geom_point() + geom_text(aes(label=City),hjust=0, vjust=1) 
cities1

# number of buses vs daily bus ridership in metropolitan areas
cities2 <- ggplot(world.cities, aes(x=world.cities$public.buses.per.million.m, 
                                    y=world.cities$daily.busridership.average.m)) + 
  geom_point() + geom_text(aes(label=City),hjust=0, vjust=1) 
cities2 

# number of private cars vs average public transportation usage
cities3 <- ggplot(world.cities, aes(x=world.cities$cars.100.hab.m, 
                                    y=world.cities$average.ridership)) + 
  geom_point() + geom_text(aes(label=City),hjust=0, vjust=1) 
cities3        
       


                  
                  
                  




