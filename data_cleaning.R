library(tidyverse)
library(readxl)
library(reshape2)
library(sf)
library(htmlwidgets)
library(leaflet)
library(ggpubr)
library(hrbrthemes)

options(scipen=999)

# load traffic data
traffic = read.csv('raw-data-2010-2019.csv')

# find average traffic volume for each intersection
traffic$total = rowSums(traffic[13:60])
avg_traffic = aggregate(traffic$total, by=list(traffic$location), FUN=mean)

# rename columns
colnames(avg_traffic) =  c("Street","avg_volume")

# remove text between parentheses
avg_traffic$Street = trimws(gsub("\\(([^()]+)\\)", "", avg_traffic$Street))


# read column names and shapefile of collision data
names = read.csv('Motor Vehicle Collisions with KSI Data fields.csv')
ksi = read_sf('Motor Vehicle Collisions with KSI Data - 4326.shp')

# get coordinates
coords = as.data.frame(st_coordinates(ksi))
ksi = as.data.frame(ksi)

ksi$X = coords$X
ksi$Y = coords$Y

colnames(ksi ) = c(names$name, 'geometry','X', "Y")

# get total collisions for each intersection
intersection_total = ksi %>% 
  group_by(STREET1, STREET2) %>% 
  summarise(count=n())

# join traffic and collision data
intersection_total$Street = paste(intersection_total$STREET1 , intersection_total$STREET2 ,sep=" AT ")
traffic_and_collisions =  merge(intersection_total, avg_traffic, by = "Street")

write.csv(traffic_and_collisions, 'traffic_and_collisions.csv')

# regression model with accidents and traffic volume
model = lm(count~avg_volume, data=traffic_and_collisions)
summary(model)


ggplot(traffic_and_collisions, aes(x = avg_volume, y = count)) + 
  geom_point(size = 3, shape = 21, color = "black", fill = "blue") +
  geom_abline(linewidth=1.5,slope=model$coefficients[2], intercept=model$coefficients[1]) +
  stat_cor(p.accuracy = 0.05, r.accuracy = 0.001) +ggtitle("Traffic Volume vs Collisions at Intersections") +
  xlab('Average Intersection Volume') +ylab('Number of Collisions') + theme_ipsum()

# table grouped by location coordinate and accident location
loccoord_accloc = ksi %>% 
  group_by(LOCCOORD, ACCLOC) %>% 
  summarise(Count=n()) %>% 
  arrange(desc(Count)) %>% 
  head(10)
names(loccoord_accloc)<-c("Location Coordinate","Accident Location", "Count")


# table grouped by only location coordinate
loccoord = ksi %>% 
  group_by(LOCCOORD) %>% 
  summarise(Count=n()) %>% 
  arrange(desc(Count))# %>% 
  #mutate(Ratio = Count / sum(Count)) 

df = data.frame('Other',sum(loccoord$Count[3:8]))
names(df)<-c("LOCCOORD","Count")
loccoord = rbind(loccoord, df)
loccoord = loccoord[-c(3:8), ]  
names(loccoord)<-c("Location Coordinate", "Count")

# table of accidents grouped by traffic control and road type
TF_CTL = ksi %>% 
  group_by(TRAFFCTL, ROAD_CLASS) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count)) %>% 
  head(10)
names(TF_CTL)<-c("Traffic Control","Road Class", "Count")


# load neighborhood census data
neighborhood_prof =  read_xlsx("neighbourhood-profiles-2021-158-model.xlsx")
neighborhood_prof = data.frame(t(neighborhood_prof)) %>% tibble::rownames_to_column() %>% janitor::row_to_names(row_number = 1)

nb_reduced = select(neighborhood_prof,1:2,2580 ,248,4 )
colnames(nb_reduced)<- c("neighbourhood","nb_num","public_transit", "af_tax_inc", "pop")
nb_reduced[2:5] = nb_reduced[2:5] %>% mutate_if(is.character,as.numeric)

# get accident totals for each neighborhood
nb_counts = ksi %>% 
  group_by(NEIGHBOURHOOD_158, HOOD_158) %>% 
  summarise(count=n())

# join with census variables
nb_counts=merge(nb_counts,nb_reduced, by.x = 'HOOD_158', by.y = 'nb_num')

write.csv(nb_counts, "nb_counts.csv")

# load shapefile for neighborhood boundaries
neighborhoods = read_sf('Neighbourhoods - 4326.shp')
neighborhoods = merge(nb_counts,neighborhoods, by.x = 'NEIGHBOURHOOD_158', by.y = 'AREA_NA7')

neighborhoods = st_as_sf(subset(neighborhoods, select = -c(4,8:17) ))

# create map of neighborhoods and accidents
bins <- c(0, 100, 200, 300, 400, 500, Inf)
pal <- colorBin("viridis", domain = neighborhoods$count, bins = bins)

labels <- sprintf(
  "%s</strong><br/>%g Collisions",
  neighborhoods$NEIGHBOURHOOD_158, neighborhoods$count
) %>% lapply(htmltools::HTML)


leaflet(neighborhoods) %>%
  addPolygons( fillColor = ~pal(count), 
               stroke=TRUE,
               fillOpacity = 0.7,
               weight = 2,
               opacity = 0.7,
               color = "black",
               dashArray = "3",
               highlightOptions = highlightOptions(
                 weight = 5,
                 color = "white",
                 dashArray = "",
                 bringToFront = TRUE),
               label = labels
               ) %>% 
   addLegend(pal = pal, values = ~count, opacity = 0.7, title = 'Count',
                position = "bottomright") %>% 
   addProviderTiles(providers$CartoDB.Positron)


# regression models for various census variables
income = lm(neighborhoods$count ~ neighborhoods$af_tax_inc)
summary(income)
ggplot(neighborhoods, aes(x = af_tax_inc, y = count)) + 
  geom_point(size = 3, shape = 21, color = "black", fill = "blue") +
  geom_abline(linewidth=1.5,slope=income$coefficients[2], intercept=income$coefficients[1]) +
  stat_cor(p.accuracy = 0.05, r.accuracy = 0.001) +ggtitle("Household Income vs Accidents") +
  xlab('Average After-Tax Household Income') +ylab('Number of Collisions') + theme_ipsum()


transit = lm(neighborhoods$count~ neighborhoods$public_transit)
summary(transit)
ggplot(neighborhoods, aes(x = public_transit, y = count)) + 
  geom_point(size = 3, shape = 21, color = "black", fill = "blue") +
  geom_abline(linewidth=1.5,slope=transit$coefficients[2], intercept=transit$coefficients[1]) +
  stat_cor(p.accuracy = 0.05, r.accuracy = 0.001) +ggtitle("Public Transit Usage vs Accidents") +
  xlab('# of Public Transit Commuters') +ylab('Number of Collisions') + theme_ipsum()

# regression model of population and accident count
pop = lm(neighborhoods$count~ neighborhoods$pop)
ggplot(neighborhoods, aes(x = pop, y = count)) + 
  geom_point(size = 3, shape = 21, color = "black", fill = "blue") +
  geom_abline(linewidth=1.5,slope=pop$coefficients[2], intercept=pop$coefficients[1]) +
  stat_cor(p.accuracy = 0.05, r.accuracy = 0.001)+ ggtitle("Neighborhood Population vs Accidents") +
  xlab('Population') +ylab('Number of Collisions') +theme_ipsum()

# number of accidents for each location within west-humber claireville neighborhood
ksi %>% 
  filter(HOOD_158 == 1) %>% 
  group_by(STREET1,STREET2) %>% 
  summarise(c=n()) %>%
  arrange(desc(c)) %>% 
  head(10) %>% 
  `colnames<-`(c("Street ", "Street 2", "Count"))
  

# map of accidents within West-humber claireville neighborhood
WHC = ksi %>% 
  filter(HOOD_158 == 1) 

WHC = st_as_sf(WHC)


labels2 <- sprintf(
  "Street 1: %s</strong><br/>Street 2: %s",
  WHC$STREET1, WHC$STREET2
) %>% lapply(htmltools::HTML)

neighborhoods[140,]$geometry
leaflet(WHC) %>% addTiles() %>%
  addMarkers(lng = ~X, lat = ~Y,clusterOptions = markerClusterOptions(),
             label = labels2)%>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data= neighborhoods[140,]$geometry, 
              stroke=TRUE,
              fillOpacity = 0,
              weight = 2,
              opacity = 1,
              color = "black",
              dashArray = "3")
