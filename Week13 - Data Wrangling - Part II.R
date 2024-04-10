# ********DATA WRANGLING - EXERCISE 9 TO 14 ******

library(tidyverse)

# ******Import Files

den <- read.csv('C:/Users/Danielle/Downloads/dennys(2).csv')
laq <- read.csv('C:/Users/Danielle/Downloads/laquinta(2).csv')

# ******Create subset - Denny's in North Carolina

den_nc <- den %>% 
  filter(state == 'NC')

den_nc

# ******Create subset - La Quinta in North Carolina

laq_nc <- laq %>% 
  filter(state == 'NC')

laq_nc

# ******Full Join of La Quinta and Denny's places in North Carolina

den_laq_nc <- full_join(den_nc, laq_nc, by= 'state')

den_laq_nc

# ******Create the function that calculates the distance using Latitude and Longitude
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}

# ******Calculate the distances between all the places combination in North Carolina

den_laq_nc <- den_laq_nc %>% 
  mutate (distances = haversine (longitude.x, latitude.x, longitude.y, latitude.y))
  
den_laq_nc %>% 
  arrange(distances)

# ******Show the closest distance from Denny's and La Quinta places in North Carolina

den_laq_nc %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distances)) %>% 
  ggplot(aes(x = closest))+
  geom_histogram(fill = 'pink',
                 color='black',
                 binwidth = 2)

 
den_laq_nc_clos <- den_laq_nc %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distances))

#---------------------------------------------------------------------------

# ******Create subset - Denny's in Texas

den_tx <- den %>% 
  filter(state == 'TX')

den_tx

# ******Create subset - La Quinta in Texas

laq_tx <- laq %>% 
  filter(state == 'TX')

laq_tx

# ******Full Join of La Quinta and Denny's places in Texas

den_laq_tx <- full_join(den_tx, laq_tx, by= 'state')

den_laq_tx

# ******Create the function that calculates the distance using Latitude and Longitude
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}

# ******Calculate the distances between all the places combination in Texas

den_laq_tx <- den_laq_tx %>% 
  mutate (distances = haversine (longitude.x, latitude.x, longitude.y, latitude.y))

den_laq_tx %>% 
  arrange(distances)

# ******Show the closest distance from Denny's and La Quinta places in Texas

den_laq_tx %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distances)) %>% 
  ggplot( aes(x = closest))+
  geom_histogram(fill = 'blue',
                 color='black',
                 binwidth = 2)

den_laq_tx_clos <- den_laq_tx %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distances))

#---------------------------------------------------------------------------

# ******Create subset - Denny's in California

den_ca <- den %>% 
  filter(state == 'CA')

den_ca

# ******Create subset - La Quinta in California

laq_ca <- laq %>% 
  filter(state == 'CA')

laq_ca

# ******Full Join of La Quinta and Denny's places in California

den_laq_ca <- full_join(den_ca, laq_ca, by= 'state')

den_laq_ca

# ******Create the function that calculates the distance using Latitude and Longitude
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}

# ******Calculate the distances between all the places combination in California

den_laq_ca <- den_laq_ca %>% 
  mutate (distances = haversine (longitude.x, latitude.x, longitude.y, latitude.y))

den_laq_ca %>% 
  arrange(distances)

# ******Show the closest distance from Denny's and La Quinta places in California

den_laq_ca %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distances)) %>% 
  ggplot( aes(x = closest))+
  geom_histogram(fill = 'steelblue',
                 color='black',
                 binwidth = 2)

den_laq_ca_clos <- den_laq_ca %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distances))


# --------------------------------------------------
#Exercise 12

den_laq_nc %>% 
  count (real_close = distances <1) %>% 
  summarize(share = (sum(real_close = TRUE) / n()) * 100)


den_laq_tx %>% 
  count (real_close = distances <1)%>% 
  summarize(share = (sum(real_close = TRUE) / n()) * 100)


den_laq_ca_test <- den_laq_ca %>% 
  count (real_close = distances <1)



ggplot()+
  geom_histogram(data = den_laq_nc_clos, aes(x = closest),
                 fill = 'red',
                 color='black',
                 alpha = 0.2,
                 binwidth = 5)+
  geom_histogram(data = den_laq_tx_clos, aes(x = closest),
                 fill = 'blue',
                 color='black',
                 alpha = 0.3,
                 binwidth = 5)+
  geom_histogram(data = den_laq_ca_clos, aes(x = closest), 
                 fill = 'green',
                 color='black',
                 alpha = 0.1,
                 binwidth = 5)




den_laq_ca_test %>% 
  summarize(real_close/n())

# -------------------BEST OPTMIZED WAY-----------

# ******Full Join of La Quinta and Denny's places 

den_laq <- full_join(den, laq, by= 'state')

den_laq

# ******Create the function that calculates the distance using Latitude and Longitude
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}

# ******Calculate the distances between all the places combination

den_laq <- den_laq %>% 
  mutate (distances = haversine (longitude.x, latitude.x, longitude.y, latitude.y))



# ******Show the closest distance from Denny's and La Quinta places

den_laq %>% 
 # filter(state == 'AK') %>% 
  group_by(address.x) %>% 
  summarize(closest = min(distances)) %>% 
  ggplot(aes(x = closest))+
  geom_histogram(fill = 'pink',
                 color='black',
                 binwidth = 2)

            