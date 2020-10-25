# ---  --- --- --- --- --- --- --- --- --- --- --- --- #
# gis.R
# imports and processes shapefiles/map data
# ---  --- --- --- --- --- --- --- --- --- --- --- --- #


# load shapfile data as sf---- 

arl.precinct <-
  st_read(
    file.path(shp)
  ) %>%
  st_as_sf()   # read as sf


arl.precinct <- st_transform(
  arl.precinct, 4269)
# set crs to 4269 or NAD83
# "+proj=lcc +lat_1=39.2 +lat_2=38.03333333333333 +lat_0=37.66666666666666 +lon_0=-78.5 +x_0=3500000.0001016 +y_0=2000000.0001016 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
#arl.precinct <- st_transform(arl.precinct, 4326)


# make varnames all lowercase
names(arl.precinct) <- str_to_lower(names(arl.precinct))



# combine two Precinct boundaries 
    # issue is that in the shapefiles these are listed seperately because they have different 
    # house districts but in the vote data they are counted as one.

jef2 <- arl.precinct %>% filter(prec_name == "Jefferson")
jef1 <- st_combine(jef2) # combined geometry



# replace Jefferson precinct geometry split by two house districts with singular jefferson precinct geometry 
arl.precinct$geometry[arl.precinct$prec_name == "Jefferson" & arl.precinct$house == 47 ] <- jef1
arl.precinct$geometry[arl.precinct$prec_name == "Jefferson" & arl.precinct$house == 49 ] <- jef1



# drop duplicates by generating random number that will be same every time 
set.seed(seed = 47, kind = NULL)
arl.precinct <- arl.precinct %>%
  arrange(objectid) %>%
  mutate(
    rand = runif(length(arl.precinct$objectid)),
    dup  = duplicated(prec_name)
  ) %>%
  filter( (dup==FALSE) == TRUE)



# check that there are 54 precincts
assert_that(nrow(arl.precinct) == 54)


# replace house variable with two house precincts 
arl.precinct$house[arl.precinct$prec_name == "Jefferson" ] <- "47/49"



saveRDS(arl.precinct,
        file = file.path(root.data, "precinct-gis.Rda"))