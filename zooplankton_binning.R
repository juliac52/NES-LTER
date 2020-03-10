# set up cut-off values 
breaks <- c(39.5,39.683,39.866,40.049,40.232,40.415,40.598,40.781,40.964,41.147,41.33)
# specify interval/bin labels
tags <- c("[39.5-39.683", "[39.683-39.866)", "[39.866-40.049)", "[40.049-40.232)", "[40.232-40.415)", "[40.415-40.598)","[40.598-40.781)", "[40.781-40.964)","[40.964-41.147)","[41.147-41.33")
# bucketing values into bins
group_tags_long <- cut(nes_zoo_long$lat, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)
# inspect bins
summary(group_tags)

group_tags_wide <- cut(nes_zoo_wide$lat, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags, mean)

nes_zoo_long$bins <- cut(nes_zoo_long$lat, breaks=breaks, labels=tags) 
#add column to nes_zoo_long denoting latitudinal bins 

#-----------------------------------------------------------------------

group_tags_long_ymd <- cut(nes_zoo_long_ymd$lat, 
                       breaks=breaks, 
                       include.lowest=TRUE, 
                       right=FALSE, 
                       labels=tags)s

