# install.packages("RPostgreSQL")
require("RPostgreSQL")
require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "calart12"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "Chalk Streams",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

# check for a table
dbExistsTable(con, "ep_incidents")
# TRUE

#lowercase names in table and columns
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

#############################load table and put all columns to lowercase#############################################################
determinands <- as.data.frame(dbGetQuery(con, "SELECT * FROM determinands"))
effluents <- as.data.frame(dbGetQuery(con, "SELECT * FROM effluents"))
incidents <- as.data.frame(dbGetQuery(con, "SELECT * FROM ep_incidents"))
pollutant <- as.data.frame(dbGetQuery(con, "SELECT * FROM ep_pollutant"))
ngr.coord <- as.data.frame(dbGetQuery(con, "SELECT * FROM ngr_coord"))
  
colnames(incidents) <- dbSafeNames(colnames(incidents))
colnames(pollutant) <- dbSafeNames(colnames(pollutant))
colnames(determinands) <- dbSafeNames(colnames(determinands))
colnames(effluents) <- dbSafeNames(colnames(effluents))
colnames(ngr.coord) <- dbSafeNames(colnames(ngr.coord))

#write tables











#############################load determinands and effluents tables and merge on common columns#############################################
# length(effluents$PERMIT_REF) == length(unique(effluents$PERMIT_REF)) 
# 
# disch_det <- merge(determinands, effluents, by = c("permit_ref", "version","effluent_number"))
# disch_det <- disch_det[, - c(19:21)]
# disch_det$MONTH_FROM <-as.numeric(disch_det$MONTH_FROM)
# disch_det$MONTH_TO <- as.numeric(disch_det$MONTH_TO)
# disch_det$month_span <- disch_det$MONTH_TO - disch_det$MONTH_FROM
# 
# colnames(disch_det) <- dbSafeNames(colnames(disch_det))
# 
# # write table to database
# dbWriteTable(con,'effluents_with_determinands',disch_det, row.names=FALSE)


#############################load consented discharge and  effluents #####################################################################
disch_det <- dbGetQuery(con, "SELECT * FROM effluents_with_determinands")

#select just septic tanks
descriptor_tmen <- disch_det %>%
   tbl_df() %>%
   select(permit_ref, region_x, tmen_desc)

descriptor_tmen <- unique(descriptor_tmen)

descriptor_tmen$tmen_desc <- as.factor(descriptor_tmen$tmen_desc)
septictank <- unique(subset(descriptor_tmen, grepl("SEPTIC", tmen_desc)))

#write table to db
dbWriteTable(con,'septic_tank',septictank, row.names=FALSE)

#plot
png(filename = "T:/Proposals/WWF Polution & Chalk Streams/output plot/septic tanks EA Region.png",width=1920,height=1080,res=150)
ggplot(data = septictank,aes(x=region_x, fill = region_x))+
  geom_bar(colour="black") +
  geom_text(stat='count',aes(label=..count..),vjust=-1, size = 3) +
  scale_x_discrete(breaks = 1:31)+
  theme(legend.position="bottom") +
  facet_wrap(~tmen_desc,ncol = 4) +
  labs(fill = "") + xlab("")
dev.off()

######################### sewage incidents and pollutant ############################################################################
#merge and subset per SEwage
pollution.incidents <- merge(incidents,pollutant, all.x = T)
dbWriteTable(con,'all_pollution_incidents',pollution.incidents, row.names=FALSE)


sewage.incidents <- subset(pollution.incidents, grepl("Sew",poll_type) & !grepl("Wales", region_wm))
sewage.incidents <- subset(sewage.incidents, !sewage.incidents$region_wm == " ")

png(filename = "T:/Proposals/WWF Polution & Chalk Streams/output plot/sewage incidents.png",width=1920,height=1080,res=150)
ggplot(data = sewage.incidents,aes(x=region_wm, fill = region_wm))+
  geom_bar(colour="black") +
  geom_text(stat='count',aes(label=..count..),vjust=-1, size = 3) +
  scale_x_discrete(breaks = 1:31)+
  theme(legend.position="bottom" ) +
  facet_wrap(~eil_water,ncol = 4) +
  labs(fill = "") + xlab("")
dev.off()

#by year
sewage.incidents$year <- as.numeric(format(sewage.incidents$not_date,"%Y"))
sewage.incidents$month <- format(sewage.incidents$not_date,"%Y- %m")

png(filename = "T:/Proposals/WWF Polution & Chalk Streams/output plot/sewage incidents by year.png",width=1920,height=1080,res=150)
ggplot(data = sewage.incidents,aes(x=eil_water, fill = eil_water))+
  geom_bar(colour="black") +
  geom_text(stat='count',aes(label=..count..),vjust=-1, size = 3) +
  scale_x_discrete(breaks = 1:31)+
  theme(legend.position="bottom" ) +
  facet_wrap(~year,ncol = 4) +
  labs(fill = "") + xlab("")
dev.off()

#trendlines by year and Region wm 
sewage.count <- count(sewage.incidents, year, eil_water)
png(filename = "T:/Proposals/WWF Polution & Chalk Streams/output plot/sewage incidents by year (trendline).png",width=1920,height=1080,res=150)
ggplot(sewage.count, aes(year, n,  fill = eil_water)) + geom_line(aes(color = eil_water)) +
  xlab("") + ylab("Count") + theme(legend.position = "bottom", legend.title=element_blank()) 
dev.off()

sewage.region <- count(sewage.incidents, year, region_wm, eil_water)
png(filename = "T:/Proposals/WWF Polution & Chalk Streams/output plot/sewage incidents by year (trendline by region).png",width=1920,height=1080,res=150)
ggplot(sewage.region, aes(year, n,  fill = eil_water)) + geom_line(aes(color = eil_water))+ facet_wrap(~region_wm, ncol = 2) +
  xlab("") + ylab("Count") + theme(legend.position = "bottom",legend.title=element_blank())
dev.off()

#by type and by type per region
png(filename = "T:/Proposals/WWF Polution & Chalk Streams/output plot/sewage incidents by type.png",width=1920,height=1080,res=150)
ggplot(data = sewage.incidents,aes(x=pollutant, fill = pollutant))+
  geom_bar(colour="black") +
  geom_text(stat='count',aes(label=..count..),vjust=-0.25, size = 3) +
  scale_x_discrete(breaks = 1:31)+
  theme(legend.position="bottom" ) +
  facet_wrap(~year,ncol = 8) +
  labs(fill = "") + xlab("")
dev.off()

png(filename = "T:/Proposals/WWF Polution & Chalk Streams/output plot/sewage incidents by type per region.png",width=1920,height=1080,res=150)
ggplot(data = sewage.incidents,aes(x=pollutant, fill = pollutant))+
  geom_bar(colour="black") +
  geom_text(stat='count',aes(label=..count..),vjust=-0.25, size = 3) +
  scale_x_discrete(breaks = 1:31)+
  theme(legend.position="bottom" ) +
  facet_wrap(~region_wm,ncol = 4) +
  labs(fill = "") + xlab("")
dev.off()


