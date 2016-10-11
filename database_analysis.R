# install.packages("RPostgreSQL")
require("RPostgreSQL")
require(dplyr)
require(reshape2)
require(ggplot2)

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
dbExistsTable(con, "determinands_effluents")
# TRUE

#lowercase names in table and columns
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

#############################load determinands and effluents tables and merge on common columns#############################################
determinands <- dbGetQuery(con, "SELECT * FROM determinands")
effluents <- dbGetQuery(con, "SELECT * FROM effluents")

length(effluents$PERMIT_REF) == length(unique(effluents$PERMIT_REF)) 

disch_det <- merge(determinands, effluents, by = c("PERMIT_REF", "VERSION","EFFLUENT_NUMBER"))
disch_det <- disch_det[, - c(19:21)]
disch_det$MONTH_FROM <-as.numeric(disch_det$MONTH_FROM)
disch_det$MONTH_TO <- as.numeric(disch_det$MONTH_TO)
disch_det$month_span <- disch_det$MONTH_TO - disch_det$MONTH_FROM

colnames(disch_det) <- dbSafeNames(colnames(disch_det))

# write table to database
dbWriteTable(con,'determinands_effluents',disch_det, row.names=FALSE)


#############################load consented discharge and  effluents #####################################################################
disch_det <- dbGetQuery(con, "SELECT * FROM determinands_effluents")

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

#
