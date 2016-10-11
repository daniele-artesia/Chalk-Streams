# install.packages("RPostgreSQL")
require("RPostgreSQL")

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
dbExistsTable(con, "determinands")
# TRUE

#load determinands and effluents tables and merge on common columns
determinands <- dbGetQuery(con, "SELECT * FROM determinands")
effluents <- dbGetQuery(con, "SELECT * FROM effluents")

length(effluents$PERMIT_REF) == length(unique(effluents$PERMIT_REF)) 

disch_det <- merge(determinands, effluents, by = c("PERMIT_REF", "VERSION","EFFLUENT_NUMBER"))
disch_det <- disch_det[, - c(19:21)]
disch_det$MONTH_FROM <-as.numeric(disch_det$MONTH_FROM)
disch_det$MONTH_TO <- as.numeric(disch_det$MONTH_TO)
disch_det$month_span <- disch_det$MONTH_TO - disch_det$MONTH_FROM

#lowercase names in table and columns
dbSafeNames = function(names) {
  names = gsub('[^a-z0-9]+','_',tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

colnames(disch_det) <- dbSafeNames(colnames(disch_det))

# write table to database
dbWriteTable(con,'determinands_effluents',disch_det, row.names=FALSE)
