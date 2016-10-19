### Preparing the Budget Authority data file for my tile tutorial
### KSF Fall 2016

#load the datafile
BAbyAgency_FY16 <- read.delim("hist05z2.txt", sep="\t", header=T, as.is=T) #we want strings not factors
#use tidyr to reshape the data into a more useful format, then clean up the year names and the number formats, filter out the total row and the subrows to Undistributed Offsetting Receipts
BAbyAgency_FY16_flat <- gather(BAbyAgency_FY16, "year", "millions", 2:ncol(BAbyAgency_FY16)) %>%
  transmute(agency = Department.or.other.unit
            , year=gsub("X","",year)
            , millions=as.numeric(gsub("[[:punct::]","",millions))) %>%
  filter(!grepl("Total|O[nf]+-budget", agency))
##TODO: not sure why I'm getting the attribute warning

##### We'll restrict the data to 2015, the last year that was not an estimate. 
##### We'll also just look at the top 10 agencies, rolling everything else into Other.

## some parameters
target.year <- "2015"
target.n <- 10

######### setup top.n dataframe
  targetdata.source <- subset(BAbyAgency_FY16_flat, year == target.year & !is.na(millions)) %>% #grab just target.year
                     mutate(agency = gsub("\\s*\\(.*\\)$","",agency)) %>% #merge the agencies that are divided into (On-Budget) and (Off-Budget) lines
                     group_by(agency) %>% summarise(millions=sum(millions)) %>% #group by agency_new and sum
                     arrange(desc(millions)) #order by size
  #get the top n by total budget amount
  targetdata.top <- top_n(targetdata.source, target.n, millions)
  targetdata.top <- rbind(targetdata.top,
                        data.frame(agency="Other", millions = sum(targetdata.source$millions) - sum(targetdata.top$millions)))

