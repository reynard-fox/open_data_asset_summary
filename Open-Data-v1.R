# title: "Open Data v1"
# author: "Michael Anderson"
# date: "3/22/2022"

# >> Run either "LOOP BLOCK" (lines 386-448) or "ALT LOOP BLOCK" (lines 451-467)
# >> A Socrata "token" is needed for the loop on lines 404 - 440
# >> A read.csv command needs a path (for the save output from the loop) on line 458

#---------------------------------------------------------
# NYC OpenData
#---------------------------------------------------------
# NYC Open Data is a large collection of free data sets published by the City of New York. 
# These tables and resources can be used to understand how the city operates. There are a 
# lot of great resources you can use to understand the history of OpenData and the basics 
# you'll need to perform operations on datasets. 

# [BetaNYC](https://beta.nyc/) has some great resources, including an Open Data 101 course 
# called [Introduction to Open Data](http://bit.ly/BetaNYC_Mozfest), which has a great section 
# on the history of OpenData in NYC. 

# I'm going to access these datasets using R/RStudio using Socrata URL queries. Other authors 
# use Python for their projects, and many of these seem to use more explicit HTTP methods to access data. 

# [OpenData](https://opendata.cityofnewyork.us/) provides a number of useful ["how-to"]
# (https://opendata.cityofnewyork.us/how-to/) pages to help you:
  
# 1. [Get started](https://dev.socrata.com/consumers/getting-started.html),
# 2. Set up [URL queries](https://dev.socrata.com/docs/queries/), and
# 3. Navigate [API endpoints](https://dev.socrata.com/docs/endpoints.html).

#---------------------------------------------------------
## Project Overview
#---------------------------------------------------------
# The aim of this analysis is to provide a high-level overview of NYC OpenData. To start our 
# investigation we will do some basic data cleaning, followed by a review available datasets. 
# Then I'll briefly explore what agencies publish the most data, what datasets are the most popular, 
# and which datasets are the largest. I'll conclude with next steps and present some ideas for 
# how to improve future investigations.

# The appendices contain stuff that really didn't fit anywhere else. To be honest, I probably 
# should have included more in the appendices and turned this extra material into it's own project. 
# Appendix A contains some useful bits of code that helped me along the way, and Appendix C is a 
# summary of the packages used to complete this work. Appendix B contains information on some statistical 
# tests for normality. The normality analysis really needs additional work. 

# This project took a while to finish. I started out going in one direction and paused to go in another. 
# In all I spent about 10-14 hours per week for 6-7 weeks on this, and even though this implies a hour 
# range of 60-120 hours (yikes!), I probably spent 80-90 hours on this. 

#---------------------------------------------------------
### Table Inventory
#---------------------------------------------------------
# A great place to gets started is with a review of the [Local Law 251 of 2017: 
# Published Data Asset Inventory](https://data.cityofnewyork.us/resource/5tqd-u88y). 
# This table contains information on all the datasets hosted in NYC OpenData, and allows 
# us to see which datasets are the most popular or which agencies are open data champions. 
# To start this review let's set up our workspace. 

# In a regular script I frequently cleanup my workspace before working. 
rm(list=ls())

# The libraries I'll use for this project. I'm going to suppress all the normal warnings these packages typically create.
suppressWarnings(suppressPackageStartupMessages({
  library(RSocrata)
  library(jsonlite)
  library(tidyverse)
  library(ggplot2)
  library(waffle)
  library(ggpubr)
  library(httr)
  library(kableExtra)
  library(knitr) # tables & formating
  library(magrittr) # tables & formating
  library(visdat) # large table summary
  library(DT) # html widgets
}))

# In a regular script I would also include these two commands to set my working directory. 
script_name <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(script_name))

# My token for Socrata, in case I'm throttled.
token <-"***************************"

# Now let's start our review of all the Published Data Asset Inventory. Two things to note: 
# 1. I haven't used my token and I'm not limited to some finite number rows in this pull, 
# nor do I have to skip my way through to pull all the rows. This is a little different from Python, 
# which seems to require skipping through the dataset to download all rows.
# 2. I also shortened column names as much as practically possible. 

# Local Law 251 of 2017: Published Data Asset Inventory
all_open_data_sets <- "https://data.cityofnewyork.us/resource/5tqd-u88y.json?$select=*"
all_open_data_sets <- read.socrata(all_open_data_sets)

# I'm going to rename these columns so they take up less space. 
rename(all_open_data_sets,
       data.agency = datasetinformation_agency,
       uid = uid,
       name = name,
       description = description,
       type = type,
       category = category,
       from.opendata.plan = legislativecompliance_datasetfromtheopendataplan,
       url = url,
       date.public = update_datemadepublic,
       update.freq = update_updatefrequency,
       last.update = last_data_updated_date,
       can.automate = legislativecompliance_candatasetfeasiblybeautomated,
       automated = update_automation,
       has.dictionary = legislativecompliance_hasdatadictionary,
       contain.address = legislativecompliance_containsaddress,
       geo.coded = legislativecompliance_geocoded,
       exists.externally = legislativecompliance_existsexternallyll1102015,
       external.freq = legislativecompliance_externalfrequencyll1102015,
       records.removed = legislativecompliance_removedrecords,
       visits = visits,
       row.count = row_count,
       column.count = column_count,
       derived.view = derived_view,
       parent.uid = parent_uid) -> all_open_data_sets

#---------------------------------------------------------
### Table Summary
#---------------------------------------------------------
# Here's a quick peak at first values from this table. Nothing unusual here - the table is 
# small (about 3400 rows and 2 dozen columns), most fields are formatted as character (including numbers), 
# and around one-third of the dataset contains binary values.

# A quick peak at the data
glimpse(all_open_data_sets)

#---------------------------------------------------------
#### Sample Data
#---------------------------------------------------------
# Here's a quick peak at some of the first values from this table. 

# Sample 
all_open_data_sets[c(2,5,17),] %>%
  kbl() %>%
  kable_styling() %>%
  scroll_box(width = "900px", height = "400px")

#---------------------------------------------------------
#### A `visdat` Graphic of Missing Data
#---------------------------------------------------------
# There's not a lot of data missing here, but two columns stick out - row.count 
# and column.count. Roughly a quarter of these values are missing. We'll dig into 
# this a bit more later. The field parent.uid is also mostly empty. We'll also dig 
# into this later too, our fix for these missing values is connected to the missing row/column values.

# A graph of missing data in the whole dataset
vis_miss(all_open_data_sets) +
    theme(axis.text.x = element_text(angle = 90))

#---------------------------------------------------------
#### Review of the *Published Data Asset Inventory* Table
#---------------------------------------------------------
# This table being reviewed - Published Data Asset Inventory - has data formatting problems 
# that we'll fix and some that we won't. Most issues are related to the consistent use of 
# "Yes" and "No", and a few are connected to missing data.

# Create an empty table to hold our information.
table_fix_summary <- data.frame("New.Column.Name"=character(),
                                "Description"=character(),
                                "Cleaning.Needed"=character(),
                                stringsAsFactors=FALSE)

# Load the table with data. 
table_fix_summary <- rbind(table_fix_summary,
c('data.agency','Agency that owns and maintains the dataset.','Yes. Some names are too long, others are not consistent, and are some missing.'),
c('uid','Unique identifier of the released dataset on the portal.','No'),
c('name','The name of the asset.','No'),
c('description','The description of the asset.','No'),
c('type','Datatype of the asset.','No'),
c('category','The asset category, if provided.','No'),
c('from.opendata.plan','Whether the asset was ever listed on the Open Data plan.','Yes. Some values are not consistent.'),
c('url','The web URL to view the asset.','No'),
c('date.public','The Date a dataset was first made public.','No'),
c('update.freq','How often the data will be updated.','No'),
c('last.update','The date the data asset was last updated, which will not reflect metadata updates.','No'),
c('can.automate','Whether the dataset updates can be automated on the NYC Open Data Portal.','Yes. Some values are not consistent.'),
c('automated','Whether the dataset updates are automated on the NYC Open Data Portal.','Yes. Some values are not consistent.'),
c('has.dictionary','Whether a data dictionary is attached or there is an URL link to the data dictionary hosted outside of the NYC Open Data Portal.','Yes. Some values are not consistent.'),
c('contain.address','Whether dataset contains address information that includes House Number, Street Name, Postcode, and/or Borough.','Yes. Some values are not consistent.'),
c('geo.coded','Whether dataset contains geospatial columns as per Local Law 108 of 2015.','Yes. Some values are not consistent.'),
c('exists.externally','Does the data source for the dataset exist on the agency website.','No'),
c('external.freq','If data source for the dataset exists externally (agency website), how often is it being updated on the agency website.','No'),
c('records.removed','Are records removed upon update of this dataset.','Yes. Some values are not consistent.'),
c('visits','The number of times the asset has been viewed.','No'),
c('row.count','The number of rows in the asset, missing if the asset does not have tabular data.','No'),
c('column.count','The number of columns in the asset, missing if the asset does not have tabular data.','No'),
c('derived.view','For derived asset, the unique identifier of their parent dataset, missing for default assets with no parent.','No'),
c('parent.uid','Whether the asset derives from another asset.','No')
)

# Rename the columns again (don't know why they're removed in the first place...).
names(table_fix_summary) <- c("New.Column.Name","Description","Cleaning.Needed")

# Create a nice table.
table_fix_summary %>% kable(align = "l") %>% kable_styling(bootstrap_options = "bordered",full_width = FALSE)

#---------------------------------------------------------
### Data Cleaning
#---------------------------------------------------------
# Key Steps
# 1. Create a shorter version of the variable `data.agency`.
# 2. Clean Yes/No variables.
# 3. Convert text to numbers.
# 4. Locate missing row and column number values. 

#---------------------------------------------------------
#### Step 1 - Simplify `data.agency`
#---------------------------------------------------------
# Recode NA as missing.
all_open_data_sets$data.agency <- trimws(ifelse(is.na(all_open_data_sets$data.agency), 
                                                              "Missing",all_open_data_sets$data.agency))
# Extract short name for graphing.
all_open_data_sets$dataset.owner <- stringr::str_extract(string =  all_open_data_sets$data.agency,
                                                         pattern = "(?<=\\().*(?=\\))")

# Shorten names for graphing, and reorder columns so the short, abbreviation "dataset.owner" is next to full name "data.agency".
all_open_data_sets %>% 
  mutate(dataset.owner = 
     case_when(
      data.agency == "Other" ~ 'OTHER',
      data.agency == "Mayor's Office of Climate and Sustainability" ~ "SUSTAIN",
      data.agency == "Mayor's Office for Economic Opportunity" ~ "ECON-OPP",
      data.agency == "Mayor’s Office to End Domestic and Gender-Based Violence" ~ "END-GBV",
      data.agency == "Missing" ~ "Missing",
      data.agency == "311" ~ "311",
      data.agency == "Mayor's Office of Climate Resiliency" ~ "CLIMATE",
      data.agency == "Civil Service Commission" ~ "CSC",
      data.agency == "NYC Health + Hospitals" ~ "H+H",
      data.agency == "Public Design Commission (formerly Art Commission (ARTCOM) and Design Commission)" ~ "PDC",
      data.agency == "HHS Accelerator" ~ "HHS-ACCEL",
      data.agency == "Mayor's Office of Climate Policy and Programs" ~ "CLIMATE-POLICY",
      data.agency == "NYC Recovery" ~ "RECOVERY",
       TRUE ~ dataset.owner)) %>%
  relocate(dataset.owner, .after = data.agency) -> all_open_data_sets

#---------------------------------------------------------
#### Step 2 - Clean Yes/No Data
#---------------------------------------------------------
# There are too many Yes/No variants. Let's find all possible variations of in the Yes/No variables.
yes_no <- c(unlist(all_open_data_sets$from.opendata.plan),
            unlist(all_open_data_sets$records.removed),
            unlist(all_open_data_sets$can.automate),
            unlist(all_open_data_sets$automated),
            unlist(all_open_data_sets$has.dictionary),
            unlist(all_open_data_sets$contain.address),
            unlist(all_open_data_sets$geo.coded)) 

# The combined list of *dirty* Yes/No options.
yes_no <-  as.data.frame(yes_no)
names(yes_no) <- 'yes_no'
yes_no %>% group_by(yes_no) %>% summarize(count=n()) %>% arrange(desc(count)) -> old_yes_no_summary

# The yes/no fields contain inconsistently formatted values. For example, 
# `unique(all_open_data_sets$from.opendata.plan)` returns this list of variants like "NO", "Yes", 
# "No", "YES", "N/A", and NA. If the yes/no data was clean we'd see a clean split, but we don't. 
# There's actually a medium amount of variation in yes/no values. While it's true that there are no 
# simple `T` or `F` values, we have just about every other version of `Yes` and `No` present. Let's clean these.

# A yes/no summary table.
kbl(old_yes_no_summary,
    position = "left", align=c('c','c'),
    caption = 'Dirty Yes/No Data') %>%
  kable_paper(full_width = F, position = "left") %>%
  column_spec(1, width = "8em", bold = T, border_right = T) %>%
  column_spec(2, width = "8em")

# Clean yes/no values 
dirty <-  c(NA ,""  ,"N/A","No","NO","yes","Yes","YES")
clean <- c("NO","NO","NO" ,"NO","NO","YES","YES","YES")
all_open_data_sets$from.opendata.plan <- clean[match(all_open_data_sets$from.opendata.plan,dirty)]
all_open_data_sets$records.removed <- clean[match(all_open_data_sets$records.removed,dirty)]
all_open_data_sets$exists.externally <- clean[match(all_open_data_sets$exists.externally,dirty)]
all_open_data_sets$can.automate <- clean[match(all_open_data_sets$can.automate,dirty)]
all_open_data_sets$automated <- clean[match(all_open_data_sets$automated,dirty)]
all_open_data_sets$has.dictionary <- clean[match(all_open_data_sets$has.dictionary,dirty)]
all_open_data_sets$contain.address <- clean[match(all_open_data_sets$contain.address,dirty)]
all_open_data_sets$geo.coded <- clean[match(all_open_data_sets$geo.coded,dirty)]

# There are too many Yes/No variants. Let's find all possible variations of in the Yes/No variables.
yes_no <- c(unlist(all_open_data_sets$from.opendata.plan),
            unlist(all_open_data_sets$records.removed),
            unlist(all_open_data_sets$can.automate),
            unlist(all_open_data_sets$automated),
            unlist(all_open_data_sets$has.dictionary),
            unlist(all_open_data_sets$contain.address),
            unlist(all_open_data_sets$geo.coded)) 

# The combined list of *clean* Yes/No options.
yes_no <-  as.data.frame(yes_no)
names(yes_no) <- 'yes_no'
yes_no %>% group_by(yes_no) %>% summarize(count=n()) %>% arrange(desc(count))-> new_yes_no_summary

# With clean `YES` and `NO` values, the table is smaller and contains just two rows. 

# A clean yes/no summary table.
kbl(new_yes_no_summary,
    position = "left", align=c('c','c'), 
    caption = 'Cleaned Yes/No Data') %>%
  kable_paper(full_width = F, position = "left") %>%
  column_spec(1, width = "8em", bold = T, border_right = T) %>%
  column_spec(2, width = "8em")

#---------------------------------------------------------
#### Step 3 - Text to Numbers
#---------------------------------------------------------
# Now we will explicitly convert visits to numbers. 

# Code visits, columns, and rows as numbers (a very short step)
all_open_data_sets$visits <- as.numeric(all_open_data_sets$visits) 
all_open_data_sets$row.count <- as.numeric(all_open_data_sets$row.count)
all_open_data_sets$column.count <- as.numeric(all_open_data_sets$column.count)

#---------------------------------------------------------
#### Step 4 - Locate Missing Row/Column Data
#---------------------------------------------------------
# The asset inventory table contains some fun descriptive measures like the 
# amount of data published by each agency, the number of times a dataset has been visited, 
# and the number of datasets published by each agency. While reviewing the amount of data 
# published by each agency I noticed a problem - some datasets were missing a count of rows 
# and columns. I happened to stumble across this while reviewing data for the TLC. Here's an example.

# Matrix size of each dataset
all_open_data_sets$size <- as.numeric(all_open_data_sets$row.count) * as.numeric(all_open_data_sets$column.count)

# An example of missing size data
all_open_data_sets %>% 
  dplyr::select(dataset.owner,uid,name,type,visits,row.count,column.count,derived.view,parent.uid,size) %>%
  filter(uid %in% c('5gj9-2kzx','7rnv-m532')) -> size_example

# Here are two tables, one with a row/column count and one without. We can take a look at whether 
# this information is really missing in OpenData using the uid (or parent.uid) for each table. 
# The URL pattern to follow is `valid_url` = `root_url` + `uid`, where 
# the `root_url` = *https://data.cityofnewyork.us/resource/*. 

# Pull a sample of data to illustrate point about missing data and uid/parent.uid
row.names(size_example) <- NULL
size_example$row.count = cell_spec(size_example$row.count, color = ifelse(is.na(size_example$row.count), "red", "black"))
size_example$column.count = cell_spec(size_example$column.count, color = ifelse(is.na(size_example$column.count), "red", "black"))
size_example$parent.uid = cell_spec(size_example$parent.uid, color = ifelse(is.na(size_example$parent.uid), "red", "black"))
size_example %>%
  kbl(escape=FALSE) %>%
  kable_paper("hover", font_size = 12) %>%
  column_spec(2, background = "lightcyan") %>%
  column_spec(6, background = "lightcyan") %>%
  column_spec(7, background = "lightcyan") %>%
  column_spec(9, background = "lightcyan")

# Casual observation - some reason older tables, especially ones that have a parent.uid 
# listed, are missing row and column counts. At the time I saw this I was bothered by the 
# error and went down a rabbit hole looking into it. I still don't know why the problem exits, 
# but I have a solution - when row and column counts are missing, and a parent.uid is present, 
# use the row/column counts of the parent.uid. Based on my review, `parent.uid` references the 
# same dataset as the `uid` record, but with more metadata in a computer friendly format. 

#--------------------------------------------------------------
##### Using `uid` and `parent.uid` to Locate Row/Column Data
#--------------------------------------------------------------
# Now that I have an idea for how to fix the problem, let's go about solving the issue.  
# I started by creating a list possible URL and then looped through them. My first approach 
# was to using queries to pull missing row and column values. To find the number of columns, 
# I would select 1 row from a dataset using `limit 1` in my URL SoQL query, and to find the number 
# of rows I used a `count(*)`. I pulled column and row counts for every table, compared them with 
# the OpenData values in the source dataset, and reviewed a sample of discrepancies. 

# In many cases I was able to fill in gaps in information. However, in a few cases I found errors 
# that were the result of bad assumptions on my part. For these errors, my row counts were fine but 
# a the column counts were off. Why? Occasionally the first row of a dataset was missing values and 
# so a subset of total columns were returned from my `limit 1` and I recorded the subset as the actual 
# column number. The fast (and inefficient) solution was to expand my limit to some larger number, I 
# chose 200 records, and hope that the larger number ensured a complete set of columns for at least one row. 
# However, this solution dramatically increased the time needed to run the loop. 

# In the end I compromised and used a combination of `http` and `query` methods. First, I counted the 
# field types returned in the HTTP header. If the header didn't included headers then I would move onto 
# step two and count columns after querying the first 200 rows (`limit 200`). I used a try-catch function 
# to split the work into the first/second buckets, and handle errors that would otherwise cause the script 
# to stop. The script still takes a solid hour or two to run.

# I also ran the loop a few times to fix problems. I would cut the loop down to 5 or 10 records before 
# expanding the range to include all tables. I ran the full loop a few times, saved the data, and loaded it 
# in separately in this markdown to speed up the rendering of the final HTML (which is this one, the one you 
# are reading). The size loop required around 1.5 hours to complete. 


####-| SART LOOP BLOCK |-####
# This loop takes a while to run. Eventually I saved the output and just loaded it instead of 
# running the loop block. 

# Let's form new URLs - use the parent.uid when it's present, otherwise use the uid.
all_open_data_sets %>%
  mutate(new.uid = case_when(is.na(parent.uid) ~ uid,TRUE ~ parent.uid)) %>%
  mutate(new.url = paste0("https://data.cityofnewyork.us/resource/",new.uid,".json")) -> all_open_data_sets

# Create empty columns to hold new row/column data
all_open_data_sets$column.new <- ""
all_open_data_sets$row.new <- ""

# Turn off warnings - there are a number of inconsequential conversion errors at the end that 
# don't affect the results and just make things look messy.
defaultW <- getOption("warn") 
options(warn = -1) 

# ----[ START loop ]----
for (i in 1:nrow(all_open_data_sets)){
# for (i in 1:10){
  
  # Get http data for url
  t0 <- all_open_data_sets$new.url[i]
  t1 <- GET(t0)
  
  # If status is OK then let's get the number of COLUMNS
  if(t1$status_code==200){
    
    t2 <- tryCatch({
      length(fromJSON(t1$headers[["x-soda2-types"]])) # Use the http header FIRST
    }, warning = function(w) {
      print('warning')
    }, error = function(e) {
      ncol(read.socrata(paste0(t0,'?$limit=200'),token)) # Use the first 200 rows SECOND
    }, finally = {
    })
    
    t2 <- ifelse(is.null(t2),NA,t2) # If no data is there (in the case of a map uid, for example), 
    # enter NA for the number of columns
    all_open_data_sets$column.new[i] <- t2
    
    u1 <- read.socrata(paste0(t0,'?$select=count(*)'),token) # Read the number of rows
    all_open_data_sets$row.new[i] <- u1[[1]]
    
    cat("\r main ok",i,rep(" ",150)) # A counter I can use to track progress
  }
  else { # If the first step fails, use NA for both row/column values
    all_open_data_sets$column.new[i] <- NA
    all_open_data_sets$row.new[i] <- NA
    
    cat("\r else na",i,rep(" ",150)) # Another counter to track progress
  }
}
# ----[ STOP loop ]----

# Turn warnings back on
options(warn = defaultW)

# write data
#write.csv(all_open_data_sets,"all_open_data_sets_STEP_17_DATA_2022-03-05.csv")

####-| END LOOP BLOCK |-####


#---{ START ALT LOOP BLOCK }
# Let's form new URLs - use the parent.uid when it's present, otherwise use the uid.
all_open_data_sets %>%
  mutate(new.uid = case_when(is.na(parent.uid) ~ uid,TRUE ~ parent.uid)) %>%
  mutate(new.url = paste0("https://data.cityofnewyork.us/resource/",new.uid,".json")) -> all_open_data_sets

# COL from IMPORTED DATA: uid,  column.new, row.new | # read previously pulled new row/column data 
temp <- read.csv("URL TO SAVED FILE")
temp %>% group_by(uid) %>% summarise(n=sum(n())) %>% filter(n>1) # should return NOTHING
temp %>% dplyr::select(uid,column.new,row.new) -> temp
left_join(all_open_data_sets,temp, by="uid", keep=TRUE) -> x
x$uid.y <- NULL
colnames(x)[colnames(x) == 'uid.x'] <- 'uid'
all_open_data_sets <- x
rm(x,temp)

#---{ STOP ALT LOOP BLOCK }

# Let's calculate new size values using our old and new row/column counts. 
# Size = ROW * COLUMN = Total cells | Size calculation using old, provided values.
ifelse(is.na(all_open_data_sets$row.count) | is.na(all_open_data_sets$column.count), NA, 
       as.numeric(all_open_data_sets$row.count)*as.numeric(all_open_data_sets$column.count)) -> 
  all_open_data_sets$old.size 

# Size calculation using new, recently pulled values.
ifelse(is.na(all_open_data_sets$row.new) | is.na(all_open_data_sets$column.new), NA, 
       as.numeric(all_open_data_sets$row.new)*as.numeric(all_open_data_sets$column.new)) ->
  all_open_data_sets$new.size

# Prep data to look at how size calculations changed
all_open_data_sets %>% mutate(new_BIGGER_old = ifelse(new.size > old.size & is.na(new.size) == F & is.na(old.size) == F, 1,0),
                              new_SAME_old = ifelse(new.size == old.size & is.na(new.size) == F & is.na(old.size) == F, 1,0),
                              new_SMALLER_old = ifelse(new.size < old.size & is.na(new.size) == F & is.na(old.size) == F, 1,0),
                              new_NA_old_NOT = ifelse(is.na(new.size) == T & is.na(old.size) == F, 1,0),
                              new_NOT_old_NA = ifelse(is.na(new.size) == F & is.na(old.size) == T, 1,0),
                              new_NA_old_NA = ifelse(is.na(new.size) == T & is.na(old.size) == T, 1,0)
                              ) %>%
   summarize(new_BIGGER_old = sum(new_BIGGER_old),
             new_SAME_old = sum(new_SAME_old),
             new_SMALLER_old = sum(new_SMALLER_old),
             new_NA_old_NOT = sum(new_NA_old_NOT),
             new_NOT_old_NA = sum(new_NOT_old_NA),
             new_NA_old_NA = sum(new_NA_old_NA)
             ) %>% 
  pivot_longer(cols = starts_with("new"),
               names_to = "Summary",
               values_to = "Count",
               values_drop_na = TRUE) -> size_difference

# Format table to show results  
size_difference %>% 
  kable(align = "l",caption = 'New vs Old Size Data') %>% 
  kable_styling(bootstrap_options = "bordered",full_width = FALSE)


# Let's see how our size estimates changed after updating our row/column values. The 
# results are, frankly, mixed. I made two types of comparisons: (1) existing tables that 
# changed size, and (2) tables that came into (or left) existences. 

# * 3,036 datasets didn't change (87.5%). The sizes of 2,399 didn't change, and 637 uids didn't 
#   have any data under either method
# * 260 datasets increased in size (7.5%). 42 datasets grew, and 218 went from missing to not missing.
# * 171 datasets *decreased* in size (5.0%). 171 datasets decreased in size. 

# Most data tables didn't change in size, and nearly as many datasets shrank as expanded. updating 
# the size data was a lot of work, and a significant diversion (the script takes around 1.0-1.5 hours 
# to finish executing). However, now that all of our data is cleaned we can now move onto some basic 
# analysis of the data. 

#--------------------------------------------------------------
### Basic Data Analysis
#--------------------------------------------------------------

#--------------------------------------------------------------
#### Size Improvements - New Row/Column Values vs Old Row/Column Values
#--------------------------------------------------------------

# The dataset sizes are very skewed. A regular histogram of the data is basically unreadable 
# (see the *Histogram of As-Is Size Data*). All the values are clustered around the low end, 
# with a smaller group of extreme values trailing out to the right. A log transformation makes 
# the distribution much easier to interpret. 

# Why did I choose a base 2 log transform? Because it allows for easier "memory" comparisons. 
# The size figures might represent a minimum data size: 1 cell = 1 byte (yes, I know 8 bits = 1 byte).
# Computer memory is described using base 2 units. For example, there are 2^10^ MB to 1 GB, and 2^10^ GB 
# (or 2^20^ MB) to 1 TB. So by transforming the date using a base 2 logarithm, the differences in orders 
# of magnitude begin to match the order of magnitude differences in data.

# Kilobyte (KB)	= 1,024 Bytes
# Megabyte (MB)	= 1,024 Kilobytes
# Gigabyte (GB)	= 1,024 Megabytes
# Terabyte (TB)	= 1,024 Gigabytes
# Petabyte (PB)	= 1,024 Terabytes

# We can see the modest improvement in size data. The new size data is blue, and the old size data is red. 
# This means that whenever a blue bar appears higher than a red bar, we have found new row/column values 
# that weren't there before.

# Histogram of as-is size data || fig.align='center', out.width='100%'
h1 <- hist((all_open_data_sets$new.size), breaks=40, plot = FALSE)
h2 <- hist((all_open_data_sets$old.size), breaks=40, plot = FALSE)
plot(h1, col='skyblue',border=F, main="Histogram of As-Is Size Data",xlab=(expression("Dataset Size (row " %*% " col)")),ylab="Dataset Count")
plot(h2,add=T,col=scales::alpha('red',.5),border=F)

# Base 2 Log histogram of size data 
h1 <- hist(log2(all_open_data_sets$new.size), breaks=40, plot = FALSE)
h2 <- hist(log2(all_open_data_sets$old.size), breaks=40, plot = FALSE)
plot(h1, col='skyblue',border=F, main="Histogram of Base 2 Log Size Data",xlab=(expression("Dataset Size (row " %*% " col)")),ylab="Dataset Count")
plot(h2,add=T,col=scales::alpha('red',.5),border=F)

# We see similar results with a line graph too - wherever light red appears this means new information 
# has been found, while light blue means our new size is lower than the old one. The raw data is highly 
# concentrated at the low end, but the log transform is more normal looking. 

# Density graph of as-is size data
all_open_data_sets %>% dplyr::select(new.size,old.size) -> test
test %>% pivot_longer( everything(), names_to = "name", values_to = "data") -> test
test$data <- ifelse(is.na((test$data)) | test$data == 0 , 0, (test$data))
ggplot(test, aes(x = data, fill = name)) + geom_density(alpha = 0.5) + 
  ggtitle("Density Graph of As-Is Size Data") + xlab(expression("Dataset Size (row " %*% " col)")) + ylab("Dataset Count")

# Density graph of log transformed data
all_open_data_sets %>% dplyr::select(new.size,old.size) -> test
test %>% pivot_longer( everything(), names_to = "name", values_to = "data") -> test
#test$data <- ifelse(is.na(log2(test$data)) | test$data == 0 , 0, log2(test$data)) # keeps ZEROs
test <- test %>% filter(data>0) %>% na.omit() %>%  mutate(data = log2(data)) # drops ZEROs
ggplot(test, aes(x = data, fill = name)) + geom_density(alpha = 0.5) +
  ggtitle("Density Graph of Base 2 Log Size Data") + xlab(expression("Dataset Size (row " %*% " col)")) + ylab("Dataset Count")

#--------------------------------------------------------------
##### Minor Detour - What type of distribution does the size data fit?
#--------------------------------------------------------------
# I wanted to see what type of distribution fits the size data. I knew there were some 
# extreme values to deal with, especially after looking at the "as-is" graph and the underlying 
# data too, and after the log-transform I wanted to see if this data is **log normal**. 
# I used the package [EnvStats](https://cran.r-project.org/web/packages/EnvStats/index.html) to 
# help analyze this data and to run some super basic tests.

# A large stat package to help estimate distributions
library("EnvStats")

# Filter size data - remove NA and any ZEROs (there should be no negatives)
all_open_data_sets %>% dplyr::select(new.size) %>% filter(new.size>0) %>% na.omit() -> size_dist_test

# Unlist and formate data into vector for analysis
as.vector(as.numeric(unlist(size_dist_test))) -> size_dist_test

# A function to choose the best distribution for the dataset
distChoose(size_dist_test, warn=FALSE) -> size_dist_test

# Print results of distChoose() function
cat(paste(paste0("-------- distChoose() Output --------"),
          paste0("Choosen Distribution: ",size_dist_test$decision),
          str_squish(size_dist_test$test.results$norm$alternative),
          str_squish(size_dist_test$test.results$gamma$alternative),
          str_squish(size_dist_test$test.results$lnorm$alternative),
          sep="\n"));

# I need to detach this package since it has several functions that are the same 
# as ones in dplyr and I don't want search for all duplicates.
detach("package:EnvStats", unload=TRUE)

# In order to see if the dataset is lognormal I need to remove *0* and *NA* values, unlist 
# (flatten) the dataset, convert to a vectored list of numbers. I used `EnvStats` library to 
# analyzed the data and relied on two functions - `gofTest()` and `distChoose `. The function 
# `gofTest()` only told me the data was not normal, so I moved on to `distChoice` which describes 
# he data as "nonparametric" which basically means it does not fit any of the tested distributions. 

# I suspect that the reason for the lack of fit is the absence of low values sizes, and a few 
# exceptionally large datasets. For example, the Parks Department table called "Daily Tasks Park 
# Cleaning Records" is a large dataset with new.uid = "kwte-dppd". This dataset has between 50 million 
# (original uid) and 583 million rows (new.uid), depending non what uid is used. When `EnvStats` 
# attempts to identify distribution the best describes the table sizes in the 
# [Published Data Asset Inventory](https://data.cityofnewyork.us/resource/5tqd-u88y), it treats "Daily Tasks 
# Park Cleaning Records" as though it were infinite and drops it. 

# In the interest of time I will put this analysis on pause and (perhaps) come back to this another time.

# The dataset that appears as INF for one distChoose()
all_open_data_sets %>% 
  dplyr::select(dataset.owner,name,update.freq,date.public,last.update,row.count,
                column.count,size,new.uid,new.url,row.new,column.new,old.size,new.size) %>% 
  dplyr::filter(new.uid == "kwte-dppd") %>% 
  kable(align = "l",format.args = list(big.mark = ",")) %>% 
  kable_styling(bootstrap_options = "bordered",full_width = FALSE) %>%
  scroll_box(width = "900px", height = "200px")

#--------------------------------------------------------------
#### Datasets by Agency - Number, Visits, and Size
#--------------------------------------------------------------
# Most datasets that are not in Open Data Plan and seem to be released voluntarily. 
# More than half of the datasets are not in the Open Data Plan.

# Now summarize how many datasets are voluntary (yes) or not (no). Most data sets look voluntary!
all_open_data_sets %>% group_by(from.opendata.plan) %>% 
  summarize(count=n()) %>% 
  mutate(percent = scales::percent(count / sum(count),accuracy = 0.1)) %>%
  dplyr::bind_rows(all_open_data_sets %>% summarize(from.opendata.plan='Total',count=n(), percent=scales::percent(1,accuracy = 0.1))) %>% 
  kable(align = "l",format.args = list(big.mark = ","), caption = "Datasets Listed in the Open Data Plan"
  ) %>% kable_styling(bootstrap_options = "bordered",full_width = FALSE, position = "left")

# Let's take a look at which agencies have released the most (and least) data. 
# The three agencies that release the **most** data: 
# 1. Education - Data on student enrollment, performance, and health.
# 2. Parks - Geospatial data files, inventories, and plant/animal census data. 
# 3. Information Technology - A lot of geospatial data files.

# Filter and summarize which datasets are required by law
all_open_data_sets %>% 
  dplyr::select(dataset.owner,from.opendata.plan) %>%
  group_by(dataset.owner,from.opendata.plan) %>% 
  summarize(count=n()) %>% 
  pivot_wider(names_from=from.opendata.plan, values_from=count, values_fill = 0) %>%
  mutate(TOTAL = NO + YES,
         Pct_No = scales::percent(NO / (NO + YES),accuracy=0.1)) -> required_by_law

# Open Data Champion - Agencies with the most datasets on OpenData
required_by_law %>% arrange(desc(TOTAL)) %>% ungroup() %>% top_n(10,TOTAL) -> releasing_most

# Open Data Horders - Agencies with the least datasets on OpenData
required_by_law %>% dplyr::filter(TOTAL<=1) %>% arrange(dataset.owner) -> releasing_lease

# Side-by-Side table
kable(releasing_most,caption = "Agencies with Most Datasests", align = "l",format.args = list(big.mark = ",")) %>% 
  kable_styling(bootstrap_options = "bordered",full_width = FALSE,position = "float_left" )
kable(releasing_lease,caption = "Agencies with Fewest Datasets", align = "l",format.args = list(big.mark = ",")) %>% 
  kable_styling(bootstrap_options = "bordered",full_width = FALSE, position = "left")

# The table below (↓) summarizes all the "totals" for each agency - totals datasets, total visits, 
# and total data. "Total Data" is really just the sum total of all matrix sizes (row x column) but 
# "total data" is precise enough.

# * Most datasets: DOE - 32% of all datasets
# * Most visits: TLC - 27% of all visits
# * Most data: TLC - 45% of all data

# How many tables are there? - 3457 with DOE representing nearly a third!
all_open_data_sets %>%
  group_by(dataset.owner) %>%
  summarize(total_datasets = n(),
            total_visits = sum(as.numeric(visits),na.rm=TRUE),
            total_size = sum(as.numeric(new.size),na.rm=TRUE)) %>%
  mutate(avg_dataset_size = format(round(total_size / total_datasets,digits=0), scientific = FALSE, big.mark = ","),
         pct_total_dataset = round((total_datasets / sum(total_datasets))*100,digits=1),
         pct_total_visit = round((total_visits / sum(total_visits))*100,digits=1),
         pct_total_size = round((total_size / sum(total_size))*100,digits=1)) %>%
  # I had to format these "total" vars after using them in the percent-of-total calculations...
  # the commas turn the numbers in to char/text
  mutate(total_datasets = format(total_datasets,big.mark=","),
         total_visits = format(total_visits,big.mark=","),
         total_size = format(total_size,big.mark=",")) -> top_open_data_agencies_v1

# HTML widget from DT  
datatable(top_open_data_agencies_v1,  class = 'cell-border stripe', caption = 'OpenData Summary by Agency.',rownames = FALSE)

#--------------------------------------------------------------
#### Total Datasets by Agnecy
#--------------------------------------------------------------
# Let's see what the total data sets per agency look like. From the chart below 
# we can see just how skewed the data is. 

# Redo the data summary with out formatting
all_open_data_sets %>%
  group_by(dataset.owner) %>%
  summarize(total_datasets = n(),
            total_visits = sum(as.numeric(visits),na.rm=TRUE),
            total_size = sum(as.numeric(new.size),na.rm=TRUE)) -> top_open_data_agencies_v2

# Bar chart graph
top_open_data_agencies_v2 %>% dplyr::select(dataset.owner,total_datasets) %>% arrange(desc(total_datasets)) %>%
  ggplot(aes(x=reorder(dataset.owner,total_datasets),y=total_datasets, fill=dataset.owner)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=dataset.owner), angle = 90,vjust=.5,hjust=0,size=2) +
  guides(colour = guide_colourbar(order = 1))+
  theme(legend.position = "none",
        axis.text.x=element_blank()) +
  labs(title = "Total Datasets on OpenData by Agency", x = "Number of Datasets", y = "Agency") 

#--------------------------------------------------------------
#### Decile Breakpoints
#--------------------------------------------------------------
# This is like a table version of the barchart above - it shows how 
# skewed the dataset is for three measures.

# 1. Datasets per Agency: 90% of agencies have fewer than 66 datasets
# 2. Visits per Dataset: 90% of datasets have been viewed less than 6200 times
# 3. Size per Dataset: 90% of datasets have less than 122,000 rows (or 3,000,000) cells

# Most of the information on OpenData isn't accessed that often, and when a dataset is 
# popular, it's extremely popular. Everything about OpenData is skewed towards a few "winner" datasets. 

# Decile breakpoints for TOTAL-DATASETS-PER-AGENCY: 90% of AGENCIES have fewer than 66 datsets
quantile(required_by_law$TOTAL, probs = seq(.1, .9, by = .1)) %>% as.matrix() %>% 
  t() %>% format(., scientific = FALSE, big.mark = ",") -> total_dataset_decile

# Decile breakpoints for TOTAL-VISITS-PER-DATASET: 90% of datasets have been viewed less than 6255 times
all_open_data_sets$visits <- as.numeric(all_open_data_sets$visits)
quantile(all_open_data_sets$visits, probs = seq(.1, .9, by = .1)) %>% as.matrix() %>% 
  t() %>% round(,digits=0) %>% format(., scientific = FALSE, big.mark = ",") -> visit_decile

# Decile breakpoints for TOTAL-ROWS-PER-DATASET: 90% of datasets have less than XXX ROWS
all_open_data_sets$row.new <- as.numeric(all_open_data_sets$row.new)
quantile(all_open_data_sets$row.new, probs = seq(.1, .9, by = .1), na.rm = TRUE) %>% as.matrix() %>% 
  t() %>% round(,digits=0) %>% format(., scientific = FALSE, big.mark = ",") -> row_decile

# Decile breakpoints for TOTAL-SIZE-PER-DATASET: 90% of datasets have less than 3,058,605 cells
all_open_data_sets$new.size <- as.numeric(all_open_data_sets$new.size)
quantile(all_open_data_sets$new.size, probs = seq(.1, .9, by = .1), na.rm = TRUE) %>% as.matrix() %>% 
  t() %>% round(,digits=0) %>% format(., scientific = FALSE, big.mark = ",") -> size_decile

# Combine all rows into one table and name rows
decile <- rbind(total_dataset_decile,visit_decile,row_decile,size_decile)
rownames(decile) <- c("Total Datasets per Agency", "Total Views per Dataset", 
                      "Total Rows per Dataset", "Total Cells per Dataset")

# Visits per Dataset - Decile Breakpoints
decile  %>% kbl(caption = "Decile Breakpoints for Datasets, Views, Rows, and Cells") %>%
  kable_styling(full_width = F)

#--------------------------------------------------------------
#### Most Popular Datasets
#--------------------------------------------------------------
# Here we have the top 50 most visited datasets. Most of the popular datasets are somehow related 
# to employment or business. The first five are related to employment - job applications, civil service 
# results, and TLC datasets describing what drivers and vehicles are approved for work. 

# Top 50 most visited data sets
all_open_data_sets %>% 
  dplyr::select( data.agency,dataset.owner,name,
                 from.opendata.plan,
                 update.freq,visits,row.new) %>%
  mutate(total.visit.rank = dense_rank(desc(as.numeric(visits)))) -> most_visited_data_sets

# Top 50 most visited data sets
most_visited_data_sets %>% filter(total.visit.rank <= 50) %>%
  mutate(visits = format(visits,big.mark=","),
         num.row = format(row.new,big.mark=",")) %>%
  select (-c(row.new)) %>% arrange(total.visit.rank) -> most_visited_data_sets

# HTML widget from DT  
datatable(most_visited_data_sets,  class = 'cell-border stripe', caption = '50 Most Visited OpenData Tables',rownames = FALSE)

#--------------------------------------------------------------
#### Update Frequency
#--------------------------------------------------------------
# Below is a high level summary of dataset type and update frequency. The most visited tables are 
# datasets and maps, but data that's updated daily is also popular. Note that the `total.size` column 
# is zero for some categories. The reason for this is that size is defined as ROWS x COLUMNS, which 
# means that if a dataset doesn't have these attributes it has "zero" size, when in reality it really 
# does. An example of a dataset with "zero" size is a mapping layer - since it has no rows/columns 
# size = 0 but the file consumes memory.

# Most visited are datasets, followed by maps
all_open_data_sets %>% select(type,visits,new.size) %>% 
  mutate_at( c("visits", "new.size"), ~replace(., is.na(.), 0)) %>% 
  group_by(type) %>% 
  summarize(count = n(),
            total.visits = sum(visits),
            total.size = sum(new.size)) %>% 
  arrange(desc(total.visits)) %>%
  mutate(count = format(count,big.mark=","),
         total.visits = format(total.visits,big.mark=","),
         total.size = format(total.size,big.mark=",")) -> popular_type 

# Format table
datatable(popular_type,  class = 'cell-border stripe', caption = 'Most Popular Data Type',rownames = FALSE, 
          options=list(paging=FALSE,searching=FALSE))

# Datasets that are updated daily are the most popular
all_open_data_sets %>% select(update.freq,visits,new.size) %>% 
  mutate_at( c("visits", "new.size"), ~replace(., is.na(.), 0)) %>% 
  group_by(update.freq) %>% 
  summarize(count = n(),
            total.visits = sum(visits),
            total.size = sum(new.size)) %>%
  arrange(desc(total.visits)) %>%
  mutate(count = format(count,big.mark=","),
         total.visits = format(total.visits,big.mark=","),
         total.size = format(total.size,big.mark=",")) -> update_freq 

# Format table
datatable(update_freq,  class = 'cell-border stripe', caption = 'Most Popular Update Frequency',
          rownames = FALSE, options=list(searching=FALSE))

#--------------------------------------------------------------
#### Category
#--------------------------------------------------------------
# Similar to our findings on the agency with the most datasets, the category with the most 
# data tables is Education. However, the largest and most popular datasets are for transportation.

# Most popular category. 
all_open_data_sets %>% select(category,visits,size) %>% 
  group_by(category) %>% summarize(count = n(),
                                   avg_size = mean(size, na.rm=TRUE), 
                                   avg_visits = mean(visits, na.rm=TRUE)) %>%
  mutate(category = ifelse(is.na(category),"NA",category),
         count = format(count,big.mark=","),
         avg_size = format(round(avg_size,digits=0),scientific = FALSE,big.mark=","),
         avg_visits = format(round(avg_visits,digits=0),big.mark=",")) -> category

# Format table
datatable(category,  class = 'cell-border stripe',
          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                            'Most Popular Update Frequency'),
          rownames = FALSE, options=list(pageLength=15,paging=FALSE,searching=FALSE))

#--------------------------------------------------------------
#### Automation
#--------------------------------------------------------------
# Most data (around 85%) is not automated, thought a small and significant portion (around 15%) is automated.

# Group and count data
all_open_data_sets %>% ungroup() %>% select(automated,can.automate) %>% 
  group_by(automated,can.automate) %>% 
  summarize(count = n(),.groups="keep") %>% 
  ungroup() %>% mutate(freq = round(count / sum(count),2)) -> automation

# Format table 
automation  %>% kbl(caption = "Automation Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),full_width = F)

#--------------------------------------------------------------
### Conclusion
#--------------------------------------------------------------
# The [Open Data Asset Inventory](https://data.cityofnewyork.us/City-Government/Local-Law-251-of-2017-Published-Data-Asset-Invento/5tqd-u88y) 
# provides high-level meta-like data on the data tables and electronic resources hosted by [NYC Open Data](https://opendata.cityofnewyork.us). 
# The most popular datasets are those related to education, transportation, and employment. This analysis offers the reader a basic review of 
# methods that can be used to clean, analyze, and present findings using a real-world, public datasets. 

#--------------------------------------------------------------
#### Things to Change Next Time Around
#--------------------------------------------------------------
# * Reflow the analysis along these lines: (1) Intro; (2) Dataset Review (variable types and "peaks" at the data); (3) Data Cleaning (yes/no harmonizing); (4) New Variables (new uid, new size data, and the text-to-number function); (5) Distributions (density functions and tables); (6) Test and Fit (normality testing, best fit assessment); (7) Correlations (correlations and tables).
# * Simpler "data.agency" name shortening.
# * A simpler loop/apply method for cleaning yes/no values.
# * Consistent tabling - I tested out too many different table format options.
# * Clarify the uid/parent.uid analysis.
# * The new size loop takes a long time to run - see if I can speed it up.
#   > Some might be logical. Under what circumstances can I use the existing row/column count information? most of my new row/column counts were the same as the old one. I unintentionally pulled all uids to see how off my method might be, but now it needs to be optimized for speed.
#   > Some might be coding. What fields can I access faster than pulling the first 200 rows to get column data? Is there an alternative to select(*) for row counts?
# * Improve the text_to_num_recode() function. I would like the function to create a recode dictionary data.frame to hold the old/new values. I would also like to have different recoding options - alphabetical sorting; large-to-small sorting based on total values for each field value; an arbitrary, user-defined list. 
# * Simplier column-name shortening - I don't want to list every item to recode.
# * A better statistical analysis and review section. Spend more time evaluating best-fit options for various fields. 

#--------------------------------------------------------------
## Citations
#--------------------------------------------------------------
# This is not an exhaustive list of web-pages that helped me prepare this analysis, but it captures some of the most important references. 

# 1. https://universeofdatascience.com/how-to-recode-character-variables-in-r/
# 2. https://docs.google.com/document/d/185lRocur3g-EyvXIUnYXNITSPp0hcB9wQ7DsA7Hvl3M/edit
# 3. https://opendata.cityofnewyork.us/how-to/
# 4. https://dev.socrata.com/consumers/getting-started.html
# 5. https://dev.socrata.com/docs/queries/
# 6. https://dev.socrata.com/docs/endpoints.html
# 7. https://github.com/njtierney/rmd-errors
# 8. https://stackoverflow.com/questions/29274501/r-markdown-changing-font-size-and-font-type-in-html-output
# 9. https://rstudio.github.io/DT/
# 10. https://stackoverflow.com/questions/8161836/how-do-i-replace-na-values-with-zeros-in-an-r-dataframe
# 11. http://www.wu.ece.ufl.edu/links/dataRate/DataMeasurementChart.html
# 12. https://rc2e.com/rmarkdown
# 13. https://stackoverflow.com/questions/12834861/access-local-dataframe-outside-the-function-in-r
# 14. https://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function
# 15. https://r-lang.com/not-in-r/
# 16. https://www.datanovia.com/en/blog/easy-correlation-matrix-analysis-in-r-using-corrr-package/
# 17. https://bwlewis.github.io/covar/missing.html
# 18. https://themockup.blog/static/slides/intro-tables-urban.html
# 19. http://www.sthda.com/english/wiki/normality-test-in-r
# 20. https://bookdown.org/yihui/rmarkdown-cookbook/
# 21. https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

#--------------------------------------------------------------
##  Appendix A
### Useful Bits of Code
#--------------------------------------------------------------
# * Use "eval = FALSE" in the markdown header to include/print code but not execute it.

# * Use this code to copy data to Excel in a **Mac**.
# # Mac - Copy data to Excel via Clipboards
# clip <- pipe("pbcopy", "w")
# write.table(dataframe, file=clip, sep = '\t', row.names=FALSE,col.names=TRUE)
# close(clip)

# * Use this code to copy data to Excel in **Windows**.
# write.table(dataframe,"clipboard",sep="\t",row.names=FALSE,col.names=TRUE)

# * Change [font color](https://bookdown.org/yihui/rmarkdown-cookbook/font-color.html) in RMarkdown.
# colorize <- function(x, color) {if (knitr::is_latex_output()) {sprintf("\\textcolor{%s}{%s}", color, x)}
# else if (knitr::is_html_output()) {sprintf("<span style='color: %s;'>%s</span>", color,x)} else x}

#--------------------------------------------------------------
##  Appendix B
### Data Distributions & Correlations
#--------------------------------------------------------------
# What correlations are there in this table for fields with discreet and meaningful values? We'll focus this 
# analysis on yes/no fields, size, visits, and dataset owners. 

# Here's the work plan:
# 1. Recode all text values as numbers.
#     a. For example - recode "update.freq" so that "Annually" = 1, "Historical Data" = 2, etc. 
#     b. Write a function to do this. 
# 2. Run a `cor` on the recoded matrx. See what's correlated.
# 3. See if there is any data to support this hypothesis: There is a positive correlation between:
#     a. Visits and Update Frequency (NO - Fail to find a correlation that's significant)
#     b. Visits and Geo Coding (NO - Fail to find a correlation that's significant)
#     c. Visits and Dataset Owner (YES - Find a correlation that's significant but small)

# After the correlation review, we'll review density curves for visits and investigate whether the 
# visit data is normal or not (spoiler - it's not).

# Load Libraries
library(corrplot) # correlation matrix graphics
library(corrr) # big correlations
library(ggpubr)
library(EnvStats)

# Create a series of normal random numbers
norm <- rnorm(1000)

# Define not in operator
`%!in%` <- Negate(`%in%`)

# Define a function to recode text values to numbers for a correlation matrix
text_to_num_recode <- function(df1,col1){
  oldvals <- as.list(sort(unique(df1[,col1])))
  newvals <- seq.int(length(oldvals))
  match_list <- newvals[match(df1[,col1], oldvals)]
  new_name <- paste0(col1,"_num")
  df1[new_name] <- match_list
  return(df1)
}

# Select data to recode
all_open_data_sets %>% 
  select(visits,new.size,from.opendata.plan,automated,can.automate,
         has.dictionary,contain.address,geo.coded,exists.externally,
         records.removed,automated,can.automate,from.opendata.plan,
         has.dictionary,contain.address,geo.coded,exists.externally,
         records.removed,dataset.owner,type,category,update.freq) -> all_open_data_sets_recode

# List of colnames to process
list_to_recode <- colnames(all_open_data_sets_recode)[colnames(all_open_data_sets_recode) %!in% 
                                                        c('visits','new.size')]

# Loop through list of columns to recode
for (i in list_to_recode){
  all_open_data_sets_recode <- text_to_num_recode(all_open_data_sets_recode,i)
}

# Add "_num" to visits and new.size to keep in the next step
colnames(all_open_data_sets_recode)[which(names(all_open_data_sets_recode) == "visits")] <- "visits_num"
colnames(all_open_data_sets_recode)[which(names(all_open_data_sets_recode) == "new.size")] <- "new.size_num"

# Drop columns without "_num" in the name (this removes the text values)
all_open_data_sets_recode <- all_open_data_sets_recode[,grepl("_num",names(all_open_data_sets_recode))]
to_drop <- apply(all_open_data_sets_recode,1,function(x) any(x>0))
all_open_data_sets_recode <- all_open_data_sets_recode[to_drop,]

# Add normal random data to a column
all_open_data_sets_recode$norm.rand_num <- rnorm(nrow(all_open_data_sets_recode))

# Shorten names so labels will fit in graphics
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'visits_num'] <- 'vist'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'new.size_num'] <- 'size'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'from.opendata.plan_num'] <- 'plan'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'automated_num'] <- 'auto'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'can.automate_num'] <- 'cato'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'has.dictionary_num'] <- 'dict'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'contain.address_num'] <- 'addr'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'geo.coded_num'] <- 'geo'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'exists.externally_num'] <- 'xtrn'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'records.removed_num'] <- 'rmvd'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'dataset.owner_num'] <- 'own'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'type_num'] <- 'type'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'category_num'] <- 'cat'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'update.freq_num'] <- 'freq'
names(all_open_data_sets_recode)[names(all_open_data_sets_recode) == 'norm.rand_num'] <- 'rand'

# Calculate large correlation matrix
open_data_corr = cor(all_open_data_sets_recode,use="pairwise.complete.obs")

# Calculate significance
testRes = cor.mtest(all_open_data_sets_recode, conf.level = 0.95)

#--------------------------------------------------------------
#### A Trimmed Correlation Matrix
#--------------------------------------------------------------
# Significant coefficients are left visible, non-significant coefficients are left blank. 
# The confidence interval used 95%. Unfortunately the results are not particularly interesting. 

# * "Can Automate" is correlated with "Automated" (cato, auto): Not too useful and a bit of a 
#   tautology - if a thing is automated it means it is automatable. 
# * "Geocoded" is highly correlated with "Address" (geo, addr): Also not too useful. Geocodes and 
#   addresses are similar, if you have one you can frequently derive the other. 

# The one correlation that grabs my eye is the one between dataset size (size) and the dataset's owner (own). 
# This is small and positive. One area for further study might be to look at correlation matrices for each owner. 
# I predict that transportation an education agencies will have differences that point towards agencies voluntarily 
# releasing large datasets with good documentation. 

# Trimmed correlation matrix
corrplot(open_data_corr, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
        addCoef.col ='grey25', number.cex = 0.7, order = 'AOE', diag=FALSE)

#--------------------------------------------------------------
#### A Full Correlation Matrix
#--------------------------------------------------------------
# All coefficients are left visible in this table.

# Full correlation matrix
corrplot(open_data_corr, method = 'color',number.cex = 0.6, order = 'AOE', 
         col = COL1('YlGn'), cl.pos = 'b',addgrid.col = 'white',addCoef.col = 'grey25')

#--------------------------------------------------------------
#### Density Curves
#--------------------------------------------------------------
# A [Density plot](https://en.wikipedia.org/wiki/Probability_density_function) displays the distribution 
# of numeric values. For a given point on the line of the curve, the *y* value represents the share of 
# objects located at point *x*. Given a vertical line (parallel to the y-axis) that passes through the 
# density curve, the total area to the left of the line represents the total share of observations at or 
# below the point the vertical line passes through x-axis. 

# The normal distribution is **black**, the raw visit data is `r colorize("green", "green")` and the log 
# transformed visit data is `r colorize("red", "red")`.

# The raw visit data does not look normal at all, but the log transformation looks considerably more normal, 
# though it has a significant right/positive skew.

# 1- Normal test data
ggdensity(norm, color = "black", fill="grey") +
  guides(colour = guide_colourbar(order = 1))+
  theme(legend.position = "none") +
  labs(title = "For Reference - Normal Distibution", x = "Number", y = "Density") 

# 2 - As-Is visits data
ggdensity(all_open_data_sets$visits, color = "green", fill="green") +
  theme(legend.position = "none") +
  labs(title = "Raw Distibution of Total Visits per Dataset", x = "Raw Total Dataset Visits", y = "Visit Density") 

# 3 - Log transformed visits data
ggdensity(log10(all_open_data_sets$visits), color = "red", fill="red") +
  theme(legend.position = "none") +
  labs(title = "Log Distibution of Total Visits per Dataset", x = "Log10 Total Dataset Visits", y = "Visit Density") 

#--------------------------------------------------------------
#### Q-Q Plots
#--------------------------------------------------------------
# A [Q-Q plot](https://en.wikipedia.org/wiki/Q–Q_plot) compares the probability distributions of two plots 
# against each other. A 45-degree line is included to show where the points should appear if the two distributions are equal. 

# None of the size distributions examined come close to lining up on the 45-degree line, though the log transformations 
# (the `r colorize("red", "red")` and `r colorize("blue", "blue")` graphs) come closer to normal (**black**) than the raw 
# data (`r colorize("green", "green")`)

qqPlot(x=norm,distribution='norm', 
       plot.type="Q-Q",points.col="black",add.line=TRUE,main="Q-Q Plot: Normal vs Normal",
       xlab = 'Quantiles of Normal', ylab = 'Quantiles of Normal')

qqPlot(x=all_open_data_sets$new.size,distribution='norm', 
       plot.type="Q-Q",points.col="green",add.line=TRUE,main="Q-Q Plot: New.Size vs Normal",
       xlab = 'Quantiles of Actual new.size', ylab = 'Quantiles of Normal')

qqPlot(x=log10(all_open_data_sets$new.size),distribution='norm', 
       plot.type="Q-Q",points.col="red",add.line=TRUE,main="Q-Q Plot: Log10 New.Size vs Normal",
       xlab = 'Quantiles of Log10(new.size)', ylab = 'Quantiles of Normal')

qqPlot(x=log10(all_open_data_sets$new.size),distribution='lnorm',
       plot.type="Q-Q",points.col = "blue",add.line=TRUE, main="Q-Q Plot: Log10 New.Size vs Log Normal",
        xlab = 'Quantiles of Log10(new.size)', ylab = 'Quantiles of LogNormal')

#--------------------------------------------------------------
#### Normality Test
#--------------------------------------------------------------
# We've already used density graphs and Q-Q plots to graphically examine the distributions of our Visit 
# and Size data elements. Visuals can help guide but for precise work we need a statistical test of normality. 
# There are [several normality tests](https://en.wikipedia.org/wiki/Normality_test) but I'll use the *Shapiro-Wilk 
# normality test* in R. I performed this test on normal data, the visit and size data, and log transformations of 
# the visit and size data. 

# The data element we'll focus on is the p-value. If the p-value for a dataset is > 0.05 then the we can not reject 
# the hypothesis that the dataset distribution is different from normal, which means the dataset is normally distributed.

# As you can see in the table below, the normal dataset is the **only** one with a normal distribution. Visit and Size 
# both are very, very far from normal - as are their log transforms, though the p-value of the log transform is half the 
# value of the untransformed data. In other words, visit and size ARE NOT normally distributed. 

# Test 1 - Normal Data
test1 <- shapiro.test(norm) 
test <- cbind(test1$data.name,test1$p.value,test1$statistic)
test[1,1]  <- "Normal"
test0 <- as.data.frame(test)

# Test 2 - Visits
test2 <- shapiro.test(all_open_data_sets$visits)
test <- cbind(test2$data.name,test2$p.value,test2$statistic)
test[1,1]  <- "Visits"
test0 <- rbind(test0,test)    

# Test 3 - Log10(Visits)
test2 <- shapiro.test(log10(all_open_data_sets$visits))
test <- cbind(test2$data.name,test2$p.value,test2$statistic)
test[1,1]  <- "Log10(Visits)"
test0 <- rbind(test0,test)  

# Test 4 - Data Size
test3 <- shapiro.test(all_open_data_sets$new.size)
test <- cbind(test3$data.name,test3$p.value,test3$statistic)
test[1,1]  <- "Size"
test0 <- rbind(test0,test) 

# Test 5 - Data Size
test3 <- shapiro.test(log10(na.omit(all_open_data_sets$new.size+.0001)))
test <- cbind(test3$data.name,test3$p.value,test3$statistic)
test[1,1]  <- "Log10(Size)"
test0 <- rbind(test0,test) 

# Clean up table
test0 <- as.data.frame(test0)
rownames(test0) <- NULL
names(test0) <- c('data.name','p.value','statistic')
test0$p.value <- format(as.numeric(test0$p.value ), digits=3)
test0$statistic <- format(as.numeric(test0$statistic ), digits=3)

# Format table 
test0 %>%  kbl(caption = 'Shapiro-Wilk normality test') %>%
  kable_styling(full_width = F)

#--------------------------------------------------------------
##  Appendix C
### Session Info
#--------------------------------------------------------------
# A quick and dirty display of the packages used to create this markdown.
sessionInfo()

###############################################################
###   s t o p
###############################################################