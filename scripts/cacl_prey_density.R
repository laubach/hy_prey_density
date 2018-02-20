###############################################################################
##############               Calculate Prey Counts               ##############
##############                 and Prey Density                  ##############
##############                 By: Zach Laubach                  ##############
##############             last updated: 19 Feb 2018             ##############
###############################################################################

  ### PURPOSE: This code is desingned to summarise the number of prey counted
  ### in prey transects. Two tabels are used, which include a data frame
  ### containing the dates of interest (typically, a file containing hyena 
  ### data) and tblPreyCount.

  ### ERRORS / BUG REPORTS: Please send to zchlaubach@gmail.com


  # Code Blocks
    # 1: Configure Workspace
    # 2: Import Data
    # 3: Set Globals
    # 4: Data transformations
    # 5: Calculate prey density
    # 6: Save prey density table



###############################################################################
##############             1.  Configure Workspace               ##############
###############################################################################

  ### 1.1 Clear global environment
    rm(list = ls())
  
  
  ### 1.2 Install and load packages 

    # Check for tidyverse and install if not already installed
      if (!'tidyverse' %in% installed.packages()[,1]){
        install.packages ('tidyverse')
      }
    # load tidyverse packages
      library ('tidyverse')
    
    # Check for sqldf and install if not already installed
      if (!'sqldf' %in% installed.packages()[,1]){
        install.packages ('sqldf')
      }
    options(gsubfn.engine = "R") #fixes tcltk bug; run before require sqldf
    # load tidyverse packages
      library ('sqldf')
    
    # Check for lubridate and install if not already installed
    if (!'lubridate' %in% installed.packages()[,1]){
      install.packages ('sqldf')
    }
    # load lubridate packages
    library ('lubridate')

    # Check for here and install if not already installed
      if (!'here' %in% installed.packages()[,1]){
        install.packages ('here')
      }
    # load here packages
      library ('here')


      
  ### 1.3 Get Version and Session Info
    R.Version()
    sessionInfo()
  
    # Developed in:   
    # R version 3.4.3 (2017-11-30)
    # Platform: x86_64-apple-darwin15.6.0 (64-bit)
    # Running under: macOS Sierra 10.12.4

    
  ### 1.4 set working directory  
    setwd(here())

    
    
###############################################################################
##############                  2. Import Data                   ##############
###############################################################################  
  
  ### 2.1 Import Access fisi backend
    # read in tidy Access fisi backend tables and save as a data frames
    source(paste0("/Volumes/Holekamp/code_repository/R/1_output_tidy_tbls/",
                  "load_tidy_tbls.R"))


###############################################################################
##############                  3. Set Globals                   ##############
###############################################################################    
        
    
  ### 3.1 Select prey census routes
    ## a) Name and group prey transects 
      north.trans <- c("rsp", "north")
      south.trans <- c("s1", "s2")
      hz.trans <- c("burn1", "burn2", "burn3", "sst")
      tm.trans <- c("rsp", "north", "s1", "s2", "sst")
      marariver.trans <- c("r1", "r2")
      talek.trans <- c("whigh", "wlow") #census route 3 (W3)
      narok.trans <- c("whigh", "wlow", "w3")
      ## Note 1: talek and fig tree (for talek trans )
      ## Note 2: exclude burns; use 6 month or possibly a 3 month window but 
        # not smaller (Kay's advice)
   
#**************************** USER DEFINED START ******************************#
    ## b) Select prey routes  
      # on the right side of <- type one of the above transect group names
      prey.route <- narok.trans
#***************************** USER DEFINED END *******************************#     
      
    ## c) Subset tblPreyCount by transects of interest    
      tblPreyCount <- filter (tblPreyCount, 
                              (grepl (paste (prey.route, collapse = "|"),
                                      transect)))    
    
    
  ### 3.2 Subet tblHyenas
#**************************** USER DEFINED START ******************************#
    ## a) Select clan  
      # in between the quotes on the right side of <-, type a hyena clan name
      clan.sub <- 'talek'
#***************************** USER DEFINED END *******************************#  

    ## b) Subset tblHyena by clan
      tblHyenas <- filter(tblHyenas, (grepl(paste0('^',clan.sub,'$'), clan))) 
      
      
  ### 7.2 Set date parameters 
#**************************** USER DEFINED START ******************************#
    ## a) Select a date variable
      # in between the quotes on the right side of <-, type the varialbe name
      # of the date to be used. This date is the reference around which 
      # prey counts/densities will be calucated for each hyena
      date.select<- 'birthdate'
      
    ## b) Create prey transect time window/periods
      # Define the boundaries for calculating prey counts/densities base on
      # the selected date varialbe. This creates a range plus and minus 
      # the date of interest (e.g. birthdate) and saves these periods in 
      # a data frame; values are in months
      
        # period names; to the right of the <-, type a comma delimited 
        # list of names
        period <- c( "peri.concpt", "gest", "birth-3", "3-6", "6-9")
        
        # starting periods; to the right of the <-, type a comman delimited 
        # list of numbers; these are months from the ref from date; 
        # has to be paired with an end date
        start <- c(-6, -3, 0, 3, 6)
        
        # ending periods; to the right of the <-, type a comman delimited 
        # list of numbers; these are months from the ref from date; 
        # has to be paired with a start date
        end <- c(-3, 0, 3, 6, 9)
#**************************** USER DEFINED START ******************************#        
    
    ## c) Combine period info into a data frame    
      # the data frame containing the time windows/periods
      period_data <- data.frame(period, start, end)
      
      
      
###############################################################################
##############              4. Data transformations              ##############
############################################################################### 
      
  ### 4.1 Remove NA from tblHyena
    ## a) Remove hyenas from tblHyena which have no birthdate 
      tblHyenas <- tblHyenas %>% drop_na(birthdate)    
      
      
      
###############################################################################
##############             5. Calculate prey density             ##############
############################################################################### 
    
  ### 5.1 List Types of Prey
    ## a) make a list of prey items
      prey.list<-unique(colnames(tblPreyCount))
 
    ## b) remove variable names from list
      prey.list <- prey.list[prey.list != c("region", "transect", "day",
                                            "month", "year", "julian.actual",
                                            "actual.date", "julian.start",
                                            "start.date", "julian.end",
                                            "end.date","bi.month", "time.start",
                                            "time.end", "dfb", "burn.state",
                                            "distance")]
      prey.list <- prey.list[prey.list != c("comments")]

  
  ### 5.2  Create New Epmty Dataframes 
    ## a) Make Transect Summary Data frame
      # This is an empty data frame that can store the prey density values
      transect_summary  <- c() 
      
    ## b) Make Transect Metadata data frame  
      # This is an empty data frame that can store metadata (eg. number of
      # transects during each prey period)
      transect_metadata <-c()

  
  ### 5.3 Prey Density Calculator
    ## a) Loop through each prey period
    for (j in 1:nrow(period_data)) {
    
    ## b) Loop through each hyena in a data frame and calculate prey density
      for (i in 1:nrow(tblHyenas)) { 
        # loop through 1:n dates
        date <- ymd (tblHyenas[i, c(date.select)])
        # loop through 1:n IDs
        id = paste (tblHyenas$id[i])               
        
        #date <- round_date(date, "month")        # round dates to first of
                                                  # the closest month, this is 
                                                  # to avoid an over and under
                                                  # counting 
        
        start.date <- date %m+% months(period_data$start[j])  # start date minus 
                                                              # months
        
        end.date <- date %m+% months(period_data$end[j])      # start date plus 
                                                              # months
      
        day.interval <-  seq.Date(from = start.date,  # sequence of all days in
                            to = end.date,            # between start and 
                            by = "day")               # end date
                            
        
        # calculate the dates of prey transects that intersect with days in 
        # in the time interval
        date_overlap <- filter(tblPreyCount, 
                               as.Date(tblPreyCount$actual.date) 
                               %in% day.interval) 
        
        # Control flow
          # if there is no date overlap, then go to next loop iteration
            if (length(date_overlap$region) < 1) {
              next
            }
            
        # Summation of each prey species 
          # A row by row summation to calculate total prey counts during
          # each bi-montly sampling period for each transect
          date_overlap$total <- rowSums(date_overlap[,c(prey.list)], 
                                        na.rm = T)
         
        # use dplyr gather function to transform data into long format
        # this version list all prey species for each hyena during each
        # prey period
        sum_prey <- date_overlap %>%
          gather_(key = "prey", value = "prey.count",
                  c(prey.list, (paste("total"))))
        
        # calculate the density of each species of prey density counted along
        # each transect (in square kilometers); assumes prey are counted 
        # within 100m on each side of the transect
        sum_prey$density <- with (sum_prey, 
                                  prey.count/(as.numeric(distance)*0.2)) 

        # calucate the average density of each species for each transect
        # (species density / # transects run)
        prey_stat <- sum_prey %>%
          group_by(prey) %>%
          summarize (num.transects = sum(!is.na(prey)),
                     mean = mean(density, na.rm = T))
                           
        
        # add the hyena's ID onto the prey stat table
        prey_stat <- cbind(prey_stat, id)
        
        # add the prey-period onto the prey stat table
        prey_stat$period <- paste(period_data$period[j])
        
        # add the newly created average prey densities to a new dataframe
        # over each iteration of the loop
        transect_summary <- rbind(transect_summary, 
                                  prey_stat[c("prey", "mean", "period", 
                                              "id")])
        
        # add the newly created prey metadata to a new dataframe
        # over each iteration of the loop
        transect_metadata <- rbind(transect_metadata, 
                                  prey_stat[c("num.transects", "period", 
                                              "id")])
      }    
    }
      
  
  ### 5.4 Reshape the final prey count data
    ## a) combine/unite the period name to each of the prey type names
      transect_summary <- transect_summary %>% 
        unite(prey.period, prey, period, sep = ".") 
      
    ## b) use dplyr spread function to transform data into wide format                    
      transect_summary <- transect_summary %>%
        spread(prey.period, mean)
   
         
  ### 5.5 Reshape the final prey count metadata
    ## a) combine/unite the period name to each of the prey type names
      transect_metadata <- transect_metadata[!duplicated 
                                             (transect_metadata[c("id",
                                                                  "period")]), ]
      
    ## b) copy the text "num.transects" to period to update variable name 
      transect_metadata$period <- with(transect_metadata, 
                                             paste("num.transects",period, 
                                                   sep="."))
      
    ## c) use dplyr spread function to transform data into wide format   
      transect_metadata <- transect_metadata %>% 
        spread(period, num.transects) 
      
      
      
  ### 5.6 Left Join transect metadata to transect summary   
    transect_summary <- left_join(transect_summary, 
                                  transect_metadata, by = "id") 
    
    
  ### 5.7 Left Join tblHyenas data to transect summary   
    prey_density <- left_join(transect_summary, tblHyenas, by = "id")    
      
  

###############################################################################
##############            6.  Save prey density table            ##############
###############################################################################

  # Save prey density table as a spreadsheet a with a .cvs extension and 
  # today's date. Files are saved in the 'output' folder in the working 
  # directory.

  ### 6.1 Set up date parameters
    # print today's date
    today <- Sys.Date()
    date <- format(today, format="%d%b%Y")
   

  ### 6.2 Generate File Names
    # For the table that will be saved as a .csv file, first generate a file 
    
    ## a) File name for sample_request table
      csv.file.name.prey <- paste ("./output/", clan.sub,"_prey_density", 
                                      date, ".csv", sep= "") 
   
      
  ### 6.3 Save Tables 
    # Save the data frame as a .csv file (a spreadsheet/table) into the 
    # output folder in the working directory.
    
    ## a) Save prey_density table
       write.csv (prey_density, file = csv.file.name.prey)
  