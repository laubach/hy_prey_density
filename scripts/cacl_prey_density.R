#################################################
####         Calculate Prey Counts           ####
####            and Prey Density             ####
####            By: Zach Laubach             ####
####       last updated: 15 Aug 2017         ####
#################################################

  ### PURPOSE: This code is desingned to summarise the number of prey counted
  ### in prey transects. Two tabels are used, which include a data frame
  ### containing the dates of interest (typically, a file containing hyena 
  ### data) and tblPreyCount.

  ### ERRORS / BUG REPORTS: Please send to zchlaubach@gmail.com

  ### Special thanks to all contributors and commentors on stackoverflow (etc.)
  ### for creative code solutions.


  # Code Blocks
    # 1: Load Packages
    # 2: Set Globals
    # 3: Set Working Directory
    # 4: Import Data
    # 5: Massage Data
    # 6: Query and Join Dataframes 
    # 7: Calculate Prey Density
    # 8: Save Intermediate Tables as Spreadsheets



#################################################
####            1. Load Packages             ####
#################################################

  # clear global environment
  rm(list = ls())
  
  # Require - load a packages  into a current R session
  
  ### 1.1 Data Manipulation and Descriptive Stats Packages
      #install.packages("plyr")
      require(plyr)
      #install.packages("dplyr")
      require(dplyr)
      #install.packages("reshape2")
      require(reshape2)
      #install.packages("reshape2")
      require(tidyr)
      #install.packages("sqldf")
      options(gsubfn.engine = "R") #fixes tcltk bug; run before require sqldf
      require(sqldf)

      #install.packages("lubridate")
      require(lubridate)

      
  ### 1.2 Get Version and Session Info
    R.Version()
    sessionInfo()
  
    # Developed in:   
      # R version 3.3.1 (2016-06-21)
      # Platform: x86_64-apple-darwin13.4.0 (64-bit)
      # Running under: OS X 10.12.3 (Sierra)

    

#################################################
####             2. Set Globals              ####
#################################################    
 
  #trans <- unique(tblPreyCount$Transect)       
  north.trans <- c("RSP", "North")
  south.trans <- c("S1", "S2")
  hz.trans <- c("Burn1", "Burn2", "Burn3", "SST")
  tm.trans <- c("RSP", "North", "S1", "S2", "SST")
  marariver.trans <- c("R1", "R2")
  talek.trans <- c("WHIGH", "WLOW") #census route 3 (W3)
  narok.trans <- c("WHIGH", "WLOW", "W3")
    # talek and fig tree (for talek trans )
  ## Note: exclude burns; use 6 month or possibly a 3 month window but not
    # smaller (Kay's advice)
  
  
    
#################################################
####         3. Set Working Directory        ####
#################################################

  ### 3.1 # Set a working directory
    #setwd("/Volumes/Holekamp/code_repository/access_fisi_export")
    setwd("~/Git/fisi_lab/hy_prey_density")
  
  
  ### 3.2 Create path to a LUMA data folder
    main_data_path <- paste("~/R/R_wd/basic_tools/prey_transect/",
                            "data/", sep = '')
    
  
  ### 3.3 Create path to Access data folder 
    access_data_path <- paste("~/R/R_wd/fisi/access_fisi_data/",
                              sep = '')
      

      
#################################################
####             4. Import Data              ####
#################################################      

  ### 4.1 Import tblHyenas
    # read in tblHyenas access backend file and save as a data frame
    tblHyenas <-  read.csv (paste(access_data_path,
                                  "tblHyenas_jun2017.csv",sep = ''), 
                            header = T)
    
  ### 4.2 Import tblPreyCount
    # read in tblPreyCount access backend file and save as a data frame
    tblPreyCount <-  read.csv (paste(access_data_path,
                                     "tblPreyCount_feb2017.csv", sep = ''), 
                               header = T)      
    
    

#################################################
####            5. Massage Data              ####
#################################################
       
  ### 5.1 Remove NA from tblHyena
    ## a) Remove hyenas from tblHyena which have no birthdate 
      tblHyenas <- 
 
      
  ### 5.2 Convert dates tblHyenas   
    ## a) convert cub first seen date
      tblHyenas$FirstSeen <- as.POSIXct(as.character 
                                            (tblHyenas$FirstSeen),
                                            format='%m/%d/%y')
      
    ## b) convert den grad date
      #tblHyenas$DenGrad <- as.POSIXct(as.character 
      #                                  (tblHyenas$DenGrad),
      #                                  format='%m/%d/%y')
      
    ## c) convert disappearance date
      tblHyenas$Disappeared <- as.POSIXct(as.character 
                                        (tblHyenas$Disappeared),
                                        format='%m/%d/%y')
      
    ## d) convert cub birth date
      tblHyenas$Birthdate <- as.POSIXct(as.character 
                                        (tblHyenas$Birthdate),
                                        format='%m/%d/%y')
      
      
  ### 5.3 Convert dates tblPreyCount   
    ## a) convert actual date that transect was run 
      tblPreyCount$ActualDate <- as.POSIXct(as.character 
                                        (tblPreyCount$ActualDate),
                                        format='%m/%d/%y')
      
    ## b) convert bi-monthly start date for transect
      tblPreyCount$StartDate <- as.POSIXct(as.character 
                                            (tblPreyCount$StartDate),
                                            format='%m/%d/%y')
      
    ## c) convert bi-monthly start date for transect
      tblPreyCount$EndDate <- as.POSIXct(as.character 
                                           (tblPreyCount$EndDate),
                                           format='%m/%d/%y')

  
              
#################################################
####      6. Query and Join Dataframes       #### 
#################################################
  
      
  ### 6.1 Subset Hyena data by clan
    main_data <- filter(main_data, (grepl('^Talek$', Clan)))    
  
  ### 6.2 Subset tblPreyCount by transects of interest    
    tblPreyCount <- filter (tblPreyCount, (grepl 
                                           (paste(narok.trans, collapse = "|"), 
                                             Transect)))    
      
      
  ### 6.3 Join tbyHyenas data to main_data
    # An INNER join of 'tblHyenas' with 'main_data', making a new
    # data frame which includes select variables of interest from each 
    # parent table. Parent tables (tblHyenas and main_data) are linked
    # where the ID variable matches. 
      main_data <- sqldf("SELECT
                             main_data.*           
                             , tblHyenas. FirstSeen, DenGrad, Disappeared,
                              Mom, Birthdate, NumberLittermates, Litrank, 
                              ArrivedDen, LeaveDen, Fate, MortalitySource,
                              DeathDate, Weaned 
                             FROM tblHyenas      
                             INNER JOIN main_data      
                             ON tblHyenas.ID = main_data.ID
                             GROUP BY sample_ID")  
    
      
      
#################################################
####        7. Calculate Prey Density        #### 
#################################################  
    
  ### 7.1 List Types of Prey
    ## a) make a list of prey items
      prey.list<-unique(colnames(tblPreyCount))
 
    ## b) remove variable names from list
      prey.list <- prey.list[prey.list != c("Region", "Transect", "Day",
                                            "Month", "Year", "JulianActual",
                                            "ActualDate", "JulianStart",
                                            "StartDate", "JulianEnd",
                                            "EndDate","BiMonth", "TimeStart",
                                            "TimeEnd", "DFB", "BurnState",
                                            "Distance")]
      prey.list <- prey.list[prey.list != c("Comments")]

  
  ### 7.2 Set Prey Density Calculator Parameters 
  
    ## a) remove data where birthday = NA
      main_data <- main_data[complete.cases(main_data[,c("Birthdate")]),]
    
    ## b) Prey Transect Time Window
      # Define the boundary dates plus and minus the date of interest 
      # (e.g. birthdate); values are in months
        start <- -3
        end <- 0
    
    ## c) Create New Epmty Dataframe  
      # This is an empty data frame that can store the prey density values
      transect_summary  <- c()  
  
          
  ### 7.3 Prey Density Calculator   
    ## a) Loop through each hyena in a data frame and calculate prey density
      for (i in 1:nrow(main_data)) { 
        date <- ymd(main_data$Birthdate[i])       # loop through 1:n dates
        
        #date <- round_date(date, "month")         # round dates to first of
                                                  # the closest month, this is 
                                                  # to avoid an over and under
                                                  # counting 
        
        start.date <- date %m+% months(start)     # start date minus # months
        
        end.date <- date %m+% months(end)         # start date plus # months
      
        day.interval <-  seq.Date(from = start.date,    # sequence of all days in
                            to = end.date,        # between start and end date
                            by = "day")
        
        # calculate the dates of prey transects that intersect with days in 
        # in the time interval
        date_overlap <- filter(tblPreyCount, 
                               as.Date(tblPreyCount$ActualDate) 
                               %in% day.interval) 
        
        # Summation of each prey species 
          # A row by row summation to calculate total prey counts during
          # each bi-montly sampling period for each transect
          date_overlap$total <- rowSums(date_overlap[,c(prey.list)], 
                                        na.rm = T)
                     
        # use dplyr gather function to transform data into long format 
        sum.prey <- date_overlap %>%
          gather_(key = "prey", value = "prey.count", c(prey.list, "total"))
        
        # calculate the density of each species of prey density counted along
        # each transect (in square kilometers); assumes prey are counted 
        # within 100m on each side of the transect
        sum.prey$density <- with (sum.prey, 
                                  prey.count/(as.numeric(Distance)*0.2)) 

        # calucate the average density of each species for each transect
        # (species density / # transects run)
        prey_stat <- ddply(sum.prey, .(prey), summarise, 
                           num_transects = sum(!is.na(prey)),
                           mean = mean(density, na.rm = T))

        # use dplyr spread function to transform data into wide format                    
        prey_stat <- spread(prey_stat, prey, mean)
        
        # add the newly created average prey densities to a new dataframe
        # over each iteration of the loop
        transect_summary <- rbind(transect_summary, prey_stat)
    
      }    
  
     
  ### 7.4 column bind transect summary with main_data    
  
    # add the newly created average prey densities to a new dataframe
    # over each iteration of the loop
      main_data <- cbind(main_data, transect_summary)    
      
  

#################################################
####       8. Save Intermediate Tables       ####
####             as Spreadsheets             ####
#################################################

  # Save intermediate tables as spreadsheets with a .cvs extension and today's
  # date. Files are saved in the 'data' folder or the 'output' folder
  # in the working directory.

  ### 8.1 Set up date parameters
    # print today's date
    today <- Sys.Date()
    date <- format(today, format="%d%b%Y")
   

  ### 8.2 Generate File Names
    # For each table that will be saved as a .csv file, first generate a file 
    # name to save each table
    
    ## a) File name for sample_request table
      csv.file.name.prey <- paste ("./basic_tools/prey_transect/output/prey_density", 
                                      date, ".csv", sep= "") 
   
      
  ### 8.3 Save Tables 
    # Save each data frame as a .csv file (a spreadsheet/table) into the 
    # data folder in the working directory.
    
    ## a) Save sample_request table
       write.csv (main_data, file = csv.file.name.prey)
  