################################################################################
#
#        Code to pull initial WQP dataset, HUC 12s, and demographic data
#               Developed by Sophia Bryson (sab159@duke.edu)
#               Masters Project for client Internet of Water
#                                 2021 - 2022
#
################################################################################

########################### WQP OVERVIEW & SETUP ###############################

# See WQP user guide here: https://www.waterqualitydata.us/portal_userguide/

# Data pulls from the WQP can be accomplished using the dataRetrieval package, 
# an interface for retrieving USGS and EPA hydrologic and water quality data:
#   readWQPdata() gets data from WQP
#   whatWQPdata() lists data available from WQP
#   tutorial here: https://owi.usgs.gov/R/dataRetrieval.html#1
#   explore all parameter codes with included data set 'parameterCdFile'
      # parameterCdFile <- parameterCdFile
      
# Data Retrieval functions for the WQP: 
   # ! readWQPdata	General Data Import from Water Quality Portal
   # readWQPqw	Raw Data Import for Water Quality Portal
   # readWQPsummary	Site Data Import from Water Quality Portal
   # whatWQPdata	Data Available from Water Quality Portal
   # whatWQPmetrics	Site Data Import from Water Quality Portal
   # whatWQPsamples	Site Data Import from Water Quality Portal
   # ! whatWQPsites	Site Data Import from Water Quality Portal
      
   # those marked with exclamation points above are the ones used here to access
   # site metadata (whatWQPsites) and records (readWQPdata)

# Parameters for pulls: 
   # Location: 
      # Options for country ("countrycode"), state ("statecode"), or county code ("countycode")
         # Country: "US"
         # State: "US:##", where ## is the state FIPS code)
         # County: "US:##:###", providings state FIPS (2) and County FIPS (3))
      # Country code pulls too much at once and causes server timeout error. So we'll stick with states.
      # Can specify multiple states at once - use "US:<2 digit state FIPS>" for each and sep with semi-colons. Do below. 
         #a note on FIPS codes: "certain numeric codes are reserved for possible future use in identifying [current territories]"
         #hence the omission of 03, 07, 14, 43, and 52. Correspondingly, FIPS codes for current states go up to 56.
      
      # NOTE: Moved to Environs
      # statenums <- seq(1,56) %>% str_pad(width = 2, side = "left", pad = "0") #add leading zeroes to get 2 digit codes for all states
      #    #remove invalid FIPS codes
      #    invalid_fips = c("03", "07", "14", "43", "52") #fips codes on reserve for territories, etc. 
      #    statenums <- statenums[statenums %notin% invalid_fips]
      # statecodes <- paste0("US:", statenums, ";") 
   
   # Other parameters (siteType, sampling characterisitcs, etc. will be handled through later filtering)
   # Adding more arguments makes timeout more likely. So just get all the sites, and then we'll filter them, and then we'll save them
      
######################### MONITORING SITE DATA FROM WQP ########################      
# --- PULL & FILTER SITE METADATA -----------------------------------------------
      
# Pull data from WQP for sets of states
   # too large of a request causes a server timeout error (504).
   # Ten at a time to prevent timeout, then bind up.
      
   #takes around 20 minutes total 
   states.1 <- whatWQPsites(statecode = statecodes[1:10]) 
   states.2 <- whatWQPsites(statecode = statecodes[11:20])
   states.3 <- whatWQPsites(statecode = statecodes[21:30])
   states.4 <- whatWQPsites(statecode = statecodes[31:40])
   states.5 <- whatWQPsites(statecode = statecodes[41:51])
   
   all.sites <- rbind(states.1, states.2, states.3, states.4, states.5) #had problems parsing, but it's all here. 
   length(unique(all.sites$StateCode)) #check for 50. Returned 51 - DC? indeed. 
    
   rm(states.1, states.2, states.3, states.4, states.5) #intermediate cleanup
   
# Filter sites to retain only sites of interest
   
   names(all.sites) #check what fields we have
   
   #monitoring location type:
   
      unique(all.sites$MonitoringLocationTypeName); table(all.sites$MonitoringLocationTypeName) #which ones do we (not) want?
      # Keeping all for now. Can & will filter later.
   
   summary(all.sites)
   
# Pare down to only retain fields of interest
   #documentation on fields can be accessed at https://www.waterqualitydata.us/portal_userguide/ 
   filtered.sites <- all.sites %>% select(OrganizationIdentifier, OrganizationFormalName, ProviderName, #organization metadata - organization vs. provider? 
                                          MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, #site metadata - left out description
                                          CountryCode, StateCode, CountyCode, HUCEightDigitCode, AquiferName, #hydro and political boundaries
                                          LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName) #location data 
   #left out accuracy, vertical measures, aquifer type, construction dates, well depths. 14 out of 36 fields retained.  
   
# Check datums, split, tranform, and remerge
   
   table(filtered.sites$HorizontalCoordinateReferenceSystemDatumName)
   
   #Drop those that are "Other" or have fewer than 10 instances. Retain NAD27, NAD83, WGS84:
   # Keeping also unkowns, as too many sites (95k+) are lost without them - see handling below
      
      datums.keep <- list("NAD27", "NAD83", "WGS84", "UNKWN", "Unknown")
      filtered.sites <- filtered.sites %>% filter(HorizontalCoordinateReferenceSystemDatumName %in% datums.keep)
      
      # #check retention
      # nrow(filtered.sites)/nrow(all.sites) #close to 95% retainted
      
   # Make it an sf - sort by CRS specified in metadata & define projections 
      
      NAD27.sites <- filtered.sites %>% filter(HorizontalCoordinateReferenceSystemDatumName == "NAD27") %>%
         st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure")) %>% 
         st_set_crs(4267) #crs from https://epsg.io/4267
      
      NAD83.sites <- filtered.sites %>% filter(HorizontalCoordinateReferenceSystemDatumName == "NAD83") %>%
         st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure")) %>% 
         st_set_crs(4269) #crs from https://epsg.io/4269 #These are each in their own state plane, technically. Error of up to 10m.
      
      WGS84.sites <- filtered.sites %>% filter(HorizontalCoordinateReferenceSystemDatumName == "WGS84") %>%
         st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure")) %>% 
         st_set_crs(4326) #crs from https://epsg.io/4326
      
      #Also keeping unknowns - too many sites were lost (60% + of sites with data in past 2 years) when filtered out. 
      # NOTE THE ASSUMPTION - assuming NAD83, which may be false. But can't afford to lose them and have not other way to assign.
      UNKWN.sites <- filtered.sites %>% filter(HorizontalCoordinateReferenceSystemDatumName %in% c("Unknown", "UNKWN")) %>%
                     st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure")) %>% 
                     st_set_crs(4269) 
   
   # Reproject all sites into NAD83, as NAD83 is the original datum of the greatest number of sites. 

      NAD27toNAD83 <- NAD27.sites %>% st_transform(crs = st_crs(NAD83.sites)) #technically needs additional transformation, but John Fay says max error of ~200 feet in teh absence of that
      
      WGS84toNAD83 <- WGS84.sites %>% st_transform(crs = st_crs(NAD83.sites))
      
      #Unknowns have been assumed to be NAD83. THis may not be true, but they don't require transformation here. 
      
   # Merge reprojected spatial datasets
      
      #cleanup - sometimes has issues otherwise
      gc()
      
      reprojected.sites <- rbind(NAD83.sites, NAD27toNAD83, WGS84toNAD83, UNKWN.sites) %>% 
                           rename(InitHorCoorRefSysDatumName = HorizontalCoordinateReferenceSystemDatumName)

      #split geometry into lat and long
      reprojected.sites <- reprojected.sites %>% mutate(x_long = unlist(map(reprojected.sites$geometry, 1)), 
                                                        y_lat = unlist(map(reprojected.sites$geometry, 2))) 
      reprojected.sites <- st_drop_geometry(reprojected.sites) #drop geometry for saveout
      

   # csv saveout of site metadata 
      write.csv(reprojected.sites, file = paste0(mp_wd_data, "AllWQPSiteMetadata.csv"), row.names = FALSE)
      
# Clean up
    rm(all.sites, datums.keep, filtered.sites, NAD27.sites, NAD27toNAD83, NAD83.sites, UNKWN.sites, WGS84.sites, WGS84toNAD83)
    gc()
      
# --- PULL & FILTER SITE RECORD DATA --------------------------------------------
      
# State parameters set above. Date parameters set in global variables. 
# Site descriptions will be used for filtering sites once they have been pulled. 
# Running by states since that offers better protection in case the WQP server goes down,
# as it seems to do with some frequency. 

# Get WQX records by state            
   # api call - documentation here: https://www.waterqualitydata.us/webservices_documentation/
   
   #Set up empty data frames      
   sitesMetadataAll <- matrix(nrow = 0, ncol = 36) 
   providerList <- matrix(nrow = 0, ncol = 2) 
   parameterList <- matrix(nrow = 0, ncol = 2) 
      
   #Load in parameter classifications
      #SDWA primary: https://www.epa.gov/ground-water-and-drinking-water/national-primary-drinking-water-regulations
      #SDWA secondary: https://www.epa.gov/sdwa/secondary-drinking-water-standards-guidance-nuisance-chemicals
      #CWA human health: https://www.epa.gov/wqc/national-recommended-water-quality-criteria-human-health-criteria-table
      #CWA aquatic life:  https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table
      #CWA organoleptic: https://www.epa.gov/wqc/national-recommended-water-quality-criteria-organoleptic-effects
      
      parameterClassification <- read.csv(file = paste0(mp_wd_data, "parameterPrevalenceClassified.csv"), header = TRUE, row.names = 1)
      # created manually from earlier pull. May be incomplete and need occassional manual update for parameters not included in the inital dataset on which the classification was made. 
      
      basic_WQ <- parameterClassification %>% filter(basic_wq == "yes")
      basic_WQParameters <- basic_WQ$CharacteristicName
      
      SDWA_Primary <- parameterClassification %>% filter(SDWA_Primary == "yes") 
      SDWA_PrimaryParameters <- SDWA_Primary$CharacteristicName
      
      SDWA_Secondary <- parameterClassification %>% filter(SDWA_Secondary == "yes")
      SDWA_SecondaryParameters <- SDWA_Secondary$CharacteristicName
      
      CWA_HH <- parameterClassification %>% filter(CWA_HumanHealth == "yes")
      CWA_HHParameters <- CWA_HH$CharacteristicName
      
      CWA_AL <- parameterClassification %>% filter(CWA_AquaticLife == "yes")
      CWA_ALParameters <- CWA_AL$CharacteristicName
      
      CWA_O <- parameterClassification %>% filter(CWA_Organoleptic == "yes")
      CWA_OParameters <- CWA_O$CharacteristicName
      
      #All regulatory parameters together:
      RegulatoryParameters <- c(SDWA_PrimaryParameters, SDWA_SecondaryParameters, CWA_HHParameters, CWA_ALParameters, CWA_OParameters) #catchall for other parameters
      
   
# Loop through states and retrieve WQP site records from period of interest  
   for(i in 1:length(statecodes)) { #takes 1 to 5 minutes per state, on average #vector becomes too large at state 50 - split in half, then filter, then join? Ended at 3:07, began at 12:51 - a little over 2 hours. Probably 3 once I get it all. 
      print(paste0("Began state ", i, " at ", now()))    #during dev - keep track of run time
      statecodepar <- statecodes[i]                      #set state of interest
      
      #call record for years of interest
      apiData <- readWQPdata(statecode = statecodepar,   
                             startDateLo = startDate,
                             startDateHi = endDate)
      
         #if no records (as will be the case for some - not all FIPS codes are used), SKIP!
         if (nrow(apiData) == 0) {next}
      
      # filter records by sample media and select only columns of interest - otherwise, too large of a memory allocation
         # notes on filtering & slimming: 
            #assuming end date is same as start date.
            #ignoring time zone differences, and times. 
            #ignoring actual values - depth, etc. Just want to know when and what was measured, not really interested in their results...
            #not sure what projectIdentifier is - left in for Blair 
            #not too worried about sample collection method for our purposes
         apiDataFiltered <- apiData %>% filter(ActivityMediaName == "Water" | ActivityMediaName == "water") %>% #filter and slim the record - otherwise, too large to save
                                        select("MonitoringLocationIdentifier", "OrganizationIdentifier",
                                               "OrganizationFormalName", "ActivityIdentifier", "ActivityTypeCode",
                                               "ActivityMediaName", "ActivityMediaSubdivisionName", "ActivityStartDate",
                                               "ProjectIdentifier", "ActivityConductingOrganizationText", "CharacteristicName",
                                               "ProviderName", "ResultStatusIdentifier")
      
      # Examine, filter, and reformat the site data
      siteData <- apiDataFiltered
      
         #Check out the data
         summary(siteData)
         length(unique(siteData$MonitoringLocationIdentifier)) #46599 sites with data. 
         
         #Are they distinct? Keep only distinct records 
         siteData <- distinct(siteData) #A few duplicate records. Unsure why, but now all distinct.
         dim(siteData)
         
         #Data quality filter - Remove records whose results were rejected - considered invalid data
         siteData <- siteData %>% filter(ResultStatusIdentifier != "Rejected") 
         
      # Handle providers - get list to saveout
         api_providerList <- siteData %>% select("OrganizationIdentifier", "OrganizationFormalName") %>% unique()
         providerList <- rbind(providerList, api_providerList)
         
      # Create tallies of parameters by class for each MEASURING INSTANCE? SITE? (Can always regroup measuring instances into sites)
         
         #handle parameters - get list to saveout
         api_parameterList <- unique(siteData$CharacteristicName)
         api_parameterPrevalence <- siteData %>% count(CharacteristicName, sort = TRUE) %>% rename(NumberOfRecords = n)
         parameterList <- rbind(parameterList, api_parameterPrevalence)
         
         #Add columns for binary classification of characteristics based on above classifications
         siteData_parClass <- siteData %>% mutate(Basic_WQ_Bool = CharacteristicName %in% basic_WQParameters) %>%
                                           mutate(SDWA_Primary_Bool = CharacteristicName %in% SDWA_PrimaryParameters) %>%
                                           mutate(SDWA_Secondary_Bool = CharacteristicName %in% SDWA_SecondaryParameters) %>%
                                           mutate(CWA_HH_Bool = CharacteristicName %in% CWA_HHParameters) %>%
                                           mutate(CWA_AL_Bool = CharacteristicName %in% CWA_ALParameters) %>%
                                           mutate(CWA_O_Bool = CharacteristicName %in% CWA_OParameters) %>%
                                           mutate(Other_Par_Bool = CharacteristicName %notin% c(basic_WQParameters, RegulatoryParameters))
         
         
         #Summarize parameters by monitoring instances, then remerge with all monitoring details by Activity ID, dropping characteristics
         # Use 'first' for all metadata - should be consistent for all characteristics, and create a single record (row) for each activity.
         # Use 'sum' for all parameters - will aggregate the number of relevant parameters measured in each category
         # Drop 'Characteristic name' as that has been summarized into the parameter counts
         
         MonInstanceParSummary <- siteData_parClass %>% group_by(ActivityIdentifier) %>% 
                                  summarise(MonitoringLocationIdentifier = first(MonitoringLocationIdentifier),
                                            OrganizationIdentifier = first(OrganizationIdentifier),
                                            OrganizationFormalName = first(OrganizationFormalName),
                                            ActivityTypeCode = first(ActivityTypeCode),
                                            ActivityMediaName = first(ActivityMediaName),
                                            ActivityMediaSubdivisionName = first(ActivityMediaSubdivisionName),
                                            ActivityStartDate = first(ActivityStartDate),
                                            ProjectIdentifier = first(ProjectIdentifier), 
                                            ActivityConductingOrganizationText = first(ActivityConductingOrganizationText),
                                            ProviderName = first(ProviderName),
                                            BasicWQ = sum(Basic_WQ_Bool),
                                            SDWAPrimary = sum(SDWA_Primary_Bool),
                                            SDWASecondary = sum(SDWA_Secondary_Bool),
                                            CWA_HH = sum(CWA_HH_Bool),
                                            CWA_AL = sum(CWA_AL_Bool),
                                            CWA_O = sum(CWA_O_Bool),
                                            Other = sum(Other_Par_Bool),
                                            .groups = "drop")
         #check the outputs
         summary(MonInstanceParSummary)
         
         # Aggregate to sites
         # Aggregate and then sum up - how many *UNIQUE* parameters have been measured at a site within the period of interest?
         SiteParSummary <- siteData_parClass %>% group_by(MonitoringLocationIdentifier) %>%
                           distinct(MonitoringLocationIdentifier, CharacteristicName, .keep_all = TRUE) %>% #get unique parameters measured at each location within past 2 years (some loss of resolution here - will not be able to filter for when these were measured, unless separate columns were made for each time frame of interest... )
                           summarise(OrganizationIdentifier = first(OrganizationIdentifier),
                                     OrganizationFormalName = first(OrganizationFormalName),
                                     ActivityTypeCode = first(ActivityTypeCode),
                                     ActivityMediaName = first(ActivityMediaName),
                                     ActivityMediaSubdivisionName = first(ActivityMediaSubdivisionName),
                                     ActivityStartDate = first(ActivityStartDate),
                                     ProjectIdentifier = first(ProjectIdentifier), 
                                     ActivityConductingOrganizationText = first(ActivityConductingOrganizationText),
                                     ProviderName = first(ProviderName),
                                     MonitoringInstances = length(unique(ActivityIdentifier)),
                                     BasicWQ = sum(Basic_WQ_Bool),
                                     SDWAPrimary = sum(SDWA_Primary_Bool),
                                     SDWASecondary = sum(SDWA_Secondary_Bool),
                                     CWA_HH = sum(CWA_HH_Bool),
                                     CWA_AL = sum(CWA_AL_Bool),
                                     CWA_O = sum(CWA_O_Bool),
                                     Other = sum(Other_Par_Bool),
                                     .groups = "drop")
         #This gives the classifications of the unique parameters measured at each monitoring location within the past 2 years (record period)
         
      # Determine whether sites are discrete or continuous... 
         
         #Calculate Days Between Monitoring Instances
         DaysBw <- MonInstanceParSummary %>% group_by(MonitoringLocationIdentifier) %>% arrange(ActivityStartDate, .by_group = TRUE)
         DaysBw <- DaysBw[order(DaysBw$ActivityStartDate),] 
         dayDiff <- c(NA, lead(DaysBw$ActivityStartDate) - DaysBw$ActivityStartDate) #calculate difference in days since preceding observation date
         DaysBw$DaysElapsed <- dayDiff[1:(length(dayDiff)-1)]
         #first value in any group will be based off of previous location at the moment... 
         max(dayDiff, na.rm = TRUE) 
         
         #Determine which sites are current
         Recency <- DaysBw %>% group_by(MonitoringLocationIdentifier) %>% 
                               summarize(min(ActivityStartDate), max(ActivityStartDate)) 
         
         #check if monitoring is still occurring (within past 35 days)
         DaysSinceMon <- today() - Recency$`max(ActivityStartDate)`
         Recency$DaysSinceMon <- DaysSinceMon
         Recency <- Recency %>% mutate(currentBool = ifelse(DaysSinceMon < 35, TRUE, FALSE)) #'current' refers to sites with monitoring in past 35 days
         
         #check how long ago monitoring first occurred (at least 3 months (95 days))
         DaysSinceStart <- today() - Recency$`min(ActivityStartDate)` #Days of Monitoring
         Recency$DaysSinceStart <- DaysSinceStart
         Recency <- Recency %>% mutate(newBool = ifelse(DaysSinceStart < 95, TRUE, FALSE)) #'new' refers to sites with monitoring beginning in the past 3 months
         
         #Assign continuous vs. discrete
            #Continuous: filter for sites where "current" = TRUE and "new" = FALSE and max(dayDiff) < 35  
            #Discrete: any others.
         OccurrenceClassification <- Recency %>% mutate(OccClass = ifelse((currentBool == TRUE & newBool == FALSE), "Continuous", "Discrete")) %>% #occurrence class - either discrete or continuous
                                     rename(EarliestDate = `min(ActivityStartDate)`, MostRecentDate = `max(ActivityStartDate)`)
         
         #Now remerge to site metadata!
         
         siteMetadata <- reprojected.sites %>% filter(MonitoringLocationIdentifier %in% siteData$MonitoringLocationIdentifier) #metadata for only those sites with data during period of interest   

         metadataUpdate <- merge(x = SiteParSummary, y = OccurrenceClassification, by = "MonitoringLocationIdentifier", all.x = TRUE, all.y = TRUE)
         
         updatedMetadata <- merge(x = siteMetadata, y = metadataUpdate, by = "MonitoringLocationIdentifier", all.x = TRUE, all.y = TRUE)
         
      
      #Add data to dataframe with site metadata
      updatedMetadataTable <- updatedMetadata %>% as.data.frame() #can't merge when in sf format since empty dataframe lacks crs and geometry  
      sitesMetadataAll <- rbind(sitesMetadataAll, updatedMetadataTable) 
      
      print(paste0(round(i *100/length(statecodes), 2), "% complete")) #watch the progress
      #sometimes seems to get stuck and needs a partial interrupt... not sure how best to fix that.
   }
      
   print(paste0("Finished running at ", now()))   #keep track of timing
   #not a problem that some loops report that the url returned no data: some FIPS codes are in reserve and not used
      
   #End report: 
   sitesData <- sitesMetadataAll %>% distinct() # Make sure there are not duplicates
   summary(sitesData)
   #some (336 at time of writing) lack geographic data. Those will be functionally omitted. Not much we can do about that. 
      
   #Pare down duplicates before saveout
   names(sitesData)
   sitesData <- sitesData %>% select(MonitoringLocationIdentifier, OrganizationIdentifier.x, OrganizationFormalName.x, ProviderName.x, 
                                     MonitoringLocationName, MonitoringLocationTypeName, MonitoringInstances,
                                     CountryCode, StateCode, CountyCode, HUCEightDigitCode, AquiferName,
                                     ActivityTypeCode, ActivityMediaName, ActivityMediaSubdivisionName,
                                     ActivityStartDate, ProjectIdentifier, ActivityConductingOrganizationText,
                                     BasicWQ, SDWAPrimary, SDWASecondary, CWA_HH, CWA_AL, CWA_O, Other,
                                     EarliestDate, MostRecentDate, DaysSinceMon, currentBool, DaysSinceStart, newBool, OccClass,
                                     InitHorCoorRefSysDatumName, x_long, y_lat) %>%
                               rename(OrganizationIdentifier = OrganizationIdentifier.x, OrganizationFormalName = OrganizationFormalName.x,
                                      ProviderName = ProviderName.x)
    
#Add provider classifications to site data:
   
   #pull in manual classification of providers
   providerClassification <- read_xlsx(paste0(mp_wd_data, "ProviderListClassified.xlsx"), sheet = "ProviderList") %>% 
                             select(OrganizationIdentifier, OrganizationFormalName, OrganizationTypeClassification)
   
   #identify and saveout list of new providers needing classification
   newProv <- providerList %>% filter(OrganizationIdentifier %notin% providerClassification$OrganizationIdentifier) %>% distinct() #label these as "not yet classified"
   write.csv(newProv, file=paste0(mp_wd_data, "TEMP_NewProvidersToClassify.csv")) #save out new additions - copy into classification list & manually classify
   
   #match classification to site data by provider
   sitesData <- merge(x = sitesData, y = providerClassification, by = "OrganizationIdentifier", all.x = TRUE) 
   
   sitesData <- sitesData %>% mutate(OrganizationTypeClassification = ifelse(is.na(OrganizationTypeClassification), "Not Yet Classified", OrganizationTypeClassification)) #handle unclassified providers
   #also handles sites where providing organization is itself NA... 
   
   sitesData <- sitesData %>% select(MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, MonitoringInstances,
                                     OrganizationIdentifier, OrganizationFormalName.x, OrganizationTypeClassification,
                                     ProviderName, CountryCode, StateCode, CountyCode, HUCEightDigitCode, AquiferName,
                                     ActivityTypeCode, ActivityMediaName, ActivityMediaSubdivisionName,
                                     ActivityStartDate, ProjectIdentifier, ActivityConductingOrganizationText,
                                     BasicWQ, SDWAPrimary, SDWASecondary, CWA_HH, CWA_AL, CWA_O, Other,
                                     EarliestDate, MostRecentDate, DaysSinceMon, currentBool, DaysSinceStart, newBool, OccClass,
                                     InitHorCoorRefSysDatumName, x_long, y_lat) %>%
                              rename(OrganizationFormalName = OrganizationFormalName.x)
   
           
#Save out:
   #Updated site (meta)data 
   write.csv(sitesData, file = paste0(mp_wd_data, "WQPSiteData.csv"), row.names = FALSE)
  
   #Export provider data for manual classification:
   write.csv(providerList, file = paste0(mp_wd_data, "ProviderList.csv"), row.names = FALSE) #save out for Blair for manual classification

   #Save out measured parameters for manual classification (then rerun the loop above if needed (first time))
   write.csv(parameterList, file = paste0(mp_wd_data, "ParameterPrevalence.csv")) #save out to manually classify
   
# Clean up 
   rm(api_parameterPrevalence, api_providerList, apiData, apiDataFiltered, basic_WQ, CWA_AL, CWA_HH, CWA_O,
      DaysBw, metadataUpdate, MonInstanceParSummary, newProv, OccurrenceClassification, parameterList, providerClassification,
      providerList, Recency, SDWA_Primary, SDWA_Secondary, siteData, siteData_parClass, siteMetadata, SiteParSummary, sitesMetadataAll,
      updatedMetadataTable, updatedMetadata)
   gc()
   
######################## HUC 12 DATA FROM WBD ##################################                            
# --- Access HUC 12s from NHDPlus WBD -------------------------------------------
   
   # # Download Watershed Boundary Dataset - IMMENSE
   # # Metadata at https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.xml
   # nhdplusTools::download_wbd(outdir = paste0(mp_wd_data, "\\wbd\\")) #takes a while. Doesn't need to be run every time, just for initial set up.
   # wbd <- read_sf(paste0(mp_wd_data, "wbd//WBD_National_GDB//WBD_National_GDB.gdb")) #If storage issues: https://stackoverflow.com/questions/10917532/memory-allocation-error-cannot-allocate-vector-of-size-75-1-mb
      
   #try a different (non-download) strategy: 
   # Use get_huc12 function of nhdplusTools to pull subbasins. Use state multipolygons as AOI.
      
   #Get national SF from census bureau to use as AOI in huc pull
   US.sf <- get_acs(geography = "state", state = statenums, variables = "B01001_001E", geometry = TRUE) #just one var to get geometry
   US.sf <- US.sf %>% select(GEOID, NAME, geometry) 
   
   #create a seed sf outside of loop to which to bind subsequent iterations
   state.sf <- US.sf %>% filter(GEOID == statenums[1]) # Geometry check can only do one multipologyon at a time
   state.sf <- st_make_valid(state.sf) # Check & select geometry validity
   state.hucs <- get_huc12(state.sf) #pass state multipolygon as AOI for which to pull huc12s
   huc12 <- state.hucs %>% select(huc12, name, states, areasqkm, hutype, tohuc)
   
   for(i in 2:length(statenums)) {
     if(i ==2) {print("SKIPPING ALASKA - added after loop"); next} #AK is 
     if(i == 19) { #AK and LA don't pull properly from wbd. Use ESRI rest endpoint & arcpullr instead. 
       endpointURL <- "https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/6"
       stateabb <- "LA"
       state.hucs <- get_spatial_layer(endpointURL,
                                       out_fields = c("huc12", "name", "states", "areasqkm", "hutype", "tohuc"),
                                       where = paste0("states LIKE '%", stateabb, "%'"))
       state.hucs <- state.hucs %>% st_transform(crs = st_crs(huc12)) %>% rename(geometry = geoms)
       huc12 <- rbind(huc12, state.hucs)
       print(paste0("Completed loop ", i)) 
       next
     } #end of AK/LA exception loop, return to normal loop
     
     state.sf <- US.sf %>% filter(GEOID == statenums[i]) # Geometry check can only do one multipologyon at a time
     state.sf <- state.sf[s2_is_valid(state.sf) == TRUE & !is.na(state.sf$geometry)] # Check & select geometry validity
     state.hucs <- get_huc12(state.sf) #pass state multipolygon as AOI for which to pull huc12s
     state.hucs <- state.hucs %>% select(huc12, name, states, areasqkm, hutype, tohuc) #select only fields of interest
     huc12 <- rbind(huc12, state.hucs)
     print(paste0("Completed loop ", i))
     
   }
    
   bkup <- huc12
   dim(huc12)
   
   #Now get and add AK, since it was too large f an allocation in the the loop but seems to work outside?
   gc()
   endpointURL <- "https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/6"
   stateabb <- "AK"
   AK.hucs <- get_spatial_layer(endpointURL,
                                out_fields = c("huc12", "name", "states", "areasqkm", "hutype", "tohuc"),
                                where = paste0("states LIKE '%", stateabb, "%'"))
   AK.hucs <- AK.hucs %>% st_transform(crs = st_crs(huc12)) %>% rename(geometry = geoms)
   huc12 <- rbind(AK.hucs, huc12)
   
   # Make this for later before removing anything: 
   to <- huc12 %>% select(huc12, tohuc) %>% st_drop_geometry() %>% #need to have only one geometric object for merge
                   mutate(tohuc = ifelse(huc12 == "040203000200", "LAKE SUPERIOR", tohuc)) #manual fix for Isle Royal - lacks 'tohuc' value
   
    #save this out - it'll be used in the update script also
    write.csv(to, file = paste0(mp_wd_data, "ToHUC.csv"), row.names = FALSE)
   
   #Simplify & combining geometry to get just one record per huc12
   # dropping hutype - don't need. Records should be same for name and states, so retain first. Sum area, concatenate 'tohucs', and use union of geometry. 
   # should also take care of duplicates at borders
   nrow(huc12); length(unique(huc12$huc12)) #8000 excess rows
   
     #work with subsets for efficiency's sake
     huc6 <- substr(huc12$huc12, 1, 6) %>% unique()  #just a category for looping
     
     # first one outside loop to have an sf onto which to bind
     huc6Sel = huc6[1]
     huc12_subset <- huc12 %>% filter(substr(huc12, 1, 6) == huc6Sel)
     huc12subset_unified <- huc12_subset %>% group_by(huc12) %>% 
                                             summarise(name = first(name), 
                                                       states = first(states), 
                                                       areasqkm = sum(areasqkm), 
                                                       tohucs = paste(tohuc, collapse = ","),
                                                       geometry = st_union(geometry)) 
     huc12_unified <- huc12subset_unified
     
     #Loop through huc6s, get single records for each huc12 and save back out
     for(i in 2:length(huc6)) {
       huc6Sel = huc6[i]
       huc12_subset <- huc12 %>% filter(substr(huc12, 1, 6) == huc6Sel)
       huc12subset_unified <- huc12_subset %>% group_by(huc12) %>% 
                                               summarise(name = first(name), 
                                                         states = first(states), 
                                                         areasqkm = sum(areasqkm), 
                                                         tohucs = paste(tohuc, collapse = ","),
                                                         geometry = st_union(geometry)) 
       huc12_unified <- rbind(huc12_unified, huc12subset_unified)
       print(paste0(round(i*100/length(huc6), 2), "% complete"))
     }
     
     summary(huc12_unified); dim(huc12_unified); length(unique(huc12_unified$huc12)) #Great! One record per huc12. Alleviates need for "distinct"
   
     huc12 <- huc12_unified

# clean up
   rm(state.sf, state.hucs, endpointURL, stateAbb, bkup, AK.hucs, huc12_unified, 
      huc12_subset, huc12subset_unified)
   gc()
   
########################## ENRICH HUC 12 DATA ##################################                            
# --- Add EPA Regions to huc12 table --------------------------------------------
   
#Fetch EPA Regions shapefile from ESRI REST endpoint   
   endpointURL <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/EPA_Regions/FeatureServer/0"
   epa.regions <- get_spatial_layer(endpointURL,
                                    out_fields = c("FID", "OBJECTID", "EPAREGION"),
                                    where = "1=1") #fetch all records
   epa.regions <- epa.regions %>% st_transform(crs = st_crs(huc12)) %>% rename(geometry = geoms)
   
# Use the huc6 loop again to join - use over() to extract values
   
   # first one outside loop to have an sf onto which to bind
   huc6Sel = huc6[1]
   huc12_subset <- huc12 %>% filter(substr(huc12, 1, 6) == huc6Sel)
   huc12_subset_epa <- st_join(x = huc12_subset, y = epa.regions, largest = TRUE) %>% #assigns region with greatest area of overlap for hucs falling on the border of EPA regions
                       select(huc12, name, states, EPAREGION, areasqkm, tohucs) %>% 
                       rename(EPAregion = EPAREGION)
   huc12_epa <- huc12_subset_epa

   #Loop through huc6s, get single records for each huc12 and save back out
   for(i in 2:length(huc6)) {
     huc6Sel = huc6[i]
     huc12_subset <- huc12 %>% filter(substr(huc12, 1, 6) == huc6Sel)
     huc12_subset_epa <- st_join(x = st_make_valid(huc12_subset), y = epa.regions, largest = TRUE) %>% #assigns region with greatest area of overlap for hucs falling on the border of EPA regions
                         select(huc12, name, states, EPAREGION, areasqkm, tohucs) %>% 
                         rename(EPAregion = EPAREGION)
     huc12_epa <- rbind(huc12_epa, huc12_subset_epa)
     print(paste0(round(i*100/length(huc6), 2), "% complete"))
   }
   
   summary(huc12_epa) #if it looks right, assign it over the last huc12, and create an intermediate saveout
   
   huc12 <- huc12_epa
   
#Clean up
rm(epa.regions, huc12_subset, huc12_subset_epa)
gc()


######################### PULL & TIDY CENSUS/ACS DATA ##########################
# Access ACS data on population & demographics. 
# Use tidycensus package
# --- Identify variables of interest -------------------------------------------   

#identify variables of interest - overview of census bureau products here: https://censusreporter.org/topics/table-codes/

# Census codes for tables of interest: #TABLES CAN ONLY PULL ONE AT A TIME - DO VARS INSTEAD (reference https://github.com/NIEPS-Water-Program/water-affordability/blob/main/rcode/access1_census_data.R)
# Population = "B01003"
# Race = "B02001" #population by race
# Ethnicity = "B03001" #population by ethnicity
# Age = "B06001" #population by age - UNNEEDED??? 
# Income = "B19001" #number of households in different income brackets
# Poverty = "C17001" #population with income below poverty status

# Census codes for variables of interest:
TotalPop = "B01001_001E" #total population
TotalHH = "B19001_001E" #number of households
MedHHInc = "B19013_001E" #median household income
TotalPov = "B17001_001E" #total below poverty line

#Get the appropriate variable codes (based on api definition) based on the above tables!
#https://www.census.gov/content/dam/Census/data/developers/acs/acs-data-variables-guide.pdf

#table B02001 - race - https://censusreporter.org/tables/B02001/
RaceVars = c("B02001_001E", "B02001_002E", "B02001_003E", "B02001_004E", "B02001_005E",
             "B02001_006E", "B02001_007E", "B02001_008E", "B02001_009E", "B02001_010E") 

#table B03001 - Hispanic/Latino - https://censusreporter.org/tables/B03001/
EthnicityVars = c("B03001_001E", "B03001_002E", "B03001_003E") 

#table B19001 - HH income bracket - https://censusreporter.org/tables/B19001/
IncomeVars = c("B19001_001E", "B19001_002E", "B19001_003E", "B19001_004E", "B19001_005E",
               "B19001_006E", "B19001_007E", "B19001_008E", "B19001_009E", "B19001_010E",
               "B19001_011E", "B19001_012E", "B19001_013E", "B19001_014E", "B19001_015E",
               "B19001_016E", "B19001_017E") 

# NOTE: Poverty vars being ommitted to avoid aggregations of aggregations. Focus on HH income instead. 
#table B17001 - poverty status - https://censusreporter.org/tables/B17001/
# PovertyVars = c("B17001_001E", "B17001_002E", "B17001_031E") #total, below poverty status, above poverty status (income in past 12 months)
# notes on poverty status and measures can be found here: https://www.census.gov/topics/income-poverty/poverty/guidance/poverty-measures.html

# NOTE: Homeownership vars being omitted to limit scope of project.
# #table B25003 - tenure - https://censusreporter.org/tables/B25003/
# HomeOwnershipVars = c("B25003_001E", "B25003_002E", "B25003_003E") #total, owner, renter

#set other parameters
acsEndYear = 2019 #most recent 5-year ACS goes through 2019. 2020 comes out in March of 2022. Update yearly as new products are released.
acsSpan = 5 #5 year needed to get BG level data, plus more stable (can be 1, 3, or 5)

# --- Get geometry -------------------------------------------------------------
# Just pull once for faster requests, then merge with otehr variables after transforming

# Block group geometry:
bg.sf <- get_acs(geography = "block group", variables = "B01001_001E", state = statenums, #just one var to get geometry
                 year = acsEndYear, survey = paste0("acs", acsSpan), 
                 geometry = TRUE, keep_geo_vars = TRUE) %>%
  select(STATEFP, GEOID, ALAND, AWATER) #name will come with other vars

# Tract geometry:
tract.sf <- get_acs(geography = "tract", variables = "B01001_001E", state = statenums, #just one var to get geometry
                    year = acsEndYear, survey = paste0("acs", acsSpan), 
                    geometry = TRUE, keep_geo_vars = TRUE) %>%
  select(STATEFP, GEOID, ALAND, AWATER) #name will come with other vars


# --- Get variables -------------------------------------------------------------

# Block group level: Race, income, and homeownership

acsRace <- get_acs(geography = "block group", variables = RaceVars, state = statenums,
                   year = acsEndYear, survey = paste0("acs", acsSpan), 
                   geometry = FALSE, keep_geo_vars = TRUE)

BGRace <- acsRace %>% mutate(race = ifelse(variable=="B02001_001", "Total_Population", 
                                           ifelse(variable=="B02001_002", "White", 
                                                  ifelse(variable=="B02001_003", "Black", 
                                                         ifelse(variable=="B02001_004", "Native",
                                                                ifelse(variable=="B02001_005", "Asian", 
                                                                       ifelse(variable=="B02001_006","Pacific_Islander", 
                                                                              ifelse(variable == "B02001_007", "Other", 
                                                                                     ifelse(variable %in% c("B02001_008", "B02001_009", "B02001_010"), "Multiple_Races", NA))))))))) %>%
  select(GEOID, NAME, race, estimate, moe) %>% #drop variable code (now in 'race') 
  pivot_wider(id_cols = !moe, names_from = race, values_from = estimate, values_fn = sum) #id_cols expression will group by GEOID & name and drop moe. Sum will total up values from "Multiple Races" 


acsIncome <- get_acs(geography = "block group", variables = IncomeVars, state = statenums,
                     year = acsEndYear, survey = paste0("acs", acsSpan), 
                     geometry = FALSE, keep_geo_vars = TRUE)

BGIncome <- acsIncome %>% mutate(HHIncome = ifelse(variable == "B19001_001", "Total_HH", #name ranges by upper limit (thousands of dollars)
                                                   ifelse(variable == "B19001_002", "hh10",
                                                          ifelse(variable == "B19001_003", "hh15",
                                                                 ifelse(variable == "B19001_004", "hh20",
                                                                        ifelse(variable ==  "B19001_005", "hh25",
                                                                               ifelse(variable == "B19001_006", "hh30",
                                                                                      ifelse(variable == "B19001_007", "hh35",
                                                                                             ifelse(variable == "B19001_008", "hh40",
                                                                                                    ifelse(variable == "B19001_009", "hh45",
                                                                                                           ifelse(variable == "B19001_010", "hh50",
                                                                                                                  ifelse(variable == "B19001_011", "hh60",
                                                                                                                         ifelse(variable == "B19001_012", "hh75",
                                                                                                                                ifelse(variable == "B19001_013", "hh100",
                                                                                                                                       ifelse(variable == "B19001_014", "hh125",
                                                                                                                                              ifelse(variable == "B19001_015", "hh150", 
                                                                                                                                                     ifelse(variable == "B19001_016", "hh200",
                                                                                                                                                            ifelse(variable == "B19001_017", "hh200more", NA)))))))))))))))))) %>% 
  select(GEOID, NAME, HHIncome, estimate, moe) %>% #drop variable code (now in 'race') 
  pivot_wider(id_cols = !moe, names_from = HHIncome, values_from = estimate) %>% #id_cols expression will group by GEOID & name and drop moe. Sum will total up values from "Multiple Races" 
  mutate(d0to24k = (hh10 + hh15 + hh20 + hh25), d25to49k = (hh30 + hh35 + hh40 + hh45 + hh50),
         d50to74k = (hh60 + hh75), d75to99k = hh100, d100to124k = hh125, d125to150k = hh150,
         d150kmore = (hh200 + hh200more)) %>% #reclassify into equal interval ($25k) buckets 
  select(GEOID, NAME, Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore)

# OMITTING tenure. Can re-add if desired in the future. 
# acsHomeownership <- get_acs(geography = "block group", variables = HomeOwnershipVars, state = statenums,
#                             year = acsEndYear, survey = paste0("acs", acsSpan), 
#                             geometry = FALSE, keep_geo_vars = TRUE)
# 
# BGHomeownership <- acsHomeownership %>% mutate(homeOwnership = ifelse(variable == "B25003_001", "Total_Housing_Units",
#                                                                       ifelse(variable == "B25003_002", "Owner",
#                                                                              ifelse(variable == "B25003_003", "Renter", NA)))) %>%
#                                         select(GEOID, NAME, homeOwnership, estimate, moe) %>% #drop variable code (now in 'race') 
#                                         pivot_wider(id_cols = !moe, names_from = homeOwnership, values_from = estimate) #id_cols expression will group by GEOID & name and drop moe.


# Tract level: Poverty, ethnicity

# OMITTING poverty (using HH income instead). Can re-add if desired in the future. 
# acsPoverty <- get_acs(geography = "tract", variables = PovertyVars, state = statenums,
#                       year = acsEndYear, survey = paste0("acs", acsSpan), 
#                       geometry = FALSE, keep_geo_vars = TRUE)
# 
# TractPoverty <- acsPoverty %>% mutate(poverty = ifelse(variable == "B17001_001", "Total_Poverty_Survey_Pop",
#                                                        ifelse(variable == "B17001_002", "Below Poverty Level",
#                                                               ifelse(variable == "B17001_031", "Above Poverty Level", NA)))) %>%
#                                select(GEOID, NAME, poverty, estimate, moe) %>% #drop variable code (now in 'race') 
#                                pivot_wider(id_cols = !moe, names_from = poverty, values_from = estimate) #id_cols expression will group by GEOID & name and drop moe.


acsEthnicity <- get_acs(geography = "tract", variables = EthnicityVars, state = statenums,
                        year = acsEndYear, survey = paste0("acs", acsSpan), 
                        geometry = FALSE, keep_geo_vars = TRUE)

TractEthnicity <- acsEthnicity %>% mutate(ethnicity = ifelse(variable == "B03001_001", "Total_Pop",
                                                             ifelse(variable == "B03001_002", "Not Hispanic/Latino",
                                                                    ifelse(variable == "B03001_003", "Hispanic/Latino", NA)))) %>%
  select(GEOID, NAME, ethnicity, estimate, moe) %>% #drop variable code (now in 'race') 
  pivot_wider(id_cols = !moe, names_from = ethnicity, values_from = estimate) #id_cols expression will group by GEOID & name and drop moe.


# --- Consolidate BG and Tract level variables & merge with geometry -----------

#consolidate blockgroup vars
acsBGData <- merge(BGRace, BGIncome, by = "GEOID") %>% #merge(BGHomeownership, by = "GEOID") %>% 
  mutate(NAME = NAME.x) %>% 
  select(GEOID, NAME, Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races,
         Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore)
#Total_Housing_Units, Owner, Renter) 

#match with blockgroup geometry
bgGeoData <- merge(bg.sf, acsBGData, by = "GEOID")

#consolidate tract vars
acsTractData <- TractEthnicity

#match with tract geometry
tractGeoData <- merge(tract.sf, acsTractData, by = "GEOID")

#Clean up 
rm(acsEthnicity, acsIncome, acsRace, bg.sf, BGIncome, BGRace, tract.sf, TractEthnicity)
gc()


########################### JOIN ACS DATA TO HUCS ############################## 
# Area weighted spatial interpolation to attribute BG & Tract level demographics to HUC12s. 
# While the assumption of uniform spatial distribution may be inaccurate, it's what we've got.
# https://geocompr.robinlovelace.net/spatial-operations.html#spatial-joining
# --- Areal Weighted Interpolation set up & seed creation ----------------------

  # Define function that prevents loop breaking when the huc sf has 2 rows: 
  safe_aw_interpolate <- function(census_sf, huc_sf){
    tryCatch({output_sf <- st_interpolate_aw(census_sf, to = huc_sf, extensive = TRUE, keep_NA = TRUE)},
             error = function(e) {output_sf <- st_interpolate_aw(census_sf, to = huc_sf, extensive = TRUE, keep_NA = FALSE)})
  }

  # state-by-state loop - far too computationally extensive otherwise

  #first one outside of loop to create SF onto which to bind
    # Simplify as much as possible - it's really struggling with these computations. Even still, slow going (matter of days)
    # But this only needs to be run once per year (when ACS data is updated)
  
    # List of unique state combinations through which to loop
    statesList <- unique(huc12$states)
    statesList <- statesList %>% subset(!is.na(statesList) & statesList %notin% c("MEX", "BC")) #drop non-US states - no ACS match
  
    statesSel = statesList[2] #skip AK for now - it has 11k + sites. Work it in later
    statesIncl <- unlist(strsplit(statesSel, ","))
    
    # abbreviation used in hucs, fips used in sites. Match. 
    fipsSel <- fips_codes %>% filter(state %in% statesIncl) %>% select(state_code) %>% unique()
    
    # filter to HUCs of interest
    hucsOfInt <- huc12 %>% select(huc12, states) %>% filter(states == statesSel)
    
    # filter to BGs and tracts of interest & get rid of non-numeric/non-extensive cols
    BGsOfInt <- bgGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
                              select(Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races,
                                     Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore) %>%
                              filter(lengths(st_intersects(., hucsOfInt)) > 0)
    
    
    TractsOfInt <- tractGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
                                    select(Total_Pop, `Not Hispanic/Latino`, `Hispanic/Latino`) %>%
                                    filter(lengths(st_intersects(., hucsOfInt)) > 0)
    
    # sites within for hucs and sites of interest 
    state_huc12_bg_sf <- st_interpolate_aw(BGsOfInt, to = hucsOfInt, extensive = TRUE, keep_NA = TRUE) # add block group data to huc12 polys. Keep NAs to keep length same as huc12 list. 
    state_huc12_bg_sf$huc12 <- hucsOfInt$huc12
    
    state_huc12_tract_sf <- st_interpolate_aw(TractsOfInt, to = hucsOfInt, extensive = TRUE, keep_NA = TRUE) # add tract data to BG-augmented huc polys
    state_huc12_tract_sf$huc12 <- hucsOfInt$huc12
    
    #Merge bg and tract level data into single sf
    state_huc12_bg_tract_sf <- data.frame(state_huc12_bg_sf, state_huc12_tract_sf) %>% 
                               select(huc12, Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races, 
                                      Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore,
                                      Total_Pop, Not.Hispanic.Latino, Hispanic.Latino, geometry)
    
    #create seed sf to which to bind all outputs
    huc12_bg_tract_sf <- state_huc12_bg_tract_sf 
    
    
    table(huc12$states) #to estimate how long each loop'll take - time correlated with hucs in state grouping
    head(huc12_bg_tract_sf)
  

# --- Areal Weighted Interpolation - loop through states ----------------------
  
  for(i in 1:length(statesList)) { 
    if(i == 1) {print("SKIPPING ALASKA - added after loop"); next} #AK is appended at the end (10k+ sites - wanted faster progress to check funciton at start)
    if(i == 2) {print("First state pulled outside loop as seed sf."); next} #First run outside of loop to create SF to which to bind
    #really starts at i = 3
    
    statesSel = statesList[i]
    statesIncl <- unlist(strsplit(statesSel, ",")) %>% toupper() #catch any that may be lowercase ("co" at one point)
    statesIncl <- statesIncl %>% subset(statesIncl %notin% c("MEX", "CN", "CAN", "BC", " ")) #remove non-US geographies 
    if(length(statesIncl) == 0) {print(paste0("No valid states for i = ", i, ". Skipping.")); next}
    
    # abbreviation used in hucs, fips used in sites. Match. 
    fipsSel <- fips_codes %>% filter(state %in% statesIncl) %>% select(state_code) %>% unique()
    
    # filter to HUCs of interest
    hucsOfInt <- huc12 %>% select(huc12, states) %>% filter(states == statesSel)
    
    # filter to BGs and tracts of interest & get rid of non-numeric/non-extensive cols
    BGsOfInt <- bgGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
                              select(Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races,
                                     Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore) %>%
                              filter(lengths(st_intersects(., hucsOfInt)) > 0)
    
    TractsOfInt <- tractGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
                                    select(Total_Pop, `Not Hispanic/Latino`, `Hispanic/Latino`) %>% 
                                    filter(lengths(st_intersects(., hucsOfInt)) > 0)
    
    # Too many records still exhausts memory. SO, subset (250 per time - that's worked before)
    if(nrow(hucsOfInt) > 200) {
      
      intervals <- seq(1, nrow(hucsOfInt), 200)
      
      for(v in 1:length(intervals)){ 
        
        # grab a subset of hucs with which to work
        if (v == length(intervals)) { 
          subset_hucsOfInt <- hucsOfInt[intervals[v]:nrow(hucsOfInt),] 
        } else{ 
          subset_hucsOfInt <- hucsOfInt[intervals[v]:intervals[v+1]-1,]}
        
        #pare down BGs and tracts based on intersection with selected hucs
        subset_BGsOfInt <- BGsOfInt %>% filter(lengths(st_intersects(., subset_hucsOfInt)) > 0)
        subset_TractsOfInt <- TractsOfInt %>% filter(lengths(st_intersects(., subset_hucsOfInt)) > 0)
        
        #interpolate to the huc12 subset with the census geogrpahy subsets using safe interpolation defined above
        subset_state_huc12_bg_sf <- safe_aw_interpolate(subset_BGsOfInt, subset_hucsOfInt)
        subset_state_huc12_bg_sf$huc12 <- subset_hucsOfInt %>% subset(geometry %in% subset_state_huc12_bg_sf$geometry) %>% .$huc12
        
        subset_state_huc12_tract_sf <- safe_aw_interpolate(subset_TractsOfInt, subset_hucsOfInt)
        subset_state_huc12_tract_sf$huc12 <- subset_hucsOfInt %>% subset(geometry %in% subset_state_huc12_tract_sf$geometry) %>% .$huc12
        
        #merge BG and tract sfs
        subset_state_huc12_bg_tract_sf <- data.frame(subset_state_huc12_bg_sf, subset_state_huc12_tract_sf) %>% 
                                          select(huc12, Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races, 
                                                 Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore,
                                                 Total_Pop, Not.Hispanic.Latino, Hispanic.Latino, geometry)
                                        
        # then add that subset to the output sf
        huc12_bg_tract_sf <- rbind(huc12_bg_tract_sf, subset_state_huc12_bg_tract_sf)
        
        # the print update
        print(paste0(round(v*100/length(intervals), 2), "% of state miniloops complete"))
        
        if (v == length(intervals)) {v = 1} #reset v upon completion of states
        
      } #end looping through to subset state huc12s
      
      #Update and move to next state selection
      print(paste0(round(i*100/length(statesList), 2), "% complete"))
      next
      
    } #end exception for state selections with many huc12s
    
    # interpolate to the huc12 subset with the census geogrpahy subsets using safe interpolation defined above
    state_huc12_bg_sf <- safe_aw_interpolate(BGsOfInt, hucsOfInt)
    state_huc12_bg_sf$huc12 <- hucsOfInt %>% subset(geometry %in% state_huc12_bg_sf$geometry) %>% .$huc12
    
    state_huc12_tract_sf <- safe_aw_interpolate(TractsOfInt, hucsOfInt)
    state_huc12_tract_sf$huc12 <- hucsOfInt %>% subset(geometry %in% state_huc12_tract_sf$geometry) %>% .$huc12
    
    #Merge bg and tract level data into single sf
    state_huc12_bg_tract_sf <- data.frame(state_huc12_bg_sf, state_huc12_tract_sf) %>% 
                               select(huc12, Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races, 
                                      Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore,
                                      Total_Pop, Not.Hispanic.Latino, Hispanic.Latino, geometry)
    
    huc12_bg_tract_sf <- rbind(huc12_bg_tract_sf, state_huc12_bg_tract_sf)
    print(paste0(round(i*100/length(statesList), 2), "% complete"))
    
  } #time for each loop varies based on number of hucs & bgs in given state(s) - ranges from 1 to 4k+ 
  
  summary(huc12_bg_tract_sf) # so far so good. Now get AK. 

# --- Areal Weighted Interpolation for AK ---------------------------------------
  
  #Go back and  get AK (10k+ huc12s):     
  statesSel = statesList[1] #Alaska
  statesIncl <- unlist(strsplit(statesSel, ",")) %>% toupper() #catch any that may be lowercase ("co" at one point)
  statesIncl <- statesIncl %>% subset(statesIncl %notin% c("MEX", "CN", "CAN", "BC")) #remove non-US geographies 
  
  # abbreviation used in hucs, fips used in sites. Match. 
  fipsSel <- fips_codes %>% filter(state %in% statesIncl) %>% select(state_code) %>% unique()
  
  # filter to HUCs of interest
  hucsOfInt <- huc12 %>% select(huc12, states) %>% filter(states == statesSel)
  
  # filter to BGs and tracts of interest & get rid of non-numeric/non-extensive cols
  BGsOfInt <- bgGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
                            select(Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races,
                                   Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore) %>%
                            filter(lengths(st_intersects(., hucsOfInt)) > 0)
  
  TractsOfInt <- tractGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
                                  select(Total_Pop, `Not Hispanic/Latino`, `Hispanic/Latino`) %>% 
                                  filter(lengths(st_intersects(., hucsOfInt)) > 0)
  
  # Too many records still exhausts memory. SO, subset (250 per time - that's worked before)
  if(nrow(hucsOfInt) > 200) {
    
    intervals <- seq(1, nrow(hucsOfInt), 200)
    
    for(v in 1:length(intervals)){ 
      
      # grab a subset of hucs with which to work
      if (v == length(intervals)) { 
        subset_hucsOfInt <- hucsOfInt[intervals[v]:nrow(hucsOfInt),] 
      } else{ 
        subset_hucsOfInt <- hucsOfInt[intervals[v]:intervals[v+1]-1,]}
      
      #pare down BGs and tracts based on intersection with selected hucs
      subset_BGsOfInt <- BGsOfInt %>% filter(lengths(st_intersects(., subset_hucsOfInt)) > 0)
      subset_TractsOfInt <- TractsOfInt %>% filter(lengths(st_intersects(., subset_hucsOfInt)) > 0)
      
      # interpolate to the huc12 subset with the census geogrpahy subsets using safe interpolation defined above
      
      subset_state_huc12_bg_sf <- safe_aw_interpolate(subset_BGsOfInt, subset_hucsOfInt)
      subset_state_huc12_bg_sf$huc12 <- subset_hucsOfInt %>% subset(geometry %in% subset_state_huc12_bg_sf$geometry) %>% .$huc12
      
      subset_state_huc12_tract_sf <- safe_aw_interpolate(subset_TractsOfInt, subset_hucsOfInt)
      subset_state_huc12_tract_sf$huc12 <- subset_hucsOfInt %>% subset(geometry %in% subset_state_huc12_tract_sf$geometry) %>% .$huc12
      
      #merge BG and tract sfs
      subset_state_huc12_bg_tract_sf <- data.frame(subset_state_huc12_bg_sf, subset_state_huc12_tract_sf) %>% 
                                        select(huc12, Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races, 
                                               Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore,
                                               Total_Pop, Not.Hispanic.Latino, Hispanic.Latino, geometry)
      
      # then add that subset to the output sf
      huc12_bg_tract_sf <- rbind(huc12_bg_tract_sf, subset_state_huc12_bg_tract_sf)
      
      # the print update
      print(paste0(round(v*100/length(intervals), 2), "% of state miniloops complete"))
      
    } #end looping through to subset state huc12s
    
    #Report out
    print(paste0("Alaska complete at ", now()))
  }
  
  summary(huc12_bg_tract_sf) 
  length(unique(huc12_bg_tract_sf$huc12))
  
# --- Examine and clean results of AWI, merge to huc12 metadata -----------------
  
  #boost
  gc()
  
  summary(huc12_bg_tract_sf) # a few duplicates because of hucs on state borders
  length(unique(huc12_bg_tract_sf$huc12)) # a few short because of non-US hucs. Both addressed below.
  
  # Examine missing hucs
  length(unique(huc12$huc12)) - length(unique(huc12_bg_tract_sf$huc12)) #41 missing.
  missedHUCSrows <- huc12 %>% filter(huc12 %notin% huc12_bg_tract_sf$huc12) # All missing or non-us States.
  mapview::mapview(missedHUCSrows)
  
  #Break out into reason for absence
    stateless <- missedHUCSrows %>% filter(states == " ")
    nonUS <- missedHUCSrows
    
    # Add state abbreviations to records missing a state value based on intersection with US.sf
    for(i in 1:nrow(stateless)) {
      
      statesForRow <- US.sf %>% filter(lengths(st_intersects(., stateless[i,])) > 0) %>% .$NAME
      stForRow <- state.abb[match(statesForRow, state.name)] %>% paste(collapse = ',')
      stateless[i, 'states'] <- stForRow
      
    }
    
    statelessHUCS <- stateless$huc12
    
    # Then run newly-stated records through the interpolation loop
    for(i in 1:length(statelessHUCS)) { 
      
      # filter to HUCs of interest
      hucOfInt <- stateless %>% select(huc12, states) %>% filter(huc12 == statelessHUCS[i])
      
      #get state(s) in which the HUC is located
      statesSel = hucOfInt$states
      if(is.na(statesSel)) {print("State NA. Skipping"); next} #there's a couple of these
      statesIncl <- unlist(strsplit(statesSel, ",")) %>% toupper() #catch any that may be lowercase ("co" at one point)
      statesIncl <- statesIncl %>% subset(statesIncl %notin% c("MEX", "CN", "CAN", "BC")) #remove non-US geographies 
      if(length(statesIncl) == 0) {print("No US states. Skipping"); next} #there's a couple of these, too 
      
      # abbreviation used in hucs, fips used in sites. Match. 
      fipsSel <- fips_codes %>% filter(state %in% statesIncl) %>% select(state_code) %>% unique()
      
      # filter to BGs and tracts of interest & get rid of non-numeric/non-extensive cols
      BGsOfInt <- bgGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
        select(Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races,
               Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore) %>%
        filter(lengths(st_intersects(., hucOfInt)) > 0)
      
      if(nrow(BGsOfInt) == 0) {print("Intersects no block groups. Skipping"); next} #there's a couple of these, too 
      
      TractsOfInt <- tractGeoData %>% filter(STATEFP %in% fipsSel$state_code) %>% 
        select(Total_Pop, `Not Hispanic/Latino`, `Hispanic/Latino`) %>% 
        filter(lengths(st_intersects(., hucOfInt)) > 0)
      
      
      # Demographics within for huc record of interest
      rec_huc12_bg_sf <- safe_aw_interpolate(BGsOfInt, hucOfInt)
      rec_huc12_bg_sf$huc12 <- hucOfInt$huc12
      
      rec_huc12_tract_sf <- safe_aw_interpolate(TractsOfInt, hucOfInt)
      rec_huc12_tract_sf$huc12 <- hucOfInt$huc12
      
      #Merge bg and tract level data into single sf
      rec_huc12_bg_tract_sf <- data.frame(rec_huc12_bg_sf, rec_huc12_tract_sf) %>% 
                               select(huc12, Total_Population, White, Black, Native, Asian, Pacific_Islander, Other, Multiple_Races, 
                                      Total_HH, d0to24k, d25to49k, d50to74k, d75to99k, d100to124k, d125to150k, d150kmore,
                                      Total_Pop, Not.Hispanic.Latino, Hispanic.Latino, geometry)
                              
      huc12_bg_tract_sf <- rbind(huc12_bg_tract_sf, rec_huc12_bg_tract_sf)
      print(paste0(round(i*100/length(statelessHUCS), 2), "% complete"))
      
    } 
    
    summary(huc12_bg_tract_sf) # a few duplicates because of hucs on state borders
    length(unique(huc12_bg_tract_sf$huc12)) # a few (5) short because of non-US hucs. Omitting. So 94572 total 
    
# Retain only distinct records - some belonging to multiple states were duplicated
  
    #round to whole numbers
    huc12demo <- huc12_bg_tract_sf %>% mutate_if(is.numeric, round, digits = 0)
    
    huc6 <- substr(huc12demo$huc12, 1, 6) %>% unique()  #just a category for looping
    
    # first one outside loop to have an sf onto which to bind
    huc6Sel = huc6[1]
    huc12demo_subset <- huc12demo %>% filter(substr(huc12, 1, 6) == huc6Sel)
    huc12demosubset_distinct <- huc12demo_subset %>% distinct()
    huc12demo_distinct <- huc12demosubset_distinct
    
    #Loop through state combos, get only distinct rows (huc12s) and save back out (loop to not crash)
    for(i in 2:length(huc6)) {
      huc6Sel = huc6[i]
      huc12demo_subset <- huc12demo %>% filter(substr(huc12, 1, 6) == huc6Sel)
      huc12demosubset_distinct <- huc12demo_subset %>% distinct()
      
      # Handle doubled hucs where one rec is NA. Basically, drop NAs for hucs with duplicate records, without dropping NAs that are real. 
      huc12demosubset_distinct <- huc12demosubset_distinct %>% group_by(huc12) %>% 
                                  arrange(Total_Population) %>% #arbitrary. Just sorting for numbers to precede NA. 
                                  summarise(Total_Population = first(Total_Population),
                                             White = first(White),
                                             Black = first(Black), 
                                             Native = first(Native),
                                             Asian = first(Asian), 
                                             Pacific_Islander = first(Pacific_Islander),
                                             Other = first(Other),
                                             Multiple_Races = first(Multiple_Races),
                                             Total_HH = first(Total_HH),
                                             d0to24k = first(d0to24k),
                                             d25to49k = first(d25to49k),
                                             d50to74k = first(d50to74k),
                                             d75to99k = first(d75to99k),
                                             d100to124k = first(d100to124k),
                                             d125to150k = first(d125to150k),
                                             d150kmore = first(d150kmore),
                                             Total_Pop = first(Total_Pop),
                                             Not.Hispanic.Latino = first(Not.Hispanic.Latino),
                                             Hispanic.Latino = first(Hispanic.Latino))
      
      #Rejoin the geoms lost above using group_by
      hucgeoms <- huc12demo_subset %>% select(huc12, geometry)
      huc12demosubset_distinct<- merge(x = huc12demosubset_distinct, y = hucgeoms, by = "huc12", all.x = TRUE)

      huc12demo_distinct <- rbind(huc12demo_distinct, huc12demosubset_distinct)
      print(paste0(round(i*100/length(huc6), 2), "% complete"))
    }
    
    summary(huc12demo_distinct) 
    
    nrow(huc12demo_distinct)
    length(unique(huc12demo_distinct$huc12))
    replicatedHUCS <- huc12demo_distinct %>% filter(duplicated(huc12demo_distinct$huc12)) %>% .$huc12
    check <- huc12demo_distinct %>% filter(huc12 %in% replicatedHUCS) 
    check.fix <- check %>% distinct() #unsure why distinct above didn't handle these...Fix it here
    
    good.sf <- huc12demo_distinct %>% filter(huc12 %notin% replicatedHUCS)
    huc12.demographics <- rbind(good.sf, check.fix) %>% .[,1:20]  #94572. Yay! Drop geometry column
    
# --- Combine with the EPA-enriched metadata -----------------------------------
    
    huc12 <- merge(huc12_epa, huc12.demographics, by = "huc12", all.x = TRUE)

# --- Save out HUC12 metadata - Original with EPA & Demographics ---------------
  # Save HUC12 metadata as spatial object - Shapefile - for use in update script
     st_write(obj = st_cast(huc12, "MULTIPOLYGON"), dsn = paste0(mp_wd_data, "HUC12Metadata.shp"), 
              driver = "ESRI Shapefile", delete_dsn = TRUE) #this allows for overwriting 
    
  # Clean up
  rm(good.sf, check, check.fix, replicatedHUCS, huc12demo, huc12demo_distinct, huc12demo_subset, 
     huc12demosubset_distinct, hucgeoms, huc6, huc6sel, huc12_bg_tract_sf, rec_huc12_bg_tract_sf,
     rec_huc12_bg_sf, rec_huc12_tract_sf, fipsSel, nonUS, stateless, statesIncl, missedHUCSrows,
     stForRows, statelessHUCS, hucOfInt, TractsOfInt, BGsOfInt) #missed a few, but overall clear
  gc()  

########################## CALCULATE SITES IN HUCS #############################
  
# Total number of sites points (monitoring locations, not instances) within each huc12 &
# within 1 huc12 upstream
# --- Calculate # sites within (intersect) each HU -----------------------------
  
   #reload sites data as SF if needed (TEMP)
   sitesData <- read.csv(file = paste0(mp_wd_data, "WQPSiteData.csv"), header = TRUE) %>% 
                         filter(is.na(x_long) == FALSE)
   sitePoints <- st_as_sf(sitesData, coords = c("x_long", "y_lat"), crs = 4269)
   
   #Crashes when I try to do it all at once. SO, loop? 

   #first one outside of loop to create SF onto which to bind
     statesSel = statesList[1]
     statesIncl <- unlist(strsplit(statesSel, ","))
     
     # abbreviation used in hucs, fips used in sites. Match. 
     fipsSel <- fips_codes %>% filter(state %in% statesIncl) %>% select(state_code) %>% unique()
     
     # filter to HUCs of interest
     hucsOfInt <- huc12 %>% filter(states == statesSel) %>% select(huc12, tohucs)
     
     # filter to monitoring sites of interest
     sitesOfInt <- sitePoints %>% mutate(StateCode = str_pad(StateCode, width = 2, side = "left", pad = "0")) %>%
                                  filter(StateCode %in% fipsSel$state_code)
     
     # sites within for hucs and sites of interest
     # hucsOfInt <- hucsOfInt[s2_is_valid(hucsOfInt) == TRUE] #checks if spherical geometry is valid: Points with invalid spherical geometry will break intersection analysis. More here: https://cran.r-project.org/web/packages/sf/vignettes/sf7.html 
     hucsOfInt$siteswithin <- lengths(st_intersects(hucsOfInt, sitesOfInt)) #count of the number of monitoring sites within the huc12 polygon
     summary(hucsOfInt$siteswithin)
   
     huc12sites <- hucsOfInt

   #Loop through state combos and calculate sites within each HUC12
   for(i in 2:length(statesList)) {
     
     statesSel = statesList[i]
     statesIncl <- unlist(strsplit(statesSel, ","))
     
     # abbreviation used in hucs, fips used in sites. Match. 
     fipsSel <- fips_codes %>% filter(state %in% statesIncl) %>% select(state_code) %>% unique()
     
     # filter to HUCs of interest
     hucsOfInt <- huc12 %>% filter(states == statesSel) %>% select(huc12, tohucs)
     
     # filter to monitoring sites of interest
     sitesOfInt <- sitePoints %>% mutate(StateCode = str_pad(StateCode, width = 2, side = "left", pad = "0")) %>%
                   filter(StateCode %in% fipsSel$state_code)
     
     # sites within for hucs and sites of interest
     #hucsOfInt <- hucsOfInt[s2_is_valid(hucsOfInt) == TRUE] #checks if spherical geometry is valid: Points with invalid spherical geometry will break intersection analysis. More here: https://cran.r-project.org/web/packages/sf/vignettes/sf7.html 
     hucsOfInt$siteswithin <- lengths(st_intersects(hucsOfInt, sitesOfInt)) #count of the number of monitoring sites within the huc12 polygon

     huc12sites <- rbind(huc12sites, hucsOfInt)
     print(paste0(round(i*100/length(statesList), 2), "% complete"))
     
   }
     
  summary(huc12sites) 
  
# --- Calculate upstream (attributed) sites ------------------------------------

  # Add a 'fromhuc' column (derive from 'tohuc')
  # Logic: if 'huc12' for a given row is in the 'tohuc' column of another row, 
  # pull the huc12 from the second row into the 'fromhuc' column of the first row
  
  #THIS WAS MOVED TO EARLIER BEFORE RECORDS WERE CONSOLIDATED!
    # to <- huc12sites %>% select(huc12, tohucs) %>% st_drop_geometry() %>% #need to have only one geometric object for merge
    #                      mutate(tohucs = ifelse(huc12 == "040203000200", "LAKE SUPERIOR", tohucs)) #manual fix for Isle Royal - lacks 'tohuc' value
    

  huc12sites_to <- merge(x = huc12sites, y = to, by.x = "huc12", by.y = "tohuc", all.x = TRUE) %>% 
                   rename(fromhuc = huc12.y) %>% st_drop_geometry() #pull huc12s for fromhuc column! And it can just be a df - we'll merge back onto sf later. 
  
    #For hucs with no upstream huc, skip the loop and do it as a batch (faster):
    noUp_huc12 <- huc12sites_to %>% filter(is.na(fromhuc == TRUE)) %>%
                                    mutate(sitesupstream = 0, sitesattributed = siteswithin) %>%
                                    select(huc12, siteswithin, sitesupstream, sitesattributed)
    summary(noUp_huc12)
    
    #For hucs with upstream hucs, use merge to get sites from hucs where the huc of interest is in the 'tohuc' col
    Up_huc12 <- huc12sites_to %>% filter(huc12 %notin% noUp_huc12$huc12) 
    
    to_from <- Up_huc12 %>% select(huc12, siteswithin) %>% merge(y = to, by = "huc12", all.x = TRUE)
    
    what_to_where <- to_from %>% group_by(huc12, tohuc) %>% 
                                 summarise(siteswithin = first(siteswithin), .groups = "drop") %>% #get only distinct records
                                 select(siteswithin, tohuc)
    
    # Match upstreams hucs to destination hucs, calc sites attributable to each huc
    Up_huc12 <- merge(x = Up_huc12, y = what_to_where, by.x = "huc12", by.y = "tohuc", all.x = TRUE) %>% #match destination huc to huc12 IDs to get upstream sites
                      rename(siteswithin = siteswithin.x, sitesupstream = siteswithin.y) %>%
                      group_by(huc12, fromhuc) %>% summarise(siteswithin = mean(siteswithin), sitesupstream = mean(sitesupstream), .groups = "drop") %>% #all distinct to/from pairs - something funky with the mean here 
                      group_by(huc12) %>% summarise(siteswithin = mean(siteswithin), sitesupstream = sum(sitesupstream), .groups = "drop") %>% #all huc12s with upstream hucs
                      mutate(sitesattributed = ifelse(is.na(sitesupstream), siteswithin, siteswithin + sitesupstream))  %>%
                      select(huc12, siteswithin, sitesupstream, sitesattributed)

    summary(Up_huc12)
    
    # Merge dataframes back together            
    huc12_sitesattributed <- rbind(noUp_huc12, Up_huc12) #bind the split analyses back together
    summary(huc12_sitesattributed)
    huc12_sitesattributed$siteswithin %>% sum() #somehow some are double counted? Unsure about that 
    
    #write.csv(huc12_sitesattributed, file = paste0(mp_wd_data, "HUC12SiteAttribution.csv"), row.names = FALSE)
    
#Clean up
    gc()
    rm(Up_huc12, to_from, what_to_where, noUp_huc12, huc12sites_to)
    
# --- Merge site attibutions to huc12 metadata and save out --------------------
    
    huc12 <- merge(huc12, huc12_sitesattributed, by = "huc12", all.x = TRUE)
    
    st_write(obj = st_cast(huc12, "MULTIPOLYGON"), dsn = paste0(mp_wd_data, "HUC12AllData.shp"), 
             driver = "ESRI Shapefile", delete_dsn = TRUE) #this allows for overwriting 
    
    
    
    
    
    
    
    