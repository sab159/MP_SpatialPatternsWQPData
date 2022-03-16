################################################################################
#
# Code to update WQP dataset, point-in-poly calc, and corresponding shapefiles
#               Developed by Sophia Bryson (sab159@duke.edu)
#               Masters Project for client Internet of Water
#                                 2021 - 2022
#
################################################################################




### SET UP #####################################################################
# --- Load datasets ------------------------------------------------------------

  #Sites (points)
  sitesData <- read.csv(file = paste0(mp_wd_data, "WQPSiteData.csv"), header = TRUE) %>% 
               filter(is.na(x_long) == FALSE)
  
  sitePoints <- st_as_sf(sitesData, coords = c("x_long", "y_lat"), crs = 4269)

  
  #HUCs (polygons)
  huc12 <- st_read(paste0(mp_wd_data, "HUC12AllData.shp")) %>% 
           select(huc12, name, states, EPAregn, aresqkm, tohucs, 
                  Ttl_Ppl, White, Black, Native, Asian, Pcfc_Is, Other, Mltpl_R,
                  Totl_HH, d0to24k, d25t49k, d50t74k, d75t99k, d100124, d125150, d150kmr,
                  Totl_Pp, Nt_Hs_L, Hspnc_L)
  
  
# --- Calibrate to last update -------------------------------------------------
  
  #Needs to be in MM-DD-YYYY for WQP pull 
  last.date <- max(sitesData$MostRecentDate) %>% as.Date('%Y-%m-%d') %>% format('%m-%d-%Y') 

### UPDATE POINT LAYER #########################################################
# --- Pull metadata for new sites ----------------------------------------------
  
  old.sites <- sitesData
  
  # New metadata since last update
  states.1 <- whatWQPsites(statecode = statecodes[1:10], startDateLo = last.date, startDateHi = endDate) 
  states.2 <- whatWQPsites(statecode = statecodes[11:20], startDateLo = last.date, startDateHi = endDate) 
  states.3 <- whatWQPsites(statecode = statecodes[21:30], startDateLo = last.date, startDateHi = endDate) 
  states.4 <- whatWQPsites(statecode = statecodes[31:40], startDateLo = last.date, startDateHi = endDate) 
  states.5 <- whatWQPsites(statecode = statecodes[41:51], startDateLo = last.date, startDateHi = endDate) 
  
  all.sites <- rbind(states.1, states.2, states.3, states.4, states.5) #had problems parsing, but it's all here. 
  
  # Pare down, filter, reproject metadata
  filtered.sites <- all.sites %>% select(OrganizationIdentifier, OrganizationFormalName, ProviderName, #organization metadata - organization vs. provider? 
                                         MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, #site metadata - left out description
                                         CountryCode, StateCode, CountyCode, HUCEightDigitCode, AquiferName, #hydro and political boundaries
                                         LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName) #location data 
  
  datums.keep <- list("NAD27", "NAD83", "WGS84", "UNKWN", "Unknown")
  filtered.sites <- filtered.sites %>% filter(HorizontalCoordinateReferenceSystemDatumName %in% datums.keep)
  
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
  reprojected.sites <- rbind(NAD83.sites, NAD27toNAD83, WGS84toNAD83, UNKWN.sites) %>% 
                       rename(InitHorCoorRefSysDatumName = HorizontalCoordinateReferenceSystemDatumName)
  
  #split geometry into lat and long
  reprojected.sites <- reprojected.sites %>% mutate(x_long = unlist(map(reprojected.sites$geometry, 1)), 
                                                    y_lat = unlist(map(reprojected.sites$geometry, 2))) 
  reprojected.sites <- st_drop_geometry(reprojected.sites) #drop geometry for saveout
  
# --- Combine old and new sites metadata ---------------------------------------
  
  new.metadata <- reprojected.sites
  old.metadata <- old.sites %>% select(names(reprojected.sites))
  reprojected.sites <- rbind(old.metadata, new.metadata) #updated metadata for use in following loop
  
# --- Pull monitoring data -----------------------------------------------------
  
  # Rerun the loop for all site ids in that list because parameters were not retained
  
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
    # created manually from earlier pull. May be incomplete and need occasional manual update for parameters not included in the inital dataset on which the classification was made. 
    
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
      
      # check if it's missing metadata and pull if so to prevent NAs further down
      missing.metadata <- siteData %>% filter(MonitoringLocationIdentifier %notin% reprojected.sites$MonitoringLocationIdentifier)
      
      if(nrow(missing.metadata > 0)) { #Only run if any selected sites are missing metadata
        
        missed.metadata <- whatWQPsites(siteid = missing.metadata$MonitoringLocationIdentifier)
        
          filtered.sites <- missed.metadata %>% select(OrganizationIdentifier, OrganizationFormalName, ProviderName, #organization metadata - organization vs. provider? 
                                                 MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, #site metadata - left out description
                                                 CountryCode, StateCode, CountyCode, HUCEightDigitCode, AquiferName, #hydro and political boundaries
                                                 LatitudeMeasure, LongitudeMeasure, HorizontalCoordinateReferenceSystemDatumName) #location data 
          
          datums.keep <- list("NAD27", "NAD83", "WGS84", "UNKWN", "Unknown")
          filtered.sites <- filtered.sites %>% filter(HorizontalCoordinateReferenceSystemDatumName %in% datums.keep)
          
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
          
          #Unknowns have been assumed to be NAD83. This may not be true, but they don't require transformation here. 
          
          # Merge reprojected spatial datasets
          reprojected.new.sites <- rbind(NAD83.sites, NAD27toNAD83, WGS84toNAD83, UNKWN.sites) %>% 
            rename(InitHorCoorRefSysDatumName = HorizontalCoordinateReferenceSystemDatumName)
          
          #split geometry into lat and long
          reprojected.new.sites <- reprojected.new.sites %>% mutate(x_long = unlist(map(reprojected.new.sites$geometry, 1)), 
                                                                    y_lat = unlist(map(reprojected.new.sites$geometry, 2))) %>% 
                                                             st_drop_geometry()
          
        #Merge all metadata
        reprojected.sites <- rbind(reprojected.new.sites, reprojected.sites)
      }
      
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
      #somehow there are NA lat and longs introduced here - some sites for which there is not metadata.... 
      
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
    
    #Despite catch, some still coming back NA:
    all.good <- sitesData %>% filter(!is.na(x_long))
    still.na <- sitesData %>% filter(is.na(x_long)) 
    table(still.na$InitHorCoorRefSysDatumName) #Ahh. Lacking a CRS. Omit. 
    sitesData <- all.good

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
    # #Updated site (meta)data - FURTHER DOWN
    # write.csv(sitesData, file = paste0(mp_wd_data, "WQPSiteData.csv"), row.names = FALSE)
    
    #Export provider data for manual classification:
    write.csv(providerList, file = paste0(mp_wd_data, "ProviderList.csv"), row.names = FALSE) #save out for Blair for manual classification
    
    #Save out measured parameters for manual classification (then rerun the loop above if needed (first time))
    write.csv(parameterList, file = paste0(mp_wd_data, "ParameterPrevalence.csv")) #save out to manually classify
  
# --- Classify site types (sw, gw) ---------------------------------------------
  #create sub-groups
  gw <- c("Aggregate groundwater use", "Borehole", "Cave", "Other-Ground Water", "Subsurface",
          "Subsurface: Cave", "Subsurface: Groundwater drain", "Subsurface: Tunnel, shaft, or mine",
          "Subsurface: Unsaturated zone", "Well", "Well: Collector or Ranney type well", "Well: Extensometer well",
          "Well: Hyporheic-zone well", "Well: Interconnected wells", "Well: Multiple wells",
          "Well: Test hole not completed as a well")
  streams <- c("BEACH Program Site-Channelized stream", "BEACH Program Site-River/Stream", "Channelized Stream",
               "River/Stream", "River/stream Effluent-Dominated", "River/Stream Ephemeral", "River/Stream Intermittent",
               "River/Stream Perennial", "Stream", "Stream: Canal", "Stream: Ditch", "Stream: Tidal stream" ) 
  canals <- c("Canal Drainage", "Canal Irrigation", "Canal Transport")
  wetlands <- c("Constructed Wetland", "Wetland", "Wetland Estuarine-Emergent", "Wetland Estuarine-Forested",
                "Wetland Estuarine-Marsh", "Wetland Estuarine-Pool", "Wetland Estuarine-Scrub-Shrub", 
                "Wetland Lacustrine-Emergent", "Wetland Palustrine-Emergent", "Wetland Palustrine-Forested",
                "Wetland Palustrine-Moss-Lichen", "Wetland Palustrine-Shrub-Scrub", "Wetland Palustrine Pond",
                "Wetland Riverine-Emergent", "Wetland Undifferentiated")
  lakes <-   c("BEACH Program Site-Great Lake", "BEACH Program Site-Lake", "Great Lake", "Lake", 
               "Lake, Reservoir, Impoundment", "Pond-Stock", "Pond-Stormwater", "Pond-Wastewater",
               "Reservoir", "Riverine Impoundment") #includes reservoirs  
  ponds <- c("Pond", "Pond-Anchialine", "Pond-Sediment")
  seeps_springs <- c("Seep", "Spring")             
  coastal_marine <- c("BEACH Program Site-Estuary", "BEACH Program Site-Ocean", "Estuary", "Ocean",
                      "Ocean: Coastal", "Playa")
  sw <- c("Aggregate surface-water-use", "Other-Surface Water")
  other <- c("BEACH Program Site-Land runoff", "BEACH Program Site-Storm sewer", "BEACH Program Site-Waste sewer",
             "CERCLA Superfund Site", "Combined Sewer", "Constructed Diversion Dam", "Constructed Tunnel",
             "Constructed Water Transport Structure", "Facility Industrial", "Facility Municipal Sewage (POTW)",
             "Facility Other", "Facility Privately Owned Non-industrial", "Facility Public Water Supply (PWS)", 
             "Facility: Cistern", "Facility: Combined sewer", "Facility: Diversion", "Facility: Field, Pasture, Orchard, or Nursery",
             "Facility: Golf course", "Facility: Laboratory or sample-preparation area", "Facility: Landfill",
             "Facility: Outfall", "Facility: Pavement", "Facility: Septic system", "Facility: Storm sewer",
             "Facility: Waste injection well", "Facility: Wastewater land application", "Facility: Wastewater sewer",
             "Facility: Water-distribution system", "Facility: Water-use establishment", "Floodwater", "Floodwater non-Urban",
             "Floodwater Urban", "Land Flood Plain", "Land Runoff", "Leachate-Lysimeter", "Leachate-SamplePoint", "Mine/Mine Discharge",
             "Pipe, Unspecified Source", "Spigot / Faucet", "Storm Sewer", "Survey Monument","Test Pit", "Waste Pit",  
             "Waste Sewer") 
  nonwater <- c("Atmosphere", "BEACH Program Site-Land", "Gallery", "Gas-Monitoring Probe", "Glacier", "Land",
                "Land: Excavation", "Land: Outcrop", "Land: Playa", "Land: Shore", "Land: Sinkhole",
                "Land: Soil hole", "Land: Volcanic vent", "Landfill", "Local Air Monitoring Station", "Mine Pit",
                "Mine/Mine Discharge Adit (Mine Entrance)", "Mine/Mine Discharge Tailings Pile", "Mine/Mine Discharge Waste Rock Pile",
                "State/Local Air Monitoring Station")#glaciers, land, air, etc. DROP THESE??? 
  
  #create overarching group
  gw <- gw
  sw <- c(sw, streams, canals, wetlands, lakes, ponds, seeps_springs, coastal_marine)
  other <- c(other, nonwater) #not dropping non-water because users can decide how to consider/include those
  
  sitesData <- sitesData %>% mutate(MonitoringLocTypeClass = case_when(MonitoringLocationTypeName %in% gw ~ "Groundwater",
                                                                     MonitoringLocationTypeName %in% sw ~ "Surface water",
                                                                     MonitoringLocationTypeName %in% other ~ "Other",
                                                                     is.na(MonitoringLocationTypeName) ~ "Not reported", 
                                                                     MonitoringLocationTypeName %notin% c(gw, sw, other) ~ "Not yet classified"))

# --- Add states as names ------------------------------------------------------
  
  #match state abbreviations and names
  state_match = fips_codes %>% select(state_code, state, state_name) %>% distinct()
  sitesData <- sitesData %>% mutate(StateCode = str_pad(as.character(StateCode), width = 2, side = "left", pad = "0")) %>%
                             merge(y = state_match, by.x = "StateCode", by.y = "state_code", all.x = TRUE) %>%
                             rename(StateAbb = state, StateName = state_name)
  
# --- Add EPA region to points -------------------------------------------------
  
  #Fetch EPA Regions shapefile from ESRI REST endpoint   
  endpointURL <- "https://services2.arcgis.com/FiaPA4ga0iQKduv3/arcgis/rest/services/EPA_Regions/FeatureServer/0"
  epa.regions <- get_spatial_layer(endpointURL,
                                   out_fields = c("FID", "OBJECTID", "EPAREGION"),
                                   where = "1=1") #fetch all records
  epa.regions <- epa.regions %>% st_transform(crs = st_crs(huc12)) %>% rename(geometry = geoms)
  
  # Re-SF-ify sites data for use in geospatial analyses
  
  sitePoints <- sitesData %>% filter(!is.na(x_long)) %>% st_as_sf(coords = c("x_long", "y_lat"), crs = 4269)
  
  # Loop through EPA regions and identify sites within each
  
  regionList <- epa.regions$EPAREGION
    
    #First one outside loop to have an sf onto which to bind
    regionSel = regionList[1]
    region_subset <- epa.regions %>% filter(EPAREGION == regionSel) #one region
    sites_subset <- sitePoints %>% st_filter(region_subset) #select points within that region
    sites_subset$EPARegion <- regionSel
    sitesUpdated <- sites_subset
  
    # Loop through regions 
    for(i in 2:length(regionList)) {
      regionSel = regionList[i]
      region_subset <- epa.regions %>% filter(EPAREGION == regionSel) #one region
      sites_subset <- sitePoints %>% st_filter(region_subset) #select points within that region
      sites_subset$EPARegion <- regionSel
      sitesUpdated <- rbind(sitesUpdated, sites_subset)
    }
    
    summary(sitesUpdated)
    
    #some points not in any EPA region due to location: 
    sitePoints %>% filter(MonitoringLocationIdentifier %notin% sitesUpdated$MonitoringLocationIdentifier) %>% mapview()
    tack.on <- sitePoints %>% filter(MonitoringLocationIdentifier %notin% sitesUpdated$MonitoringLocationIdentifier)
    tack.on$EPARegion <- NA
    sitesData <- rbind(sitesUpdated, tack.on)
    
# --- Save back out ------------------------------------------------------------
    
  # Reconvert to lat and long cols
    sitesData <- sitesData %>% mutate(x_long = unlist(map(sitesData$geometry, 1)), 
                                      y_lat = unlist(map(sitesData$geometry, 2))) %>%
                               st_drop_geometry()  
    
  #Updated site (meta)data 
  write.csv(sitesData, file = paste0(mp_wd_data, "WQPSiteData.csv"), row.names = FALSE)
  # Site Metadata is contained in All Data. No need to duplicate.
  
### UPDATE POLYGON LAYER #######################################################
# --- Strip data off of metadata -----------------------------------------------
  
  #Handled above
  
  old.hucs <- huc12

# --- Recalculate sites in hucs ------------------------------------------------
  
sitePoints <- st_as_sf(sitesData, coords = c("x_long", "y_lat"), crs = 4269)
  
  #Crashes when I try to do it all at once. SO, loop? 
  
  #first one outside of loop to create SF onto which to bind
  statesList <- unique(huc12$states) %>% subset(!is.na(statesList) & statesList %notin% c("MEX", "BC"))
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
    hucsOfInt$siteswithin <- lengths(st_intersects(hucsOfInt, sitesOfInt)) #count of the number of monitoring sites within the huc12 polygon
    
    huc12sites <- rbind(huc12sites, hucsOfInt)
    print(paste0(round(i*100/length(statesList), 2), "% complete"))
    
  }
  
  summary(huc12sites) 
  
# --- Realculate upstream (attributed) sites ------------------------------------
  
  # Add a 'fromhuc' column (derive from 'tohuc')
  # Logic: if 'huc12' for a given row is in the 'tohuc' column of another row, 
  # pull the huc12 from the second row into the 'fromhuc' column of the first row
  
  #load up/downstream match file created in initial script: 
  to <- read.csv(paste0(mp_wd_data, "ToHUC.csv"))
  
  huc12sites_to <- merge(x = huc12sites, y = to, by.x = "huc12", by.y = "tohuc", all.x = TRUE) %>% 
                   rename(fromhuc = huc12.y) %>% st_drop_geometry() #pull huc12s for fromhuc column! And it can just be a df - we'll merge back onto sf later. 
  
  #For hucs with no upstream huc, skip the loop and do it as a batch (faster):
  noUp_huc12 <- huc12sites_to %>% filter(is.na(fromhuc == TRUE)) %>%
                                  mutate(sitesupstream = 0, 
                                         sitesattributed = ifelse(is.na(siteswithin), 0, siteswithin)) %>%
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
  
  
# --- Merge site attibutions to huc12 metadata ---------------------------------
  
  huc12 <- merge(huc12, huc12_sitesattributed, by = "huc12", all.x = TRUE)
  
# --- Classify as monitored vs. unmonitored ------------------------------------
    
  #Add in monitoring classification: 
  huc12 <- huc12 %>% mutate(monitored = case_when(sitesattributed > 0 ~ "monitored",
                                                  sitesattributed == 0 ~ "unmonitored"))
  
# --- Add states as names ------------------------------------------------------
  
  # Single state hucs are straightforward:
  singlestate <- huc12 %>% filter(str_length(states) == 2)
    
    #match state abbreviations and names
    singlestate <- singlestate %>% merge(y = state_match, by.x = "states", by.y = "state", all.x = TRUE) %>%
                                   mutate(StateAbb = states) %>% 
                                   rename(StateName = state_name) %>% 
                                   select(-state_code)
    
    singlestate <- singlestate %>% mutate(StateCode = str_pad(as.character(StateCode), width = 2, side = "left", pad = "0")) %>%
      merge(y = state_match, by.x = "StateCode", by.y = "state_code", all.x = TRUE) %>%
      rename(StateAbb = state, StateName = state_name)
  
  
  # There are some hucs (5000) that intersect multiple states... Handle these separately. 
  # Classifying as "multistate". Alternative future options include creating multiple records or assigning to state with largest overlap
  multistate <- huc12 %>% filter(str_length(states) > 2)
    
    multistate$StateAbb <- "MULT"
    multistate$StateName <- "Multiple States"
  
  
  # And what oddballs are left? States are NA. Abbreviations and names will be likewise. 
  others <- huc12 %>% filter(huc12 %notin% c(multistate$huc12, singlestate$huc12)) #some sites have NA states... 
  
    others$StateAbb <- "NA"
    others$StateName <- "NA"
  
  huc12 <- rbind(singlestate, multistate, others)  
  
# --- Save back out ------------------------------------------------------------
  # Shapefile
  
  st_write(obj = st_cast(huc12, "MULTIPOLYGON"), dsn = paste0(mp_wd_data, "HUC12AllData.shp"), 
           driver = "ESRI Shapefile", delete_dsn = TRUE) #this allows for overwriting 
  