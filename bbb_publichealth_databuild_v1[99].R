#================================================ #
#
#  Public Health Modeling for MA Build Back Better
#  By: Naomi Shimberg
#  Start date: 2021-02-17
#=============================================== #

# ====== 1. SETUP =====
#Read in some libraries. I load the same ones every time, and very likely won't use all of them. But useful to have them all here to start.
library(dplyr)
library(readr)
library(Hmisc)
library(ggplot2)
library(data.table)
library(dplyr)
library(dtplyr)
library(lubridate)
library(binsreg)
library(tidyquant)
library(zoo)
library(stargazer)
library(lfe) 
require(gridExtra)
library(viridis)
library(readtext)
library(stringr)
library(xtable)
library(haven)
library(AER)
library(devtools) 
library(leebounds)
library(lubridate)
library(binsreg)
library(tidyquant)
library(zoo)
library(RColorBrewer)
library(lfe) 
library(readtext)
library(sandwich)
library(lmtest)
library(estimatr)
library(dotwhisker)
library(broom)
library(cowplot)
library(ggpubr)
library(robust)

#Function to remove na values
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

#Clear R
rm(list = ls())

#Set working directory to our google drive folder
setwd("~/Google Drive/My Drive/Build Back Better/Massachusetts Report/Phase 1: Jobs and Public Health/1. Data Analysis/Public Health Modeling")

#READS IN COUNTY LEVEL CACES DATA FOR GROUND LEVEL AND ELEVATED POINT SOURCES CONTAINING DAMAGE BY COUNTY BY POLLUTANT

#Read in CACES data
c_ground <- fread("~/Google Drive/My Drive/Build Back Better/Massachusetts Report/Phase 1: Jobs and Public Health/1. Data Analysis/Public Health Modeling/Raw Data/CACES_county_groundlevel.csv") %>%
  data.table()
c_elevated <- fread("~/Google Drive/My Drive/Build Back Better/Massachusetts Report/Phase 1: Jobs and Public Health/1. Data Analysis/Public Health Modeling/Raw Data/CACES_county_elevated.csv") %>%
  data.table()

#READS IN COUNTY LEVEL NEI DATA CONTAINING EMISSIONS BY COUNTY BY POLLUTANT BY SECTOR

#Read in NEI data
n <- fread("~/Google Drive/My Drive/Build Back Better/Massachusetts Report/Phase 1: Jobs and Public Health/1. Data Analysis/Public Health Modeling/Raw Data/NEI_county_2017.csv") %>%
  data.table()

#READS IN THE BRIDGE BETWEEN NEI AND CACES (ASK MORE ABOUT THIS) CONTAINS CO2e VALUES OF SECTORS

#Read in MA GHG Emissions Inventory data
g <- fread("~/Google Drive/My Drive/Build Back Better/Massachusetts Report/Phase 1: Jobs and Public Health/1. Data Analysis/Public Health Modeling/Raw Data/GHG-NEI-CACES_bridge.csv") %>%
  data.table()
g <- g[,.(ma_sector, MMT_CO2e)] #only use CO2e values

#READS IN MULTIPLIERS FOR STATE AND COUNTY LEVEL (ASK MORE ABOUT THIS)

#Read in state level and county multipliers
m_state <- fread("~/Google Drive/My Drive/Build Back Better/Massachusetts Report/Phase 1: Jobs and Public Health/1. Data Analysis/Public Health Modeling/Raw Data/multipliers_state.csv") %>%
  data.table()

m_county <- fread("~/Google Drive/My Drive/Build Back Better/Massachusetts Report/Phase 1: Jobs and Public Health/1. Data Analysis/Public Health Modeling/Raw Data/multipliers_county.csv") %>%
  data.table()

#===== 2. MERGE AND CLEAN CACES GROUND AND ELEVATED =====
#Merge CASES ground level and elevated. We have to download ground level and elevated in separate data sets, but we decide to use either ground level or elevated depending on the point source.

#SLAPS THE ELEVATED DATA ONTO THE BOTTOM OF THE GROUND DATA
#C IS NOW THE CACES DATA

#Vertical merge so we retain columns, but now have separate entries for ground and elevated
c <- rbind(c_ground, c_elevated)

#RENAMES CACES FIPS TO COUNTY

#Get rid of some extra variable and reorder to create clean CASES dataset
setnames(c, "fips", "county")

#REORDERS COLUMNS TO GET RID OF THE STATE ABBREVIATION

#Reorder columns
c <- c[, .(county,pollutant, model, season,elevated,damage)]

#USES ONLY ANNUAL CACES DATA

#Just use annual numbers
c <- c[season == "annual"]

# LOOPS THROUGH CACES DATA TO CREATE NEW DATA SET THAT CONTAINS AVERAGES BY COUNTY BY POLLUTANT OF DAMAGES BY MODEL

#Find average of three models to get central estimate for damages in each county
caces  <- matrix(0, nrow = 2*5*length(unique(c$county)), ncol = 6)
k <- 1
for (i in unique(c$county)){
     for (j in unique(c$pollutant)){
         for (m in unique(c$elevated)){
          caces[k, 1] <- i
          caces[k, 2] <- j
          caces[k, 3] <- "average"
          caces[k, 4] <- "annual"
          caces[k, 5] <- m
          caces[k, 6] <- mean(c$damage[c$pollutant == j & c$county == i & c$elevated == m]) #this is the code where I find the mean
        k <- k+1
  }
 }
}
 
#Convert to data.table for merging later and rename columns
caces <- as.data.table(caces)
names(caces) <- c("county", "pollutant", "model","season", "elevated","damage")

#convert damage to class numeric
caces[, damage := as.numeric(damage)]


# ==== 3. CLEAN NEI DATA  =====

#PADS COUNTY STRING WITH 0s AND TACKS STATE FIPS ONTO THE FRONT (SAME FORMAT AS CACES)

#Create county variable in NEI data
#append state + county (0XX) to get county variable to match CACES data for later merge
n[, length := str_length(COUNTY_FIPS)]
n[length ==2, fips := paste("0", COUNTY_FIPS, sep = "")]
n[length ==1, fips := paste("00", COUNTY_FIPS, sep = "")]
n[, county := paste0(STATE_FIPS, fips)]

#RENAMES NEI VARIABLES TO BE IN LINE WITH CACES VARIABLES

#Get rid of some extra variable, reorder, and rename to create clean NEI data set
setnames(n,"SECTOR","sector")
setnames(n,"POLLUTANT","pollutant")
setnames(n,"POLLUTANT TYPE","pollutant_type")
setnames(n,"EMISSIONS","emissions")
setnames(n,"UNIT OF MEASURE","unit")

#CONVERTS TO METRIC TONS TO MATCH CACES

#Convert emissions from short tons ton to metric tons to match CACES data
n[, emissions := emissions * 0.9071847]

#REMOVES LEAD

#Remove lead from data
n <- n[pollutant != "Lead"]

#GETS RID OF POLLUTANT TYPE AND UNIT

#Reorder columns
n <- n[,.(county, sector, pollutant, emissions)]

#RENAMES POLLUTANTS TO BE IN-LINE WITH CACES

#Rename pollutants for merge with CACES
n[pollutant == "Ammonia", pollutant := "nh3"]
n[pollutant == "Carbon Dioxide", pollutant := "co2"]
n[pollutant == "Carbon Monoxide", pollutant := "co"]

n[pollutant == "Volatile Organic Compounds", pollutant := "voc"]
n[pollutant == "PM10 Primary (Filt + Cond)", pollutant := "pm10"]

n[pollutant == "Nitrogen Oxides", pollutant := "nox"]
n[pollutant == "Nitrous Oxide", pollutant := "nitrous oxide"] 

n[pollutant == "Sulfur Dioxide", pollutant := "so2"]

n[pollutant == "Organic Carbon portion of PM2.5-PRI", pollutant := "organic carbon pm25"]
n[pollutant == "Elemental Carbon portion of PM2.5-PRI", pollutant := "elemental carbon pm25"]
n[pollutant == "PM2.5 Primary (Filt + Cond)", pollutant := "pm25"]

n[pollutant == "Methane", pollutant := "nh4"] #CONFUSED ABOUT METHANE (ASK ABOUT THIS)

#ASSIGNS SOURCES BASED ON SECTORS TO BE MORE BROAD AND IN-LINE WITH CACES

#Rename and reorganize sectors 

n[sector == "Fuel Comb - Electric Generation - Coal" | 
    sector == "Fuel Comb - Electric Generation - Natural Gas" |
    sector == "Fuel Comb - Electric Generation - Oil" |
    sector == "Fuel Comb - Electric Generation - Other",
  source := "Electricity"]

n[sector == "Fuel Comb - Residential - Natural Gas"| 
    sector == "Fuel Comb - Residential - Oil"| 
    sector == "Fuel Comb - Residential - Other", 
  source := "Residential"]

n[sector == "Fuel Comb - Comm/Institutional - Coal" | 
    sector == "Fuel Comb - Comm/Institutional - Natural Gas"| 
    sector == "Fuel Comb - Comm/Institutional - Oil"| 
    sector == "Fuel Comb - Comm/Institutional - Biomass"| 
    sector == "Fuel Comb - Comm/Institutional - Other",
  source := "Commercial/Industrial"]

#FILTERS OUT HIGH STACK INDUSTRIAL TO USE ELEVATED DAMAGES LATER

n[sector == "Fuel Comb - Industrial Boilers, ICEs - Coal" | 
    sector == "Fuel Comb - Industrial Boilers, ICEs - Natural Gas"| 
    sector == "Fuel Comb - Industrial Boilers, ICEs - Oil"| 
    sector == "Fuel Comb - Industrial Boilers, ICEs - Biomass",
  source := "Commercial/Industrial high stack"] #I separate out industrial boilers for now because we want elevated damages. I combine them with the other Comm/Industrial later.

n[sector == "Mobile - On-Road non-Diesel Heavy Duty Vehicles" | 
    sector == "Mobile - On-Road non-Diesel Light Duty Vehicles" | 
    sector == "Gas Stations" | 
    sector == "Bulk Gasoline Terminals"| 
    sector == "Mobile - On-Road Diesel Heavy Duty Vehicles"| 
    sector == "Mobile - On-Road Diesel Light Duty Vehicles"| 
    sector == "Mobile - Commercial Marine Vessels"| 
    sector == "Dust - Paved Road Dust"|
    sector == "Mobile - Locomotives",
  source := "Transportation"]

#FILTERS OUT HIGH STACK TRANSPORTATION TO USE ELEVATED DAMAGES LATER

n[sector == "Mobile - Aircraft", source := "Transportation high stack"] #I separate out aircrafts for now because we want elevated damages. I combine them with the other Comm/Industrial later.

n[sector == "Industrial Processes - Storage and Transfer",
  source := "Fossil Fuel Industry"]

n[sector == "Industrial Processes - Cement Manuf"| 
    sector =="Industrial Processes - Chemical Manuf"| 
    sector =="Industrial Processes - Ferrous Metals"| 
    sector =="Industrial Processes - NEC"| 
    sector =="Industrial Processes - Non-ferrous Metals"| 
    sector =="Industrial Processes - Petroleum Refineries"| 
    sector =="Industrial Processes - Pulp & Paper"| 
    sector =="Miscellaneous Non-Industrial NEC",
  source := "Industrial Processes"]

n[sector == "Agriculture - Livestock Waste" | sector == "Agriculture - Fertilizer Application", 
  source := "Agriculture"]

#REMOVES NA SOURCES

#Remove sectors besides those which we've decided to assign a source    
n <- n[!is.na(source)]

n[, length(unique(sector))]

# ==== 4. COMBINE CASES AND NEI TO GET THE TOTAL COST OF EACH POLLUTANT IN MA BY COUNTY =====

#MERGES NEI AND CACES DATA BY COUNTY AND POLLUTANT

#Merge CAES and NEI data by county and pollutant
nei_caces <- merge(n, caces[,.(county, pollutant, elevated, damage)], by = c("county", "pollutant"))

#SEPARATES OUT HIGH STACK AND GROUND LEVEL SOURCES

#Depending on source, only keep damages at high stack or ground level
groundlevel <- nei_caces[(source == "Agriculture" | source == "Residential" | source ==  "Commercial/Industrial" | source == "Fossil Fuel Industry" | source == "Transportation") 
                         & elevated == "ground level",]

highstack <- nei_caces[(source == "Industrial Processes" | source == "Electricity" | source == "Commercial/Industrial high stack" | source == "Transportation high stack") &
                         elevated == "high stack",]

#RECOMBINES THE GROUND LEVEL AND HIGH STACK SOURCES

#Recombine ground level and high stack
d <- rbind(groundlevel, highstack)

#RENAMES THE TWO HIGH STACK SOURCES TO NORMAL

#Rename the "Commercial/Industrial high stack" to be part of "Commercial/Industrial" post merge, same thing for "Transportation high stack"
d[source == "Commercial/Industrial high stack", source := "Commercial/Industrial"]
d[source == "Transportation high stack", source := "Transportation"]

#REORGANIZES THE COLUMNSel

#Rearrange columns
d <- d[,.(county,pollutant, source, sector, elevated, emissions, damage)]

#======Disperse "Other" categories to other sectors within source if not present in the MA GHG inventory=======
#Create prop table sectors, DO NOT include "Other" category in prop.table
electricity <- d[source == "Electricity" & sector != "Fuel Comb - Electric Generation - Other",
                 .("sector" = unique(sector), "perc" = prop.table(emissions)), by = .(county,pollutant)]
industrial <- d[source == "Commercial/Industrial" & elevated != "high stack" & sector != "Fuel Comb - Comm/Institutional - Other",
                .("sector" = unique(sector),"perc" = prop.table(emissions)), by = .(county,pollutant)]
residential <- d[source == "Residential"  & sector != "Fuel Comb - Residential - Other",
                 .("sector" = unique(sector), "perc" = prop.table(emissions)), by = .(county,pollutant)]

#Combine electricity, industrial, and residential proportion tables
source <- do.call("rbind", list(electricity, industrial, residential))

#Merge these proportions tables with emissions by sector by county by pollutant
dd <- merge(d,source, by = c("county", "sector","pollutant"), all.x = T)

#Assign emissions of "Other" categories to new variable, "emissions_other," to later disperse
for (i in unique(dd$pollutant)){
  for (j in unique(dd$county)){
  dd[source == "Commercial/Industrial" & pollutant == i & county == j & elevated != "high stack", emissions_other := dd$emissions[dd$sector == "Fuel Comb - Comm/Institutional - Other" & dd$pollutant == i & dd$county == j]]
  dd[source == "Residential" & pollutant == i & county == j, emissions_other := dd$emissions[dd$sector == "Fuel Comb - Residential - Other" & dd$pollutant == i & dd$county == j]]
  }
}

#only "Other" for Electric in certain counties, for certain pollutant types
electric_county <- c("25001","25005","25011","25017")
for (i in unique(dd$pollutant[dd$county == "25001" & dd$sector == "Fuel Comb - Electric Generation - Other"])){
  for (j in electric_county){
  dd[source == "Electricity" & pollutant == i & county == j, emissions_other := dd$emissions[dd$sector == "Fuel Comb - Electric Generation - Other" & dd$pollutant == i & dd$county == j]]
  }
}
electric_county1 <- c("25013","25027")
for (i in unique(dd$pollutant[dd$county == "25013" & dd$sector == "Fuel Comb - Electric Generation - Other"])){
  for (j in electric_county1){
    dd[source == "Electricity" & pollutant == i & county == j, emissions_other := dd$emissions[dd$sector == "Fuel Comb - Electric Generation - Other" & dd$pollutant == i & dd$county == j]]
  }
}

#Remove the "other" categories
dd <- dd[sector != "Fuel Comb - Electric Generation - Other",]
dd <- dd[sector != "Fuel Comb - Comm/Institutional - Other",]
dd <- dd[sector != "Fuel Comb - Residential - Other",]


#Disperse emissions to create emissions_new variable
dd[, emissions_new := emissions]
dd[!is.na(emissions_other), emissions_new := emissions + (emissions_other*perc)] #if had "Other" column to disperse


#Find total damages (in mt) by sector, county, and pollutant.
dd[, caces_damage := emissions_new * damage]

#Remove unnecessary column and reorder
dd <- dd[,.(county, sector, pollutant, source, emissions_new, caces_damage)]

##========5. MERGE NEI CO2 EMSSIONS TO GET $ DAMAGE CO-POLLUTANTS/MT CO2======
#Find total damages by sector and county.
damage <- dd[, .("caces_damage" = sum(caces_damage), "pm25_emissions" = sum(emissions_new[pollutant == "pm25"])), by = .(sector,county)]


#Create ma_sector, where row names match MA GHG inventory report
damage[sector == "Agriculture - Fertilizer Application", ma_sector := "Ag soils"]
damage[sector == "Agriculture - Livestock Waste", ma_sector := "Manure management"]

damage[sector == "Fuel Comb - Comm/Institutional - Biomass", ma_sector := "Commercial - Other"]
damage[sector == "Fuel Comb - Comm/Institutional - Coal", ma_sector := "Commercial - Coal"]
damage[sector == "Fuel Comb - Comm/Institutional - Natural Gas", ma_sector := "Commercial - Natural Gas"]
damage[sector == "Fuel Comb - Comm/Institutional - Oil", ma_sector := "Commercial - Petroleum"]

damage[sector == "Fuel Comb - Industrial Boilers, ICEs - Biomass", ma_sector := "Industrial - Other"]
damage[sector == "Fuel Comb - Industrial Boilers, ICEs - Coal", ma_sector := "Industrial - Coal"]
damage[sector == "Fuel Comb - Industrial Boilers, ICEs - Natural Gas", ma_sector := "Industrial - Natural Gas"]
damage[sector == "Fuel Comb - Industrial Boilers, ICEs - Oil", ma_sector := "Industiral - Petroleum"]

damage[sector == "Fuel Comb - Electric Generation - Coal", ma_sector := "Coal"]
damage[sector == "Fuel Comb - Electric Generation - Natural Gas", ma_sector := "Natural Gas"]
damage[sector == "Fuel Comb - Electric Generation - Oil", ma_sector := "Petroleum"]

damage[sector == "Fuel Comb - Residential - Natural Gas", ma_sector := "Residential Natural Gas"]
damage[sector == "Fuel Comb - Residential - Oil", ma_sector := "Residential Petroleum"]

damage[sector == "Mobile - Aircraft", ma_sector := "Aviation"]
damage[sector == "Mobile - Commercial Marine Vessels", ma_sector := "Boats"]
damage[sector == "Mobile - Locomotives", ma_sector := "Locomotives"]
damage[sector == "Mobile - On-Road Diesel Heavy Duty Vehicles", ma_sector := "Diesel Highway - Heavy Duty Vehicles"]
damage[sector == "Mobile - On-Road Diesel Light Duty Vehicles", ma_sector := "Diesel Highway - LDVs and Passenger Cars"]
damage[sector == "Mobile - On-Road non-Diesel Heavy Duty Vehicles", ma_sector := "Gasoline Highway - Heavy Duty Vehicles"]
damage[sector == "Mobile - On-Road non-Diesel Light Duty Vehicles" | sector == "Bulk Gasoline Terminals" | sector == "Gas Stations" | sector == "Dust - Paved Road Dust", 
       ma_sector := "Gasoline Highway - Passenger Cars, Light Duty Vehicles, and Motorcycles"]


damage[sector == "Industrial Processes - Cement Manuf" | 
         sector ==   "Industrial Processes - Chemical Manuf"| 
         sector ==  "Industrial Processes - Ferrous Metals"| 
         sector ==  "Industrial Processes - NEC"| 
         sector ==  "Industrial Processes - Non-ferrous Metals"| 
         sector ==  "Industrial Processes - Petroleum Refineries"| 
         sector ==  "Industrial Processes - Pulp & Paper"| 
         sector ==  "Industrial Processes - Storage and Transfer"| 
         sector ==  "Miscellaneous Non-Industrial NEC",
       ma_sector := "Industrial Processes"]


#New damage data set, only with the MA GHG inventory sectors
ma_damage <- damage[, .("caces_damage" = sum(caces_damage), "pm25_emissions" = sum(pm25_emissions)), by = .(ma_sector,county)]


##========6. STATE-LEVEL AGGREGATION ======
#Merge with MA CO2e emissions
county <- merge(ma_damage, g, by = "ma_sector")

#disburse g to county level
#Weights by county
county[, pm25_sector_sum := sum(pm25_emissions), by = .(ma_sector)]
county[, pm25_freq := pm25_emissions / pm25_sector_sum]
county[, MMT_CO2e_county := MMT_CO2e * pm25_freq]


#find damage per CO2e at the county level
county[, damage_per_CO2e := caces_damage / (MMT_CO2e_county*1000000)]


#re-aggregate to state-level
state <- county[, .("caces_damage" = sum(caces_damage), "MMT_CO2e" = sum(MMT_CO2e_county)), by = .(ma_sector)]
state[, damage_per_CO2e := caces_damage / (MMT_CO2e*1000000)]

fwrite(state, "state_multipliers.csv")



##========7. MERGE WITH MULTIPLIERS ======
#state level multipliers
state_multipliers <- merge(m_state, state, by = "ma_sector")
state_multipliers[, project_savings := CO2e_per_million_dollars * damage_per_CO2e] 

#reorder columns for flow of logic
state_multipliers <- state_multipliers[,.(project, ma_sector, CO2e_per_million_dollars,damage_per_CO2e, project_savings)]

#export spreadsheet
#fwrite(state_multipliers, "final_state_multipliers.csv")

#county level multipliers
m_county[, county := as.numeric(county)]
county[, county := as.numeric(county)]

county_multipliers <- merge(m_county[,.(project, ma_sector,county,CO2e_per_million_dollars)], county[,.(county, ma_sector, caces_damage, damage_per_CO2e)], by = c("ma_sector","county"))
county_multipliers[, project_savings := CO2e_per_million_dollars * damage_per_CO2e]
county_multipliers <- county_multipliers[,.(project, ma_sector, county, CO2e_per_million_dollars,damage_per_CO2e, project_savings)]

#export spreadsheet
#fwrite(county_multipliers, "final_county_multipliers.csv")

##========8. AGGREGATE BY PROJECT ======
state_projects <- state_multipliers[, .("project_savings" = sum(project_savings)), by = "project"]
county_projects <- county_multipliers[, .("project_savings" = sum(project_savings)), by = "project"]

savings <- rbind(state_projects, county_projects)

#export spreadsheet
#fwrite(savings, "final_project_savings.csv")




 

































