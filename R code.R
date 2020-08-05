## R code for the supplementary document, COVID-19 study

#Download the Zipcode-level COVID-19 data from the JSON of Chicago Reporter
dataraw<-fromJSON("https://covid-data.chicagoreporter.com/zip-archive/il-zip-covid-historical-20200617.json")

#Download age-specific population data from the American Community Survey (ACS) 2014-2018
library(tidycensus)
library(tidyverse)
library(dplyr)

census_api_key("------",install = TRUE,overwrite=TRUE)
#Example: Age < 20
zip20 <- get_acs(geography = "zcta",
                 variables = c(total="B01001_001E",
                               total_m="B01001_002E",
                               male5 = "B01001_003E", 
                               male5_9="B01001_004E",
                               male10_14="B01001_005E",
                               male15_17="B01001_006E",
                               male18_19="B01001_007E",
                               total_f="B01001_026E",
                               female5="B01001_027E",
                               female5_9="B01001_028E",
                               female10_14="B01001_029E",
                               female15_17="B01001_030E",
                               female18_19="B01001_031E"),
                 year = 2018)

## Select zipcode for IL
zip20_IL <- subset(zip20,zip20$GEOID>60000 & zip20$GEOID<63000)

zip20_calculate <- subset(zip20_IL, zip20_IL$variable %in% c("B01001_003","B01001_004","B01001_005","B01001_006",
                                                             "B01001_007","B01001_027","B01001_028","B01001_029",
                                                             "B01001_030","B01001_031"))
#zip20_sum is the population of age <20 in each zip code area of IL
zip20_sum <- zip20_calculate %>%
  group_by(GEOID) %>%
  summarise(age20 = sum(estimate,na.rm=T))

#####Caculate the age-specific incidence of COVID-19 (cases/1000 people)
POP <- read.csv("/Age.csv",header=T,as.is = T)

IDPH <- read.csv("/IDPH_May25_Jun17.csv",header=T,as.is = T)

colnames(POP)[colnames(POP)=="GEOID"] <- "zip"

IDPH2 <- merge(IDPH,POP,by="zip",x.all=T)

#Comfirm rate
IDPH2$age1_c_r <- 1000*IDPH2$age1/IDPH2$age20
IDPH2$age2_c_r <- 1000*IDPH2$age2/IDPH2$age20_29
IDPH2$age3_c_r <- 1000*IDPH2$age3/IDPH2$age30_39
IDPH2$age4_c_r <- 1000*IDPH2$age4/IDPH2$age40_49
IDPH2$age5_c_r <- 1000*IDPH2$age5/IDPH2$age50_59
IDPH2$age6_c_r <- 1000*IDPH2$age6/IDPH2$age60_69
IDPH2$age7_c_r <- 1000*IDPH2$age7/IDPH2$age70_79
IDPH2$age8_c_r <- 1000*IDPH2$age8/IDPH2$age80
IDPH2$age_all_c_r <- 1000*IDPH2$age_all_c/IDPH2$age_all

write.csv(IDPH2, file ="/IDPH_rate_longitudinal.csv",row.names=F,na="")#Change to your local drive

#Download ACS Data Profile Table from the Census Bureau website: https://www.census.gov/data/developers/data-sets/acs-5year.html
#Only select the Percent Estimate (PE) variable
# Download the data of PE variables
Census_profile_PE <- read.csv("/Census_profile_PE.csv",header=T,as.is = T)

#Download data by group

#Ancestry
variable <- Census_profile_PE$Name[1:28] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_ancestry.csv",row.names=F,na="")#Change to your local drive

#Bedrooms
variable <- Census_profile_PE$Name[29:35] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_bedroom.csv",row.names=F,na="")#Change to your local drive

#Citizen
variable <- Census_profile_PE$Name[36:38] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_citizen.csv",row.names=F,na="")#Change to your local drive

#Worker
variable <- Census_profile_PE$Name[39:43] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_worker.csv",row.names=F,na="")#Change to your local drive

#Commute
variable <- Census_profile_PE$Name[44:51] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_commute.csv",row.names=F,na="")#Change to your local drive

#Computer
variable <- Census_profile_PE$Name[52:54] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_computer.csv",row.names=F,na="")#Change to your local drive

#Disable
variable <- Census_profile_PE$Name[55:62] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_disable.csv",row.names=F,na="")#Change to your local drive

#Education
variable <- Census_profile_PE$Name[63:72] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_education.csv",row.names=F,na="")#Change to your local drive

#Employment
variable <- Census_profile_PE$Name[73:89] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_employment.csv",row.names=F,na="")#Change to your local drive

#Fertility
variable <- Census_profile_PE$Name[90:96] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_fertility.csv",row.names=F,na="")#Change to your local drive

#Gradeparents
variable <- Census_profile_PE$Name[97:105] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_gradeparents.csv",row.names=F,na="")#Change to your local drive

#Gross rent
variable <- Census_profile_PE$Name[106:123] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_grossrent.csv",row.names=F,na="")#Change to your local drive

#Insurance
variable <- Census_profile_PE$Name[124:147] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_hinsurance.csv",row.names=F,na="")#Change to your local drive

#Hispanic
variable <- Census_profile_PE$Name[148:163] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_hispanic.csv",row.names=F,na="")#Change to your local drive

#House heating
variable <- Census_profile_PE$Name[164:173] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_hheating.csv",row.names=F,na="")#Change to your local drive

#Household
variable <- Census_profile_PE$Name[174:189] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_household.csv",row.names=F,na="")#Change to your local drive

#HOUSING OCCUPANCY
variable <- Census_profile_PE$Name[190:194] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_hoccupancy.csv",row.names=F,na="")#Change to your local drive

#Housing Tenure
variable <- Census_profile_PE$Name[195:199] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_htenure.csv",row.names=F,na="")#Change to your local drive

# Income and benefit
variable <- Census_profile_PE$Name[200:243] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_income.csv",row.names=F,na="")#Change to your local drive

#Industry
variable <- Census_profile_PE$Name[244:257] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_industry.csv",row.names=F,na="")#Change to your local drive

#Language
variable <- Census_profile_PE$Name[258:269] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_language.csv",row.names=F,na="")#Change to your local drive

#Marital
variable <- Census_profile_PE$Name[270:281] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_marital.csv",row.names=F,na="")#Change to your local drive

#Mortgage
variable <- Census_profile_PE$Name[282:284] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_mortgage.csv",row.names=F,na="")#Change to your local drive

#Occupants per room
variable <- Census_profile_PE$Name[285:288] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_occupantsroom.csv",row.names=F,na="")#Change to your local drive


#Occupation
variable <- Census_profile_PE$Name[289:294] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_occupation.csv",row.names=F,na="")#Change to your local drive

#Poverty
variable <- Census_profile_PE$Name[295:313] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_poverty.csv",row.names=F,na="")#Change to your local drive

#Place of birth
variable <- Census_profile_PE$Name[314:320] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_placeofbirth.csv",row.names=F,na="")#Change to your local drive

#Race
variable <- Census_profile_PE$Name[321:357] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_race.csv",row.names=F,na="")#Change to your local drive

#Relationship
variable <- Census_profile_PE$Name[358:364] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_relation.csv",row.names=F,na="")#Change to your local drive

#Residence
variable <- Census_profile_PE$Name[365:372] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_residence.csv",row.names=F,na="")#Change to your local drive

#Rooms
variable <- Census_profile_PE$Name[373:383] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_rooms.csv",row.names=F,na="")#Change to your local drive

#School enrollment
variable <- Census_profile_PE$Name[384:389] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_school_enroll.csv",row.names=F,na="")#Change to your local drive

#Selected
variable <- Census_profile_PE$Name[390:426] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_selected.csv",row.names=F,na="")#Change to your local drive

#Sex and age
variable <- Census_profile_PE$Name[427:458] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_sexage.csv",row.names=F,na="")#Change to your local drive

#total housing unit
variable <- Census_profile_PE$Name[459] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_totalhunit.csv",row.names=F,na="")#Change to your local drive

#US citizenship status
variable <- Census_profile_PE$Name[460:462] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_UScitizen.csv",row.names=F,na="")#Change to your local drive

#Units in structure
variable <- Census_profile_PE$Name[463:472] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_structure.csv",row.names=F,na="")#Change to your local drive

#Housing value
variable <- Census_profile_PE$Name[473:482] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_hvalue.csv",row.names=F,na="")#Change to your local drive

#Vehicle available
variable <- Census_profile_PE$Name[483:487] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_vehicle.csv",row.names=F,na="")#Change to your local drive

#Veteran status
variable <- Census_profile_PE$Name[488:489] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_veteran.csv",row.names=F,na="")#Change to your local drive

#World region
variable <- Census_profile_PE$Name[490:496] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_world.csv",row.names=F,na="")#Change to your local drive

#Year moved in
variable <- Census_profile_PE$Name[497:503] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_yearmovein.csv",row.names=F,na="")#Change to your local drive

#Year of entry US
variable <- Census_profile_PE$Name[504:510] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_yearentry.csv",row.names=F,na="")#Change to your local drive

#Year house built
variable <- Census_profile_PE$Name[511:521] 

SES <- get_acs(geography = "zcta",
               variables = c(variable),
               year = 2018,
               output = "wide")

SES_IL <- subset(SES,SES$GEOID>60000 & SES$GEOID<63000)

write.csv(SES_IL, file ="/Census_PE_yearhouse.csv",row.names=F,na="")#Change to your local drive


########### Poisson regression with repeated measurements of COVID-19 incidence
#Add urban area as covariate
COVID_r <- read.csv("/IDPH_rate_longitudinal.csv",header=T,as.is = T)

library(foreign)
library(MASS)

urban <- read.csv("/IL_urban_area_ZCTA.csv",  as.is = FALSE)

urban2 <- subset(urban, urban$ZCTA_IL_ZC %in% COVID_r$zip)

colnames(urban2)[colnames(urban2)=="ZCTA_IL_ZC"] <- "zip"

#Set a cut off for urban percentage by mean
urban2$Urban[urban2$Urban_per == 100] <- "Urban"
urban2$Urban[urban2$Urban_per < 100] <- "Rural"

COVID <- merge(COVID_r,urban2,by="zip")

colnames(COVID)[colnames(COVID)=="zip"] <- "zipcode"


library(lme4)
#"variables" is a folder to save the downloaded PE variables
filelist <- list.files("/variables",  all.files=FALSE,
                       full.names=FALSE,pattern = "\\.csv$") 

for (j in 1:length(filelist)){ 
  SES <- read.csv(paste0("/variables/",filelist[j]),header=T,as.is = T)
  
  #Select columns end with "PE"
  a <- grepl("^.+(PE)$",colnames(SES))
  col_PM <- subset(colnames(SES),a)
  
  colnames(SES)[colnames(SES)=="GEOID"] <- "zipcode"
  
  #Select variables to merge
  #Age <20
  fit_res1 <- data.frame()
  fit_pvalue1 <- data.frame()
  for( i in 1:length(col_PM)){ 
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age1_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      
      data1$age <- round(data1$age1_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban) +(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1e-2),tolPwrss=1e-2))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age1_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age1_p=pvalue,Age1_s=OR)
      #fit_res1 <- rbind(fit_res1, temp1)
      fit_pvalue1 <- rbind(fit_pvalue1, temp2)
    }
  }
  #Age 20-29
  fit_res2 <- data.frame()
  fit_pvalue2 <- data.frame()
  for( i in 1:length(col_PM)){
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age2_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      data1$age <- round(data1$age2_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban) +(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1),tolPwrss=1))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age2_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age2_p=pvalue,Age2_s=OR)
      #fit_res2 <- rbind(fit_res2, temp1)
      fit_pvalue2 <- rbind(fit_pvalue2, temp2)
    }
  }
  #Age 30-39
  fit_res3 <- data.frame()
  fit_pvalue3 <- data.frame()
  for( i in 1:length(col_PM)){
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age3_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      data1$age <- round(data1$age3_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban) +(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1e-2),tolPwrss=1e-2))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age3_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age3_p=pvalue,Age3_s=OR)
      #fit_res3 <- rbind(fit_res3, temp1)
      fit_pvalue3 <- rbind(fit_pvalue3, temp2)
    }
  }
  #Age 40-49
  fit_res4 <- data.frame()
  fit_pvalue4 <- data.frame()
  for( i in 1:length(col_PM)){
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age4_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      data1$age <- round(data1$age4_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban)+(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1e-2),tolPwrss=1e-2))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age4_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age4_p=pvalue,Age4_s=OR)
      #fit_res4 <- rbind(fit_res4, temp1)
      fit_pvalue4 <- rbind(fit_pvalue4, temp2)
    }
  }
  #Age 50-59
  fit_res5 <- data.frame()
  fit_pvalue5 <- data.frame()
  for( i in 1:length(col_PM)){
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age5_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      data1$age <- round(data1$age5_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban) +(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1e-2),tolPwrss=1e-2))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age5_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age5_p=pvalue,Age5_s=OR)
      #fit_res5 <- rbind(fit_res5, temp1)
      fit_pvalue5 <- rbind(fit_pvalue5, temp2)
    }
  }
  #Age 60-69
  fit_res6 <- data.frame()
  fit_pvalue6 <- data.frame()
  for( i in 1:length(col_PM)){
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age6_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      data1$age <- round(data1$age6_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban) +(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1e-2),tolPwrss=1e-2))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age6_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age6_p=pvalue,Age6_s=OR)
      #fit_res6 <- rbind(fit_res6, temp1)
      fit_pvalue6 <- rbind(fit_pvalue6, temp2)
    }
  }  
  #Age 70-79
  fit_res7 <- data.frame()
  fit_pvalue7 <- data.frame()
  for( i in 1:length(col_PM)){
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age7_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      data1$age <- round(data1$age7_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban) +(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1e-2),tolPwrss=1e-2))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age7_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age7_p=pvalue,Age7_s=OR)
      #fit_res7 <- rbind(fit_res7, temp1)
      fit_pvalue7 <- rbind(fit_pvalue7, temp2)
    }
  }
  #Age >80
  fit_res8 <- data.frame()
  fit_pvalue8 <- data.frame()
  for( i in 1:length(col_PM)){
    if(isFALSE(max(SES[,col_PM[i]], na.rm = TRUE)>100)) #Remove columns with original numbers
    {
      expo <- SES[c("zipcode",col_PM[i])]
      age <- COVID[c("zipcode","age8_c_r","Urban","date")]
      
      data1 <- merge(expo,age,by="zipcode",y.all=T)
      colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
      data1$age <- round(data1$age8_c_r,0)
      
      fit <- glmer(age~expo+factor(Urban) +(1|zipcode),data=data1,family=poisson(link="log"),control=glmerControl(optimizer="bobyqa",boundary.tol=1e-2 ,check.conv.singular =.makeCC(action ="ignore",tol=1e-2),tolPwrss=1e-2))
      
      #R2 <- summary(fit)$adj.r.squared
      pvalue <- coef(summary(fit))[2,'Pr(>|z|)']
      OR <- exp(coef(summary(fit))[2,'Estimate'])
      
      #temp1 <- data.frame(expo=col_PM[i],Age8_R2=R2)
      temp2 <- data.frame(expo=col_PM[i],Age8_p=pvalue,Age8_s=OR)
      #fit_res8 <- rbind(fit_res8, temp1)
      fit_pvalue8 <- rbind(fit_pvalue8, temp2)
    }
  }
  
  print(j)
  
  c1 <- merge(fit_pvalue2,fit_pvalue1,by="expo")
  c2 <- merge(fit_pvalue3,c1,by="expo")
  c3 <- merge(fit_pvalue4,c2,by="expo")
  c4 <- merge(fit_pvalue5,c3,by="expo")
  c5 <- merge(fit_pvalue6,c4,by="expo")
  c6 <- merge(fit_pvalue7,c5,by="expo")
  c7 <- merge(fit_pvalue8,c6,by="expo")
  #Save the Poission regression result in a folder called "Mix"
  write.csv(c7, file =paste0("/Mix_",filelist[j]),row.names=F,na="")
}

#Merge results
filelist <- list.files("/Mix",  all.files=FALSE,
                       full.names=FALSE,pattern = "\\.csv$") 

Linear_all <- data.frame()
for (j in 1:length(filelist)){ 
  Linear <- read.csv(paste0("/Mix/",filelist[j]),header=T,as.is = T)
  
  pos1 <- regexpr('PE_', filelist[j])
  pos2 <- regexpr('.csv', filelist[j])
  group <- substr(filelist[j], pos1+3, pos2-1)
  
  Linear$group <- group
  Linear_all <- rbind(Linear_all, Linear)
}
# Adjust for Bonferroni correction
Linear_all$Age8_adjust <- p.adjust(Linear_all$Age8_p, method = "bonferroni", n = 8*length(Linear_all$Age8_p))
Linear_all$Age7_adjust <- p.adjust(Linear_all$Age7_p, method = "bonferroni", n = 8*length(Linear_all$Age7_p))
Linear_all$Age6_adjust <- p.adjust(Linear_all$Age6_p, method = "bonferroni", n = 8*length(Linear_all$Age6_p))
Linear_all$Age5_adjust <- p.adjust(Linear_all$Age5_p, method = "bonferroni", n = 8*length(Linear_all$Age5_p))
Linear_all$Age4_adjust <- p.adjust(Linear_all$Age4_p, method = "bonferroni", n = 8*length(Linear_all$Age4_p))
Linear_all$Age3_adjust <- p.adjust(Linear_all$Age3_p, method = "bonferroni", n = 8*length(Linear_all$Age3_p))
Linear_all$Age2_adjust <- p.adjust(Linear_all$Age2_p, method = "bonferroni", n = 8*length(Linear_all$Age2_p))
Linear_all$Age1_adjust <- p.adjust(Linear_all$Age1_p, method = "bonferroni", n = 8*length(Linear_all$Age1_p))
# Add labels to the PE variables
short_name <- read.csv("/Short_name.csv",header=T,as.is = T)

colnames(short_name)[colnames(short_name)=="Name"] <- "expo"

Linear_all2 <- merge(Linear_all,short_name,by="expo",x.all=T)

#List of duplicated PE variables
list_a <- c("DP04_0092PE","DP04_0046PE","DP03_0007PE","DP02_0002PE","DP05_0027PE","DP05_0088PE","DP05_0031PE","DP04_0002PE",
            "DP05_0034PE","DP02_0112PE","DP05_0071PE","DP05_0019PE","DP03_0096PE","DP05_0002PE","DP02_0092PE","DP02_0095PE",
            "DP02_0102PE","DP03_0105PE","DP03_0115PE")

Linear_all3 <- subset(Linear_all2, !(Linear_all2$expo %in% list_a) )# Exclude duplicated PE variables

write.csv(Linear_all3, file ="/Linear_all_result_long.csv",row.names=F,na="")

#Correlation
COVID_r <- read.csv("/IDPH_rate_longitudinal.csv",header=T,as.is = T)

COVID <- COVID_r %>%
  group_by(zip) %>%
  summarise(age1=mean(age1_c_r),age2=mean(age2_c_r),age3=mean(age3_c_r),age4=mean(age4_c_r),age5=mean(age5_c_r),
            age6=mean(age6_c_r),age7=mean(age7_c_r),age8=mean(age8_c_r))


filelist <- list.files("/variables",  all.files=FALSE,
                       full.names=FALSE,pattern = "\\.csv$") 

for (j in 1:length(filelist)){ 
  SES <- read.csv(paste0("/variables/",filelist[j]),header=T,as.is = T)
  
  #Select columns end with "PE"
  a <- grepl("^.+(PE)$",colnames(SES))
  col_PM <- subset(colnames(SES),a)
  
  colnames(SES)[colnames(SES)=="GEOID"] <- "zip"
  
  #Select variables to merge
  #Age <20
  fit_res1 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age1")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age1"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age1_corr=P_corr)
    fit_res1 <- rbind(fit_res1, temp)
  }
  #Age 20-29
  fit_res2 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age2")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age2"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age2_corr=P_corr)
    fit_res2 <- rbind(fit_res2, temp)
  }
  #Age 30-39
  fit_res3 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age3")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age3"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age3_corr=P_corr)
    fit_res3 <- rbind(fit_res3, temp)
  }
  #Age 40-49
  fit_res4 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age4")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age4"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age4_corr=P_corr)
    fit_res4 <- rbind(fit_res4, temp)
  }
  #Age 50-59
  fit_res5 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age5")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age5"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age5_corr=P_corr)
    fit_res5 <- rbind(fit_res5, temp)
  }
  #Age 60-69
  fit_res6 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age6")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age6"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age6_corr=P_corr)
    fit_res6 <- rbind(fit_res6, temp)
  }
  #Age 70-79
  fit_res7 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age7")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age7_c_r"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age7_corr=P_corr)
    fit_res7 <- rbind(fit_res7, temp)
  }
  #Age >80
  fit_res8 <- data.frame()
  for( i in 1:length(col_PM)){
    expo <- SES[c("zip",col_PM[i])]
    age <- COVID[c("zip","age8")]
    
    data1 <- merge(expo,age,by="zip",y.all=T)
    colnames(data1)[colnames(data1)==col_PM[i]] <- "expo"
    colnames(data1)[colnames(data1)=="age8_c_r"] <- "age"
    
    P_corr <- cor(data1$expo, data1$age,  method = "pearson", use = "complete.obs")
    
    temp <- data.frame(expo=col_PM[i],Age8_corr=P_corr)
    fit_res8 <- rbind(fit_res8, temp)
  }
  
  b1 <- merge(fit_res2,fit_res1,by="expo")
  b2 <- merge(fit_res3,b1,by="expo")
  b3 <- merge(fit_res4,b2,by="expo")
  b4 <- merge(fit_res5,b3,by="expo")
  b5 <- merge(fit_res6,b4,by="expo")
  b6 <- merge(fit_res7,b5,by="expo")
  b7 <- merge(fit_res8,b6,by="expo")
  #Save correlation results to a folder called "Correlation". All file names start with "Cor_"
  write.csv(b7, file =paste0("/Correlation/Cor_",filelist[j]),row.names=F,na="")
}

# Merge the group-specific results together and make heatmap
filelist <- list.files("/Correlation",  all.files=FALSE,
                       full.names=FALSE,pattern = "\\.csv$") 

Corr_all <- data.frame()
for (j in 1:length(filelist)){ 
  Corr <- read.csv(paste0("/Correlation/",filelist[j]),header=T,as.is = T)
  
  pos1 <- regexpr('PE_', filelist[j])
  pos2 <- regexpr('.csv', filelist[j])
  group <- substr(filelist[j], pos1+3, pos2-1)
  
  Corr$group <- group
  Corr_all <- rbind(Corr_all, Corr)
}

short_name <- read.csv("/Short_name.csv",header=T,as.is = T)

colnames(short_name)[colnames(short_name)=="Name"] <- "expo"

Corr_all2 <- merge(Corr_all,short_name,by="expo",x.all=T)

write.csv(Corr_all2, file ="/Corr_result.csv",row.names=F,na="")

###########R script for IDPH COVID19 paper

#Table 1
# Average population of all ZCTA in IL
zip20_IL <- read.csv("/Census_IL20.csv",header=T,as.is = T)

zip20_IL2 <- subset(zip20_IL, zip20_IL$variable == "B01001_001")

# Read area
library(foreign)
area <- read.dbf("/IL_urban_area_ZCTA.dbf",  as.is = FALSE)
#AREA_GEO is the area of ZCTA, it is in the unit of sqkm

area2 <- area[area$ZCTA_IL_ZC %in% zip20_IL2$GEOID,c("ZCTA_IL_ZC","AREA_GEO","Urban_per")]

#Merge the population and area file
colnames(area2)[colnames(area2)=="ZCTA_IL_ZC"] <- "GEOID"

pop_area <- merge(zip20_IL2, area2, by="GEOID")

#Average population density of ZCTAs in IL
pop_area$pop_den <- pop_area$estimate/pop_area$AREA_GEO
mean(pop_area$pop_den)
#543.1645

#Average % urban
mean(pop_area$Urban_per)
#25.24299

#Exclusive to study ZCTA
COVID_r <- read.csv("/IDPH_rate.csv",header=T,as.is = T)

pop_area2 <- pop_area[pop_area$GEOID %in% COVID_r$zipcode,]
mean(pop_area2$pop_den)
#2340.181
mean(pop_area2$Urban_per)
#90.73451

#Test for population density, All IL vs Study ZCTA
t.test(pop_area$pop_den,pop_area2$pop_den) 
#p-value < 2.2e-16
#Test for % urban, All IL vs Study ZCTA
t.test(pop_area$Urban_per,pop_area2$Urban_per) 
#p-value < 2.2e-16

# Figure 2
IDPH_rate <- read.csv("/IDPH_rate_longitudinal.csv",header=T,as.is = T)

data1 <- IDPH_rate[,c("date","zip","age1_c_r")]
data2 <- IDPH_rate[,c("date","zip","age2_c_r")]
data3 <- IDPH_rate[,c("date","zip","age3_c_r")]
data4 <- IDPH_rate[,c("date","zip","age4_c_r")]
data5 <- IDPH_rate[,c("date","zip","age5_c_r")]
data6 <- IDPH_rate[,c("date","zip","age6_c_r")]
data7 <- IDPH_rate[,c("date","zip","age7_c_r")]
data8 <- IDPH_rate[,c("date","zip","age8_c_r")]

data1$age <- "<20"
data2$age <- "20-29"
data3$age <- "30-39"
data4$age <- "40-49"
data5$age <- "50-59"
data6$age <- "60-69"
data7$age <- "70-79"
data8$age <- "80+"

colnames(data1)[colnames(data1)=="age1_c_r"] <- "covid"
colnames(data2)[colnames(data2)=="age2_c_r"] <- "covid"
colnames(data3)[colnames(data3)=="age3_c_r"] <- "covid"
colnames(data4)[colnames(data4)=="age4_c_r"] <- "covid"
colnames(data5)[colnames(data5)=="age5_c_r"] <- "covid"
colnames(data6)[colnames(data6)=="age6_c_r"] <- "covid"
colnames(data7)[colnames(data7)=="age7_c_r"] <- "covid"
colnames(data8)[colnames(data8)=="age8_c_r"] <- "covid"

new_data <- rbind(data1,data2,data3,data4,data5,data6,data7,data8)

#Calculate zipcode average
library(dplyr)

new_data2 <- new_data %>%
  group_by(date,age) %>%
  summarise(inci=mean(covid))

#Format date in Excel
write.csv(new_data2, file ="/Table2_data.csv",row.names=F,na="")

#Draw line graph
new_data2 <- read.csv("/Table2_data.csv",header=T,as.is = T)

new_data2$date <-  as.Date(new_data2$date, format = "%m/%d")

library(ggplot2)
library(directlabels)
library(ggrepel)

tiff("/Figure2.tiff", width = 1500, height =1300,res=300)
ggplot(data=new_data2, aes(x=date, y=inci, group=age,colour = factor(age))) +
  geom_line(aes(color=factor(age)), size=0.9)+
  geom_point(aes(color=factor(age)))+
  labs(x="Date", y="Average cases/1000 people")+
  scale_colour_discrete(guide = 'none') +
  scale_y_continuous(breaks = seq(0, 32, by = 5))+
  xlim(as.Date(c('2020-05-23','2020-06-20')))+
  ggrepel::geom_text_repel(data = new_data2[new_data2$date=="2020-06-17",], aes(label = age),hjust = 1)
dev.off()

#Table 2
Linear_all <- read.csv("/Linear_all_result_long.csv",header=T,as.is = T)

for (i in 1:length(Linear_all$expo)){
  pos1 <- regexpr('_',Linear_all$expo[i])
  DP <- substr(Linear_all$expo[i], 1, pos1-1)
  Linear_all$DP[i] <- DP
}


library(dplyr)

# Calculate number of subprofiles
sum_linear <- Linear_all %>%
  group_by(DP,group) %>%
  summarise(count=n())

# Age <20
Linear_all1 <- Linear_all[Linear_all$Age1_adjust<0.05,]
sum_linear1 <- Linear_all1 %>%
  group_by(DP,group) %>%
  summarise(count1=n())

# Age 20-29
Linear_all2 <- Linear_all[Linear_all$Age2_adjust<0.05,]
sum_linear2 <- Linear_all2 %>%
  group_by(DP,group) %>%
  summarise(count2=n())

# Age 30-39
Linear_all3 <- Linear_all[Linear_all$Age3_adjust<0.05,]
sum_linear3 <- Linear_all3 %>%
  group_by(DP,group) %>%
  summarise(count3=n())

# Age 40-49
Linear_all4 <- Linear_all[Linear_all$Age4_adjust<0.05,]
sum_linear4 <- Linear_all4 %>%
  group_by(DP,group) %>%
  summarise(count4=n())

# Age 50-59
Linear_all5 <- Linear_all[Linear_all$Age5_adjust<0.05,]
sum_linear5 <- Linear_all5 %>%
  group_by(DP,group) %>%
  summarise(count5=n())

# Age 60-69
Linear_all6 <- Linear_all[Linear_all$Age6_adjust<0.05,]
sum_linear6 <- Linear_all6 %>%
  group_by(DP,group) %>%
  summarise(count6=n())

# Age 70-79
Linear_all7 <- Linear_all[Linear_all$Age7_adjust<0.05,]
sum_linear7 <- Linear_all7 %>%
  group_by(DP,group) %>%
  summarise(count7=n())

# Age 80+
Linear_all8 <- Linear_all[Linear_all$Age8_adjust<0.05,]
sum_linear8 <- Linear_all8 %>%
  group_by(DP,group) %>%
  summarise(count8=n())

#Calculate percentage
data1 <- merge(sum_linear,sum_linear1,by=c("DP","group"), all.x = T)
data2 <- merge(data1,sum_linear2,by=c("DP","group"), all.x = T)
data3 <- merge(data2,sum_linear3,by=c("DP","group"), all.x = T)
data4 <- merge(data3,sum_linear4,by=c("DP","group"), all.x = T)
data5 <- merge(data4,sum_linear5,by=c("DP","group"), all.x = T)
data6 <- merge(data5,sum_linear6,by=c("DP","group"), all.x = T)
data7 <- merge(data6,sum_linear7,by=c("DP","group"), all.x = T)
data8 <- merge(data7,sum_linear8,by=c("DP","group"), all.x = T)

#Calculate the percentage of positive
data8$age1_rate <- 100*data8$count1/data8$count
data8$age2_rate <- 100*data8$count2/data8$count
data8$age3_rate <- 100*data8$count3/data8$count
data8$age4_rate <- 100*data8$count4/data8$count
data8$age5_rate <- 100*data8$count5/data8$count
data8$age6_rate <- 100*data8$count6/data8$count
data8$age7_rate <- 100*data8$count7/data8$count
data8$age8_rate <- 100*data8$count8/data8$count

write.csv(data8, file ="/Table2_positive_profile.csv",row.names=F,na="0")

##### Neatmap #######
#Correlation heatmap
#Heatmap
library(pheatmap)
library(RColorBrewer)
library(viridis)

Corr_all <- read.csv("/Corr_result.csv",header=T,as.is = T)

Corr_all <- Corr_all[c("expo","Age1_corr","Age2_corr","Age3_corr","Age4_corr",
                       "Age5_corr","Age6_corr","Age7_corr","Age8_corr","group","Short_name")]

jpeg("/Heatmap_corr.jpeg", width = 3000, height =6000,res=600)

group <- Corr_all$group
rnames<-Corr_all$Short_name
rownames(Corr_matrix) <- Corr_all$Short_name
colnames(Corr_matrix) <- colnames(Corr_all)[2:9]
Corr_all <- Corr_all[,2:9]
Corr_matrix <- data.matrix(Corr_all)
#Set color gradient
my_palette <- colorRampPalette(colors = c("darkblue", "red"))
breaks <- seq(from=min(range(Corr_matrix)), to=max(range(Corr_matrix)), length.out=100)
midpoint <- which.min(abs(breaks - 0))
rampCol1 <- colorRampPalette(c("darkblue", "grey"))(midpoint)
rampCol2 <- colorRampPalette(c("grey", "red"))(100-(midpoint+1))
rampCols <- c(rampCol1,rampCol2)

pheatmap(
  mat             = Corr_matrix,
  color           = rampCols,
  border_color    = NA,
  drop_levels     = TRUE,
  fontsize        = 1.2,
  labels_row      = rnames,
  show_rownames   = T,
  treeheight_row  = 0, treeheight_col = 0,
  cellwidth = 30, cellheight = 1,
  cluster_cols=F, cluster_rows=F
)
dev.off()

###### Draw networks in R
# Create link files
whole_network <- read.csv("/Linear_Census_Corr_Matrix_sig.csv",header=T,as.is = T)

short_name <- read.csv("/Short_name.csv",header=T,as.is = T)

colnames(short_name)[colnames(short_name)=="Name"] <- "row"

whole_network2 <- merge(whole_network,short_name,by="row",x.all=T)

colnames(whole_network2)[colnames(whole_network2)=="Short_name"] <- "Short_name_row"
colnames(short_name)[colnames(short_name)=="row"] <- "column"

whole_network3 <- merge(whole_network2,short_name,by="column",x.all=T)

colnames(whole_network3)[colnames(whole_network3)=="Short_name"] <- "Short_name_col"

myvars <- c("Short_name_row","Short_name_col","p_corrected","cor","abs_cor")
whole_network4 <- whole_network3[myvars]

whole_network4$cor_d[whole_network4$cor>=0] <- "p"
whole_network4$cor_d[whole_network4$cor<0] <- "n"

write.table(whole_network4, file ="J:/pediatrics/STROUSTRUP/NICUHEALTH/TRAP Study -Xueying/COVID19/Network_whole.txt",sep = ";",row.names=F,na="",col.names = TRUE,quote=FALSE)

# Create Node files
Linear_all <- read.csv("J:/pediatrics/STROUSTRUP/NICUHEALTH/TRAP Study -Xueying/COVID19/Linear_all_result_nodu.csv",header=T,as.is = T)

for (i in 1:8){
  myvars <- c("Short_name",paste0("Age",i,"_adjust"),paste0("Age",i,"_s"))
  Age <- Linear_all[myvars]
  
  colnames(Age)[colnames(Age)==paste0("Age",i,"_adjust")] <- "Age_adjust"
  colnames(Age)[colnames(Age)==paste0("Age",i,"_s")] <- "Age_s"
  
  Age2 <- Age[order(Age$Age_adjust),]
  
  #Nodes <- Age2[Age2$Age_adjust <0.05,c("Short_name","Age_adjust","Age_s")] # Set up a inclusion standard for drawing network
  Nodes <- Age2[1:20,c("Short_name","Age_adjust","Age_s")]
  
  write.table(Nodes, file =paste0("/Network_age",i,"_Nodes.txt"),sep = ";",row.names=F,na="",col.names = TRUE,quote=FALSE)
}

library(tidyverse)
library(tidygraph)
library(ggraph)
library(Gmisc)
library(gridExtra)
library(pROC)
library(gtable)

#Read data for network
links_w <- read.table("/Network_whole.txt",header=T,as.is = T,sep = ";")

#Age <20
nodes <- read.table("/Network_age1_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age20 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy1 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

#Age 20-29
nodes <- read.table("/Network_age2_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age20_29 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy2 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

#30-39
nodes <- read.table("/Network_age3_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age30_39 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy3 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)


#40-49
nodes <- read.table("/Network_age4_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age40_49 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy4 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

#50-59
nodes <- read.table("/Network_age5_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age50_59 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy5 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

#60-69
nodes <- read.table("/Network_age6_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age60_69 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy6 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

#70-79
nodes <- read.table("/Network_age7_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age70_79 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy7 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

# Age 80+
nodes <- read.table("/Network_age8_Nodes.txt",header=T,as.is = T,sep = ";")
links_w2 <- subset(links_w, links_w$Short_name_row %in% nodes$Short_name)
links_w3 <- subset(links_w2, links_w2$Short_name_col %in% nodes$Short_name)

links <- links_w3[,c("Short_name_row","Short_name_col","abs_cor","cor_d")]

#Remove nodes without links
link_name <- links %>% 
  select(Short_name_row, Short_name_col) %>% 
  t %>% c %>% unique

nodes2 <- subset(nodes,nodes$Short_name %in% link_name )

#Save the unconnected nodes as annotation table
nodes_discon <- subset(nodes,! nodes$Short_name %in% link_name )
nodes_discon$Pvalue <- round(abs(log10(nodes_discon$Age_adjust)),0)
nodes_discon$Slope <- round(nodes_discon$Age_s,2)  

colnames(nodes_discon)[colnames(nodes_discon)=="Short_name"] <- "Variable"

myvars <- c("Variable","Slope","Pvalue")
nodes_age80 <- nodes_discon[myvars]

nodes2$slope_d[nodes2$Age_s>=0] <- "p"
nodes2$slope_d[nodes2$Age_s<0] <- "n"

nodes2$log_p <- 0-log10(nodes2$Age_adjust)

routes_tidy8 <- tbl_graph(nodes = nodes2, edges = links, directed = TRUE)

tsize <- ttheme_default(base_size = 7, base_colour = "black")

g1 <- ggraph(routes_tidy1, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,20), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(4,6,8))))+
  theme(legend.position = "right" )+
  ggtitle("Age <20") +
  theme_graph()+
  annotation_custom(tableGrob(nodes_age20[1:4,1:3],theme = tsize), xmin=-1.5, xmax=0, ymin=-4, ymax=0)

jpeg("/Age1_network.jpeg", width = 1100, height =700,res=110)
g1
dev.off()

#Age 20-29
g2 <- ggraph(routes_tidy2, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,20), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(2,4,6,8,10))))+
  theme(legend.position = "right" )+
  ggtitle("Age 20-29") +
  theme_graph() +
  annotation_custom(tableGrob(nodes_age20_29,theme = tsize), xmin=-3, xmax=0, ymin=2, ymax=0)

jpeg("/Age2_network.jpeg", width = 1100, height =800,res=110)
g2
dev.off()

#Age 30-39
g3 <- ggraph(routes_tidy3, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,20), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(2,4,6,8))))+
  theme(legend.position = "right" )+
  ggtitle("Age 30-39") +
  theme_graph() +
  annotation_custom(tableGrob(nodes_age30_39,theme = tsize), xmin=1.5, xmax=0, ymin=-2, ymax=0)

jpeg("/Age3_network.jpeg", width = 1100, height =800,res=110)
g3
dev.off()

#Age 40-49
g4 <- ggraph(routes_tidy4, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,20), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(2,4,6,8,10))))+
  theme(legend.position = "right" )+
  ggtitle("Age 40-49") +
  theme_graph() +
  annotation_custom(tableGrob(nodes_age40_49,theme = tsize), xmin=2.5, xmax=0, ymin=1.1, ymax=0)

jpeg("/Age4_network.jpeg", width = 1100, height =800,res=110)
g4
dev.off()

#Age 50-59
g5 <- ggraph(routes_tidy5, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,20), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(2,4,6,8,10))))+
  theme(legend.position = "right" )+
  ggtitle("Age 50-59") +
  theme_graph()  +
  annotation_custom(tableGrob(nodes_age50_59,theme = tsize), xmin=2.5, xmax=0, ymin=-2.4, ymax=0)

jpeg("/Age5_network.jpeg", width = 1100, height =800,res=110)
g5
dev.off()

#Age 60-69
g6 <- ggraph(routes_tidy6, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,20), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(2,4,6,8))))+
  theme(legend.position = "right" )+
  ggtitle("Age 60-69") +
  theme_graph() +
  annotation_custom(tableGrob(nodes_age60_69,theme = tsize), xmin=1.3, xmax=0, ymin=2.4, ymax=0)

jpeg("/Age6_network.jpeg", width = 1100, height =800,res=110)
g6
dev.off()

#Age 70-79
g7 <- ggraph(routes_tidy7, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,15), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(2,4,6,8,10))))+
  theme(legend.position="right")+
  ggtitle("Age 70-79") +
  theme_graph() +
  annotation_custom(tableGrob(nodes_age70_79,theme = tsize), xmin=-1.2, xmax=0, ymin=-3.5, ymax=0)

jpeg("/Age7_network.jpeg", width = 1100, height =800,res=110)
g7
dev.off()

# Age 80+
g8 <- ggraph(routes_tidy8, layout = 'kk') + 
  geom_edge_link(aes(colour  = factor(cor_d)), alpha = 1) + 
  geom_node_point(aes(size  = log_p,colour=Age_s),pch=16) + scale_size(range = c(2,15), name="Poisson regression:\n-Log10(P-value)")+
  scale_color_gradient2(name="Poisson regression:\nSlope",midpoint=0, low="green", mid="white", high="red", space ="Lab" )+
  geom_node_text(aes(label = Short_name), repel = TRUE,check_overlap =T) +
  scale_edge_colour_discrete(name="Correlation coefficients:\nDirection",
                             breaks=c("n","p"),
                             labels=c("Negative","Positive"))+
  guides(colour = guide_colorbar(order = 1),
         size = guide_legend(order = 2,override.aes = list(size = c(2,4,6,8))))+
  theme(legend.position="right")+
  ggtitle("Age 80+") +
  theme_graph()+
  annotation_custom(tableGrob(nodes_age80,theme = tsize), xmin=-5, xmax=0, ymin=-3.3, ymax=0)

jpeg("/Age8_network.jpeg", width =1100, height =800,res=110)
g8
dev.off()

################ Create VennDiagram##########
library(VennDiagram)
library(extrafont)
library(showtext)

Age1 <- read.table("/Network_age1_Nodes.txt",header=T,as.is = T,sep = ";")
Age2 <- read.table("/Network_age2_Nodes.txt",header=T,as.is = T,sep = ";")
Age3 <- read.table("/Network_age3_Nodes.txt",header=T,as.is = T,sep = ";")
Age4 <- read.table("/Network_age4_Nodes.txt",header=T,as.is = T,sep = ";")
Age5 <- read.table("/Network_age5_Nodes.txt",header=T,as.is = T,sep = ";")
Age6 <- read.table("/Network_age6_Nodes.txt",header=T,as.is = T,sep = ";")
Age7 <- read.table("/Network_age7_Nodes.txt",header=T,as.is = T,sep = ";")
Age8 <- read.table("/Network_age8_Nodes_1row.txt",header=T,as.is = T,sep = ";")

Age_adult <- rbind(Age2,Age3,Age4,Age5)
adult <- unique(Age_adult$Short_name)

Age_senior <- rbind(Age6,Age7,Age8)
senior <- unique(Age_senior$Short_name)

a <- venn.diagram(
  x = list(Age1$Short_name, adult,senior),
  category.names = c("<20","Adults","Senior"),imagetype="svg",
  filename = '/venn_diagramm.svg',
  output=TRUE
)


a <- venn.diagram(
  x = list(Age1$Short_name, adult,senior),
  category.names = c("Children (<20)","Adults (20-59)","Senior (60+)"),
  filename = NULL,
  output=TRUE
)

# Chart
setEPS()
postscript("/venn_diagramm.eps",fonts = "serif")
grid.draw(a)
dev.off()




