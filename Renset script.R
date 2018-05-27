########################################################
########################################################
########################################################
####                                                ####
#### Aflevering - Political Data Science            ####
#### Udarbejdet af Mathias Rasmussen                ####
#### Emne: The Bottom Billion i tidsserieoerspektiv ####
####                                                ####
########################################################
########################################################
#### INDHOLD                                        ####
#### 1) Anvendte pakker                             ####
#### 2) Load af data og merge                       ####
#### 3) Inspicerer data                             ####
#### 4) Kombinerer variable til brug for analyse    ####
#### 5) Estimation af SEM model                     ####
#### 6) Illustration af data                        ####
#### 7) Afrapportering af estimation                ####
########################################################
########################################################
########################################################

# 1) Relevante pakker anvendt i opgaven
library(dplyr) #Indeholder mange brugbare funktioner til alt fra illustration og databehandling
library(tidyr) # god til databehandling
library(httr) 
library(readxl)
library(WDI) # World Bank Development Indicators API
library(psData) #Political Data pakken indeholder gængse demokratiindeks. Jeg vil anvende Polity IV
library(lavaan) #Udviklet pakke til SEM estimation til forskningsbrug

########################################################

# 2) Loader data
# Jeg begynder med at lave to datasæt; et for hver af de to datakilder jeg anvender

# Laver datasæt med World Bank World Development Index 
data_wdi <- WDI(country =c("AFG",	"AGO",	"AZE",	"BEN",	"BTN",	"BOL",	"BFA",	"BDI",	"KHM",	"CMR",	"CAF",	"TCD",	"COM",	"ZAR",	"COK",	"CIV",	"DJI", "GNQ",	"ERI",	"ETH",	"GMB",	"GHA",	"GIN",	"GNB",	"GUY",	"HTI",	"KAZ",	"KEN",	"PRK",	"KGZ",	"LAO",	"LSO",	"LBR",	"MDG",	"MWI",	"MLI",	"MRT",	"MDA",	"MNG",	"MOZ",	"MMR",	"NPL",	"NER",	"NGA",	"RWA",	"SEN",	"SLE",	"SOM",	"SDN",	"TJK",	"TZA",	"TGO",	"TKM",	"UGA",	"UZB",	"YEM",	"ZMB",	"ZWE"), indicator = c("VC.PKP.TOTL.UN", "IC.EXP.CSBC.CD","IC.IMP.CSBC.CD", "DT.ODA.OATL.CD", "NY.ADJ.DMIN.GN.ZS", "NY.GDP.PETR.RT.ZS"), start = 1974, end = 2017, extra = FALSE, cache = NULL)
View(data_wdi)

# Laver datasæt med Polity IV
# forsøger at indlæse pakke med API, men virker desværre ikke
data_pol4 <- PolityGet(vars = 'polity2')
head(data_pol4)
View(data_pol4)

# Indlæser derfor datasæt i R via URL
polity <- "http://www.systemicpeace.org/inscr/p4v2016.xls"

# Download Polity data to temporary file
GET(polity, write_disk(polity.temp <- tempfile(fileext = ".xls")))
# Read and clean Polity data
data_pol4 <- read_excel(polity.temp) %>%
  select(cowcode = scode, year, polity = polity2) %>%
  mutate_at(vars(year, cowcode, polity),
            funs(as.integer))%>%
  filter(year == 1965:2015)

glimpse(data_pol4)
View(data_pol4)

# Begge datasæt klar til at blive kombineret. Samlet datasæt hedder data_bb (Bottom Billion)
data_bb <- left_join()
########################################################