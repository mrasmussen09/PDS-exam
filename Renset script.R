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
#### 6) Test af fit                                 ####
#### 7) Illustration                                ####
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
data_wdi <- WDI(country =c("AFG",	"AGO",	"AZE",	"BEN",	"BTN",	"BOL",	"BFA",	"BDI",	"KHM",	"CMR",	"CAF",	"TCD",	"COM",	"ZAR",	"COK",	"CIV",	"DJI", "GNQ",	"ERI",	"ETH",	"GMB",	"GHA",	"GIN",	"GNB",	"GUY",	"HTI",	"KAZ",	"KEN",	"PRK",	"KGZ",	"LAO",	"LSO",	"LBR",	"MDG",	"MWI",	"MLI",	"MRT",	"MDA",	"MNG",	"MOZ",	"MMR",	"NPL",	"NER",	"NGA",	"RWA",	"SEN",	"SLE",	"SOM",	"SDN",	"TJK",	"TZA",	"TGO",	"TKM",	"UGA",	"UZB",	"YEM",	"ZMB",	"ZWE"), indicator = c("VC.PKP.TOTL.UN", "IC.EXP.CSBC.CD","IC.IMP.CSBC.CD", "DT.ODA.OATL.CD", "NY.ADJ.DMIN.GN.ZS", "NY.GDP.PETR.RT.ZS"), start = 1975, end = 2015, extra = FALSE, cache = NULL)
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
  select(scode, year, polity = polity2) %>%
  mutate_at(vars(year, polity),
            funs(as.integer))%>%
  filter(year == 1975:2015)

glimpse(data_pol4)
View(data_pol4)

# Generere variable med gennemsnit til brug for bølger. Jeg vælger at anvende fire bølger med gennemsnit for 10 år.
edt_wdi <- data_wdi %>%
  mutate(udv1 = mean(year == 1975:1984)) %>%
  mutate(udv2 = mean(year == 1985:1994)) %>%
  mutate(udv2 = mean(year == 1995:1994)) %>%
  mutate(udv2 = mean(year == 2005:2014))

View(edt_wdi)

edt_pol4 <- data_pol4 %>%
  mutate(demo1 = mean(year == 1975:1984)) %>%
  mutate(demo2 = mean(year == 1985:1994)) %>%
  mutate(demo3 = mean(year == 1995:1994)) %>%
  mutate(demo4 = mean(year == 2005:2014))
        
View(edt_pol4)
########################################################
# 3) Inspicerer data og variable

# 4) kombinerer datasæt
# Begge datasæt klar til at blive kombineret. Samlet datasæt hedder data_bb (Bottom Billion)
data_bb <- full_join(edt_wdi, edt_pol4)
View(data_bb)
# 5) Estimation af model
model <- '
# regressions
demo1 ~ udv1
demo2 ~ udv1 + udv2
demo3 ~ udv2 + udv3
demo4 ~ udv3 + udv4

# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8

6) test af fit
fit <- sem(model, data=data_bb)
summary(fit, standardized=FALSE)