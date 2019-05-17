#clear memory
rm(list=ls())

##Load packages
requirements <- c('readxl', 'caTools', 'randomForest', 'caret', 'stringr', 'dummies', 'fastDummies', 'data.table','e1071','SparseM','Metrics','xlsx','glmnet','MASS','RRF','reshape','tidyr','xts','dplyr','lubridate','zoo','rfUtilities','tree','randomForestExplainer')
for(requirement in requirements){if(!(requirement %in% installed.packages())) install.packages(requirement, repos = "http://cran.us.r-project.org")}
lapply(requirements, require, character.only=T)
options(width=150)

#read in data
train <- read_excel("BK HPI/Brooklyn_House_Train.xlsx")
test<-read_excel("BK HPI/Brooklyn_House_Test.xlsx")

train$train<-1
test$train<-0

data<-rbind(train,test)

#subset to three building class categories
data$building_class_category<-str_sub(data$building_class_category,1,2)

data$building_class_category<-as.numeric(data$building_class_category)
data<-data[which(data$building_class_category<=3),]

# histogram of raw sale price
hist(data$sale_price,breaks=100)

# log transform sale price
data$lprice<-log(data$sale_price)

# histogram of log transformed sale price
hist(data$lprice,breaks=100)

# plot(train$building_class,type="1")
# train<-train[which(train$sale_price!=0),]


# impute mean of continuous variables
data$year_of_sale[is.na(data$year_of_sale)] <- mean(data$year_of_sale,na.rm=TRUE)

# delete spaces and dashes
str_replace_all(data$neighborhood, fixed(" "), "")
str_replace_all(data$neighborhood, fixed("-"), "")

# dmy <- predict(dummyVars(train$neighborhood, data = train),newdata=train)
# trsf <- data.frame(predict(dmy, newdata = train))

#convert character variables to numeric
data$CD<-as.numeric(data$CD)
data$CT2010<-as.numeric(data$CT2010)
data$CB2010<-as.numeric(data$CB2010)
data$SchoolDist<-as.numeric(data$SchoolDist)
data$Council<-as.numeric(data$Council)
data$ZipCode<-as.numeric(data$ZipCode)
data$FireComp<-as.numeric(data$FireComp)
data$PolicePrct<-as.numeric(data$PolicePrct)
data$HealthCent<-as.numeric(data$HealthCent)
data$HealthArea<-as.numeric(data$HealthArea)
data$SanitBoro<-as.numeric(data$SanitBoro)
data$SanitDistr<-as.numeric(data$SanitDistr)
data$SanitSub<-as.numeric(data$SanitSub)
data$Address<-as.numeric(data$Address)
data$ZoneDist1<-as.numeric(data$ZoneDist1)
data$ZoneDist2<-as.numeric(data$ZoneDist2)
data$ZoneDist3<-as.numeric(data$ZoneDist3)
data$ZoneDist4<-as.numeric(data$ZoneDist4)
data$Overlay1<-as.numeric(data$Overlay1)
data$Overlay2<-as.numeric(data$Overlay2)
data$SPDist1<-as.numeric(data$SPDist1)
data$SPDist2<-as.numeric(data$SPDist2)
data$SPDist3<-as.numeric(data$SPDist3)
data$LtdHeight<-as.numeric(data$LtdHeight)
data$SplitZone<-as.numeric(data$SplitZone)
data$BldgClass<-as.numeric(data$BldgClass)
data$LandUse<-as.numeric(data$LandUse)
data$Easements<-as.numeric(data$Easements)
data$OwnerName<-as.numeric(data$OwnerName)
data$LotArea<-as.numeric(data$LotArea)
data$BldgArea<-as.numeric(data$BldgArea)
data$ComArea<-as.numeric(data$ComArea)
data$ResArea<-as.numeric(data$ResArea)
data$OfficeArea<-as.numeric(data$OfficeArea)
data$RetailArea<-as.numeric(data$RetailArea)
data$GarageArea<-as.numeric(data$GarageArea)
data$StrgeArea<-as.numeric(data$StrgeArea)
data$FactryArea<-as.numeric(data$FactryArea)
data$OtherArea<-as.numeric(data$OtherArea)
data$AreaSource<-as.numeric(data$AreaSource)
data$NumBldgs<-as.numeric(data$NumBldgs)
data$NumFloors<-as.numeric(data$NumFloors)
data$UnitsRes<-as.numeric(data$UnitsRes)
data$UnitsTotal<-as.numeric(data$UnitsTotal)
data$LotFront<-as.numeric(data$LotFront)
data$LotDepth<-as.numeric(data$LotDepth)
data$BldgFront<-as.numeric(data$BldgFront)
data$BldgDepth<-as.numeric(data$BldgDepth)
data$Ext<-as.numeric(data$Ext)
data$ProxCode<-as.numeric(data$ProxCode)
data$IrrLotCode<-as.numeric(data$IrrLotCode)
data$LotType<-as.numeric(data$LotType)
data$BsmtCode<-as.numeric(data$BsmtCode)
data$AssessLand<-as.numeric(data$AssessLand)
data$AssessTot<-as.numeric(data$AssessTot)
data$ExemptLand<-as.numeric(data$ExemptLand)
data$ExemptTot<-as.numeric(data$ExemptTot)
data$YearBuilt<-as.numeric(data$YearBuilt)
data$YearAlter1<-as.numeric(data$YearAlter1)
data$YearAlter2<-as.numeric(data$YearAlter2)
data$HistDist<-as.numeric(data$HistDist)
data$Landmark<-as.numeric(data$Landmark)
data$BuiltFAR<-as.numeric(data$BuiltFAR)
data$ResidFAR<-as.numeric(data$ResidFAR)
data$CommFAR<-as.numeric(data$CommFAR)
data$FacilFAR<-as.numeric(data$FacilFAR)
data$BoroCode<-as.numeric(data$BoroCode)
data$BBL<-as.numeric(data$BBL)
data$CondoNo<-as.numeric(data$CondoNo)
data$Tract2010<-as.numeric(data$Tract2010)
data$XCoord<-as.numeric(data$XCoord)
data$YCoord<-as.numeric(data$YCoord)
data$ZoneMap<-as.numeric(data$ZoneMap)
data$ZMCode<-as.numeric(data$ZMCode)
data$Sanborn<-as.numeric(data$Sanborn)
data$TaxMap<-as.numeric(data$TaxMap)
data$EDesigNum<-as.numeric(data$EDesigNum)
data$APPBBL<-as.numeric(data$APPBBL)
data$APPDate<-as.numeric(data$APPDate)
data$PLUTOMapID<-as.numeric(data$PLUTOMapID)
data$FIRM07_FLA<-as.numeric(data$FIRM07_FLA)
data$PFIRM15_FL<-as.numeric(data$PFIRM15_FL)
data$Version<-as.numeric(data$Version)
data$MAPPLUTO_F<-as.numeric(data$MAPPLUTO_F)
data$SHAPE_Leng<-as.numeric(data$SHAPE_Leng)
data$SHAPE_Area<-as.numeric(data$SHAPE_Area)


# drop variables with too many categories to be meaningful
myvars <- names(data) %in% c("block", "lot", "SanitSub",'Address','address','OwnerName','building_class','apartment_number','Borough','BoroCode') 
data <- data[!myvars]


#create dummy variables from categorical
data <- fastDummies::dummy_cols(data)


############################basic logistic model##########################
# logit<-glm(lprice~building_class_category+tax_class+zip_code+block,data=train)
# AIC(logit)

#create function to return mode (for imputing in categorical variables where there is no mean)
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }

#split price into categories
# train$pricecat<-cut(train$sale_price,seq(0, 1000000, 50000))
# train$pricecat<-as.numeric(train$pricecat)
# train$areacat<-cut(train$gross_sqft,seq(0,400000,1000))
# 
# test$pricecat<-cut(test$sale_price,seq(0, 1000000, 50000))
# test$pricecat<-as.numeric(test$pricecat)
# test$areacat<-cut(test$gross_sqft,seq(0,400000,1000))
# 
# train$sale_price<-na.omit(train$sale_price)



#trim spaces and dashes from variable names
tidy.name.vector <- make.names(data, unique=TRUE)
names(data) <- gsub(" ", "_", names(data))
names(data) <- gsub("-", "_", names(data))

# sapply(newdata, function(x) sum(is.na(x)))

# drop variables with all NA values in test dataset
# myvars <- names(train) %in% c("tax_class_2C", "lprice", "OwnerType_M",'OwnerType_C') 
# train <- train[!myvars]

# multipleRF <- dlapply(1:4, 
#                       function(n){
#                         randomForest::randomForest
# 

# impute mean of continuous variables for NAs
data$BldgArea[is.na(data$BldgArea)]<-mean(data$BldgArea)
data$BldgDepth[is.na(data$BldgDepth)]<-mean(data$BldgDepth)
data$BldgFront[is.na(data$BldgFront)]<-mean(data$BldgFront)
data$BuiltFAR[is.na(data$BuiltFAR)]<-mean(data$BuiltFAR)
data$FacilFAR[is.na(data$FacilFAR)]<-mean(data$FacilFAR)
data$GarageArea[is.na(data$GarageArea)]<-mean(data$GarageArea)
data$gross_sqft[is.na(data$gross_sqft)]<-mean(data$gross_sqft)
data$land_sqft[is.na(data$land_sqft)]<-mean(data$land_sqft)
data$LotArea[is.na(data$LotArea)]<-mean(data$LotArea)
data$LotDepth[is.na(data$LotDepth)]<-mean(data$LotDepth)
data$LotFront[is.na(data$LotFront)]<-mean(data$LotFront)
data$lprice[is.na(data$lprice)]<-mean(data$lprice)
data$NumFloors[is.na(data$NumFloors)]<-mean(data$NumFloors)
data$OfficeArea[is.na(data$OfficeArea)]<-mean(data$OfficeArea)
data$OtherArea[is.na(data$OtherArea)]<-mean(data$OtherArea)
data$ResArea[is.na(data$ResArea)]<-mean(data$ResArea)
data$SHAPE_Area[is.na(data$SHAPE_Area)]<-mean(data$SHAPE_Area)
data$SHAPE_Leng[is.na(data$SHAPE_Leng)]<-mean(data$SHAPE_Leng)
data$StrgeArea[is.na(data$StrgeArea)]<-mean(data$StrgeArea)
data$total_units[is.na(data$total_units)]<-mean(data$total_units)
data$XCoord[is.na(data$XCoord)]<-mean(data$XCoord)
data$YCoord[is.na(data$YCoord)]<-mean(data$YCoord)
data$YearAlter2[is.na(data$YearAlter2)]<-mean(data$YearAlter2)

#replace NAs in categorical variables with 0
data$X__1[is.na(data$X__1)]<-0
data$APPBBL[is.na(data$APPBBL)]<-0
data$APPDate[is.na(data$APPDate)]<-0
data$AreaSource[is.na(data$AreaSource)]<-0
data$AssessLand[is.na(data$AssessLand)]<-0
data$AssessTot[is.na(data$AssessTot)]<-0
data$BBL[is.na(data$BBL)]<-0
data$BldgClass[is.na(data$BldgClass)]<-0
data$borough[is.na(data$borough)]<-0
data$BsmtCode[is.na(data$BsmtCode)]<-0
data$building_class_at_sale[is.na(data$building_class_at_sale)]<-0
data$building_class_at_sale_A0[is.na(data$building_class_at_sale_A0)]<-0
data$building_class_at_sale_A1[is.na(data$building_class_at_sale_A1)]<-0
data$building_class_at_sale_A2[is.na(data$building_class_at_sale_A2)]<-0
data$building_class_at_sale_A3[is.na(data$building_class_at_sale_A3)]<-0
data$building_class_at_sale_A4[is.na(data$building_class_at_sale_A4)]<-0
data$building_class_at_sale_A5[is.na(data$building_class_at_sale_A5)]<-0
data$building_class_at_sale_A6[is.na(data$building_class_at_sale_A6)]<-0
data$building_class_at_sale_A7[is.na(data$building_class_at_sale_A7)]<-0
data$building_class_at_sale_A9[is.na(data$building_class_at_sale_A9)]<-0
data$building_class_at_sale_B1[is.na(data$building_class_at_sale_B1)]<-0
data$building_class_at_sale_B2[is.na(data$building_class_at_sale_B2)]<-0
data$building_class_at_sale_B3[is.na(data$building_class_at_sale_B3)]<-0
data$building_class_at_sale_B9[is.na(data$building_class_at_sale_B9)]<-0
data$building_class_at_sale_C0[is.na(data$building_class_at_sale_C0)]<-0
data$building_class_at_sale_S0[is.na(data$building_class_at_sale_S0)]<-0
data$building_class_at_sale_S1[is.na(data$building_class_at_sale_S1)]<-0
data$building_class_at_sale_S2[is.na(data$building_class_at_sale_S2)]<-0
data$building_class_category[is.na(data$building_class_category)]<-0
data$CB2010[is.na(data$CB2010)]<-0
data$CD[is.na(data$CD)]<-0
data$ComArea[is.na(data$ComArea)]<-0
data$commercial_units[is.na(data$commercial_units)]<-0
data$CommFAR[is.na(data$CommFAR)]<-0
data$CondoNo[is.na(data$CondoNo)]<-0
data$Council[is.na(data$Council)]<-0
data$CT2010[is.na(data$CT2010)]<-0
data$easement[is.na(data$easement)]<-0
data$Easements[is.na(data$Easements)]<-0
data$EDesigNum[is.na(data$EDesigNum)]<-0
data$ExemptLand[is.na(data$ExemptLand)]<-0
data$ExemptTot[is.na(data$ExemptTot)]<-0
data$Ext[is.na(data$Ext)]<-0
data$FactryArea[is.na(data$FactryArea)]<-0
data$FireComp[is.na(data$FireComp)]<-0
data$FIRM07_FLA[is.na(data$FIRM07_FLA)]<-0
data$HealthArea[is.na(data$HealthArea)]<-0
data$HealthCent[is.na(data$HealthCent)]<-0
data$HistDist[is.na(data$HistDist)]<-0
data$IrrLotCode[is.na(data$IrrLotCode)]<-0
data$Landmark[is.na(data$Landmark)]<-0
data$LandUse[is.na(data$LandUse)]<-0
data$LotType[is.na(data$LotType)]<-0
data$LtdHeight[is.na(data$LtdHeight)]<-0
data$MAPPLUTO_F[is.na(data$MAPPLUTO_F)]<-0
data$neighborhood[is.na(data$neighborhood)]<-0
data$neighborhood_BATH_BEACH[is.na(data$neighborhood_BATH_BEACH)]<-0
data$neighborhood_BAY_RIDGE[is.na(data$neighborhood_BAY_RIDGE)]<-0
data$neighborhood_BEDFORD_STUYVESANT[is.na(data$neighborhood_BEDFORD_STUYVESANT)]<-0
data$neighborhood_BENSONHURST[is.na(data$neighborhood_BENSONHURST)]<-0
data$neighborhood_BERGEN_BEACH[is.na(data$neighborhood_BERGEN_BEACH)]<-0
data$neighborhood_BOERUM_HILL[is.na(data$neighborhood_BOERUM_HILL)]<-0
data$neighborhood_BOROUGH_PARK[is.na(data$neighborhood_BOROUGH_PARK)]<-0
data$neighborhood_BRIGHTON_BEACH[is.na(data$neighborhood_BRIGHTON_BEACH)]<-0
data$neighborhood_BROOKLYN_HEIGHTS[is.na(data$neighborhood_BROOKLYN_HEIGHTS)]<-0
data$neighborhood_BROOKLYN_UNKNOWN[is.na(data$neighborhood_BROOKLYN_UNKNOWN)]<-0
data$neighborhood_BROWNSVILLE[is.na(data$neighborhood_BROWNSVILLE)]<-0
data$neighborhood_BUSH_TERMINAL[is.na(data$neighborhood_BUSH_TERMINAL)]<-0
data$neighborhood_BUSHWICK[is.na(data$neighborhood_BUSHWICK)]<-0
data$neighborhood_CANARSIE[is.na(data$neighborhood_CANARSIE)]<-0
data$neighborhood_CARROLL_GARDENS[is.na(data$neighborhood_CARROLL_GARDENS)]<-0
data$neighborhood_CLINTON_HILL[is.na(data$neighborhood_CLINTON_HILL)]<-0
data$neighborhood_COBBLE_HILL[is.na(data$neighborhood_COBBLE_HILL)]<-0
data$neighborhood_COBBLE_HILL_WEST[is.na(data$neighborhood_COBBLE_HILL_WEST)]<-0
data$neighborhood_CONEY_ISLAND[is.na(data$neighborhood_CONEY_ISLAND)]<-0
data$neighborhood_CROWN_HEIGHTS[is.na(data$neighborhood_CROWN_HEIGHTS)]<-0
data$neighborhood_CYPRESS_HILLS[is.na(data$neighborhood_CYPRESS_HILLS)]<-0
data$neighborhood_DOWNTOWN_FULTON_FERRY[is.na(data$neighborhood_DOWNTOWN_FULTON_FERRY)]<-0
data$neighborhood_DOWNTOWN_FULTON_MALL[is.na(data$neighborhood_DOWNTOWN_FULTON_MALL)]<-0
data$neighborhood_DOWNTOWN_METROTECH[is.na(data$neighborhood_DOWNTOWN_METROTECH)]<-0
data$neighborhood_DYKER_HEIGHTS[is.na(data$neighborhood_DYKER_HEIGHTS)]<-0
data$neighborhood_EAST_NEW_YORK[is.na(data$neighborhood_EAST_NEW_YORK)]<-0
data$neighborhood_FLATBUSH_CENTRAL[is.na(data$neighborhood_FLATBUSH_CENTRAL)]<-0
data$neighborhood_FLATBUSH_EAST[is.na(data$neighborhood_FLATBUSH_EAST)]<-0
data$neighborhood_FLATBUSH_LEFFERTS_GARDEN[is.na(data$neighborhood_FLATBUSH_LEFFERTS_GARDEN)]<-0
data$neighborhood_FLATBUSH_NORTH[is.na(data$neighborhood_FLATBUSH_NORTH)]<-0
data$neighborhood_FLATLANDS[is.na(data$neighborhood_FLATLANDS)]<-0
data$neighborhood_FORT_GREENE[is.na(data$neighborhood_FORT_GREENE)]<-0
data$neighborhood_GERRITSEN_BEACH[is.na(data$neighborhood_GERRITSEN_BEACH)]<-0
data$neighborhood_GOWANUS[is.na(data$neighborhood_GOWANUS)]<-0
data$neighborhood_GRAVESEND[is.na(data$neighborhood_GRAVESEND)]<-0
data$neighborhood_GREENPOINT[is.na(data$neighborhood_GREENPOINT)]<-0
data$neighborhood_KENSINGTON[is.na(data$neighborhood_KENSINGTON)]<-0
data$neighborhood_MADISON[is.na(data$neighborhood_MADISON)]<-0
data$neighborhood_MANHATTAN_BEACH[is.na(data$neighborhood_MANHATTAN_BEACH)]<-0
data$neighborhood_MARINE_PARK[is.na(data$neighborhood_MARINE_PARK)]<-0
data$neighborhood_MIDWOOD[is.na(data$neighborhood_MIDWOOD)]<-0
data$neighborhood_MILL_BASIN[is.na(data$neighborhood_MILL_BASIN)]<-0
data$neighborhood_NAVY_YARD[is.na(data$neighborhood_NAVY_YARD)]<-0
data$neighborhood_OCEAN_HILL[is.na(data$neighborhood_OCEAN_HILL)]<-0
data$neighborhood_OCEAN_PARKWAY_NORTH[is.na(data$neighborhood_OCEAN_PARKWAY_NORTH)]<-0
data$neighborhood_OCEAN_PARKWAY_SOUTH[is.na(data$neighborhood_OCEAN_PARKWAY_SOUTH)]<-0
data$neighborhood_OLD_MILL_BASIN[is.na(data$neighborhood_OLD_MILL_BASIN)]<-0
data$neighborhood_PARK_SLOPE[is.na(data$neighborhood_PARK_SLOPE)]<-0
data$neighborhood_PARK_SLOPE_SOUTH[is.na(data$neighborhood_PARK_SLOPE_SOUTH)]<-0
data$neighborhood_PROSPECT_HEIGHTS[is.na(data$neighborhood_PROSPECT_HEIGHTS)]<-0
data$neighborhood_RED_HOOK[is.na(data$neighborhood_RED_HOOK)]<-0
data$neighborhood_SEAGATE[is.na(data$neighborhood_SEAGATE)]<-0
data$neighborhood_SHEEPSHEAD_BAY[is.na(data$neighborhood_SHEEPSHEAD_BAY)]<-0
data$neighborhood_SPRING_CREEK[is.na(data$neighborhood_SPRING_CREEK)]<-0
data$neighborhood_SUNSET_PARK[is.na(data$neighborhood_SUNSET_PARK)]<-0
data$neighborhood_WILLIAMSBURG_CENTRAL[is.na(data$neighborhood_WILLIAMSBURG_CENTRAL)]<-0
data$neighborhood_WILLIAMSBURG_EAST[is.na(data$neighborhood_WILLIAMSBURG_EAST)]<-0
data$neighborhood_WILLIAMSBURG_NORTH[is.na(data$neighborhood_WILLIAMSBURG_NORTH)]<-0
data$neighborhood_WILLIAMSBURG_SOUTH[is.na(data$neighborhood_WILLIAMSBURG_SOUTH)]<-0
data$neighborhood_WINDSOR_TERRACE[is.na(data$neighborhood_WINDSOR_TERRACE)]<-0
data$neighborhood_WYCKOFF_HEIGHTS[is.na(data$neighborhood_WYCKOFF_HEIGHTS)]<-0
data$NumBldgs[is.na(data$NumBldgs)]<-0
data$Overlay1[is.na(data$Overlay1)]<-0
data$Overlay2[is.na(data$Overlay2)]<-0
data$OwnerType[is.na(data$OwnerType)]<-0
data$OwnerType_C[is.na(data$OwnerType_C)]<-0
data$OwnerType_M[is.na(data$OwnerType_M)]<-0
data$OwnerType_NA[is.na(data$OwnerType_NA)]<-0
data$OwnerType_O[is.na(data$OwnerType_O)]<-0
data$OwnerType_P[is.na(data$OwnerType_P)]<-0
data$OwnerType_X[is.na(data$OwnerType_X)]<-0
data$PFIRM15_FL[is.na(data$PFIRM15_FL)]<-0
data$PLUTOMapID[is.na(data$PLUTOMapID)]<-0
data$PolicePrct[is.na(data$PolicePrct)]<-0
data$ProxCode[is.na(data$ProxCode)]<-0
data$residential_units[is.na(data$residential_units)]<-0
data$ResidFAR[is.na(data$ResidFAR)]<-0
data$RetailArea[is.na(data$RetailArea)]<-0
data$sale_date[is.na(data$sale_date)]<-0
data$sale_price[is.na(data$sale_price)]<-0
data$Sanborn[is.na(data$Sanborn)]<-0
data$SanitBoro[is.na(data$SanitBoro)]<-0
data$SanitDistr[is.na(data$SanitDistr)]<-0
data$SchoolDist[is.na(data$SchoolDist)]<-0
data$SPDist1[is.na(data$SPDist1)]<-0
data$SPDist2[is.na(data$SPDist2)]<-0
data$SPDist3[is.na(data$SPDist3)]<-0
data$SplitZone[is.na(data$SplitZone)]<-0
data$tax_class[is.na(data$tax_class)]<-0
data$tax_class_1[is.na(data$tax_class_1)]<-0
data$tax_class_1B[is.na(data$tax_class_1B)]<-0
data$tax_class_2[is.na(data$tax_class_2)]<-0
data$tax_class_2A[is.na(data$tax_class_2A)]<-0
data$tax_class_2B[is.na(data$tax_class_2B)]<-0
data$tax_class_2C[is.na(data$tax_class_2C)]<-0
data$tax_class_4[is.na(data$tax_class_4)]<-0
data$tax_class_at_sale[is.na(data$tax_class_at_sale)]<-0
data$tax_class_NA[is.na(data$tax_class_NA)]<-0
data$TaxMap[is.na(data$TaxMap)]<-0
data$Tract2010[is.na(data$Tract2010)]<-0
data$UnitsRes[is.na(data$UnitsRes)]<-0
data$UnitsTotal[is.na(data$UnitsTotal)]<-0
data$Version[is.na(data$Version)]<-0
data$year_built[is.na(data$year_built)]<-0
data$year_of_sale[is.na(data$year_of_sale)]<-0
data$YearAlter1[is.na(data$YearAlter1)]<-0
data$YearBuilt[is.na(data$YearBuilt)]<-0
data$zip_code[is.na(data$zip_code)]<-0
data$ZipCode[is.na(data$ZipCode)]<-0
data$ZMCode[is.na(data$ZMCode)]<-0
data$ZoneDist1[is.na(data$ZoneDist1)]<-0
data$ZoneDist2[is.na(data$ZoneDist2)]<-0
data$ZoneDist3[is.na(data$ZoneDist3)]<-0
data$ZoneDist4[is.na(data$ZoneDist4)]<-0
data$ZoneMap[is.na(data$ZoneMap)]<-0

# data$building_class_at_sale<-as.factor(data$building_class_at_sale)
# data$neighborhood<-as.factor(data$neighborhood)
# data$tax_class<-as.factor(data$tax_class)

#create random sample for testing RF model
# samp<-data[sample(1:nrow(data), 1000,replace=FALSE),]



#import macro vars
GDP <- read_excel("BK HPI/GDP.xls", col_types = c("date", 'numeric'))
FEDFUNDS <- read_excel("BK HPI/FEDFUNDS.xls", col_types = c("date",      "numeric"))

#create quarter variable in dataset for merging with GDP
dateqtr<-as.Date(data$sale_date)
data$quarter<-quarter(data$sale_date, with_year = TRUE, fiscal_start = 1)

#extract month and year from date
data$sale_date<-substr(data$sale_date,0,7)
FEDFUNDS$observation_date<-substr(FEDFUNDS$observation_date,0,7)

#change date into quarter for GDP
data$dateqtr<-as.Date(data$sale_date)
data$quarter<-quarter(data$sale_date, with_year = TRUE, fiscal_start = 1)

GDP$dateqtr<-as.Date(GDP$observation_date)
GDP$quarter<-quarter(GDP$observation_date, with_year = TRUE, fiscal_start = 1)

# GDP %>% distinct(quarter, .keep_all = TRUE)
FEDFUNDS %>% distinct(date, .keep_all = TRUE)

#change variable names for merge
data$date<-data$sale_date
FEDFUNDS$date<-FEDFUNDS$observation_date

#backfill missing values of econ variables
# na.locf(GDP$GDP)
na.locf(FEDFUNDS$FEDFUNDS)

#merge in econ vars
data<-merge(data,GDP,by='quarter', all.x = TRUE)
data<-merge(data,FEDFUNDS,by='date', all.x = TRUE)
# na.locf(data$GDP)
# na.locf(data$FEDFUNDS)

#split data into test and train
train<-data[which(data$train==1),]
test<-data[which(data$train==0),]

#display number of observations with sale price above second standard deviation
# sum(train$sale_price>(mean(train$sale_price)+2*sd(train$sale_price)))

#replace observations with sale price above second SD with 10% trimmed mean
train$sale_price[train$sale_price>(mean(train$sale_price)+2*sd(train$sale_price))]<-mean(train$sale_price, trim = 0.10)

#exclude observations with a sale price of 0 (data description says that these were a transfer of property without an actual sale)
train <- train[ which(train$sale_price!=0), ]

#drop all observations with sale price of less than $1000
train <- train[ which(train$sale_price>=1000), ]

hist(train$lprice,breaks=100)


#fit logistic and stepwise models 
full.model <- lm(lprice ~zip_code+
                   total_units+
                   year_built+
                   sale_price+
                   CD+
                   SchoolDist+
                   HealthArea+
                   LandUse+
                   LotArea+
                   ResArea+
                   GarageArea+
                   OtherArea+
                   NumFloors+
                   LotFront+
                   BldgDepth+
                   AssessLand+
                   ExemptTot+
                   YearAlter2+
                   BuiltFAR+
                   FacilFAR+
                   CondoNo+
                   YCoord+
                   APPBBL+
                   MAPPLUTO_F+
                   neighborhood_BOROUGH_PARK+
                   neighborhood_OCEAN_PARKWAY_SOUTH+
                   neighborhood_CARROLL_GARDENS+
                   neighborhood_WILLIAMSBURG_EAST+
                   neighborhood_MANHATTAN_BEACH+
                   neighborhood_SUNSET_PARK+
                   neighborhood_FLATBUSH_NORTH+
                   neighborhood_FORT_GREENE+
                   neighborhood_FLATBUSH_EAST+
                   neighborhood_COBBLE_HILL+
                   neighborhood_MADISON+
                   neighborhood_SEAGATE+
                   neighborhood_CLINTON_HILL+
                   neighborhood_WILLIAMSBURG_NORTH+
                   neighborhood_DYKER_HEIGHTS+
                   neighborhood_FLATLANDS+
                   neighborhood_OCEAN_PARKWAY_NORTH+
                   neighborhood_NAVY_YARD+
                   neighborhood_COBBLE_HILL_WEST+
                   neighborhood_DOWNTOWN_FULTON_MALL+
                   tax_class_2A+
                   tax_class_2C+
                   building_class_at_sale_B1+
                   building_class_at_sale_A2+
                   building_class_at_sale_A4+
                   building_class_at_sale_S2+
                   building_class_at_sale_S0+
                   residential_units+
                   land_sqft+
                   CT2010+
                   Council+
                   PolicePrct+
                   SanitBoro+
                   Easements+
                   BldgArea+
                   OfficeArea+
                   StrgeArea+
                   AreaSource+
                   UnitsRes+
                   LotDepth+
                   LotType+
                   AssessTot+
                   YearBuilt+
                   ResidFAR+
                   Tract2010+
                   TaxMap+
                   SHAPE_Leng+
                   neighborhood_SHEEPSHEAD_BAY+
                   neighborhood_EAST_NEW_YORK+
                   neighborhood_FLATBUSH_CENTRAL+
                   neighborhood_CYPRESS_HILLS+
                   neighborhood_BEDFORD_STUYVESANT+
                   neighborhood_GRAVESEND+
                   neighborhood_OCEAN_HILL+
                   neighborhood_GERRITSEN_BEACH+
                   neighborhood_WINDSOR_TERRACE+
                   neighborhood_BERGEN_BEACH+
                   neighborhood_GREENPOINT+
                   neighborhood_BRIGHTON_BEACH+
                   neighborhood_KENSINGTON+
                   neighborhood_PARK_SLOPE_SOUTH+
                   neighborhood_GOWANUS+
                   neighborhood_PROSPECT_HEIGHTS+
                   neighborhood_CONEY_ISLAND+
                   neighborhood_WYCKOFF_HEIGHTS+
                   neighborhood_BUSH_TERMINAL+
                   neighborhood_DOWNTOWN_METROTECH+
                   neighborhood_BROOKLYN_UNKNOWN+
                   tax_class_4+
                   tax_class_1B+
                   building_class_at_sale_B9+
                   building_class_at_sale_A1+
                   building_class_at_sale_S1+
                   building_class_at_sale_B3+
                   building_class_at_sale_A3+
                   building_class_at_sale_A0+
                   commercial_units+
                   gross_sqft+
                   year_of_sale+
                   CB2010+
                   ZipCode+
                   HealthCent+
                   SanitDistr+
                   ComArea+
                   RetailArea+
                   FactryArea+
                   NumBldgs+
                   UnitsTotal+
                   BldgFront+
                   ProxCode+
                   BsmtCode+
                   ExemptLand+
                   YearAlter1+
                   CommFAR+
                   BBL+
                   XCoord+
                   PLUTOMapID+
                   SHAPE_Area+
                   neighborhood_PARK_SLOPE+
                   neighborhood_CROWN_HEIGHTS+
                   neighborhood_MIDWOOD+
                   neighborhood_CANARSIE+
                   neighborhood_BAY_RIDGE+
                   neighborhood_BROWNSVILLE+
                   neighborhood_BUSHWICK+
                   neighborhood_BENSONHURST+
                   neighborhood_MARINE_PARK+
                   neighborhood_OLD_MILL_BASIN+
                   neighborhood_MILL_BASIN+
                   neighborhood_WILLIAMSBURG_CENTRAL+
                   neighborhood_WILLIAMSBURG_SOUTH+
                   neighborhood_BATH_BEACH+
                   neighborhood_SPRING_CREEK+
                   neighborhood_FLATBUSH_LEFFERTS_GARDEN+
                   neighborhood_BROOKLYN_HEIGHTS+
                   neighborhood_BOERUM_HILL+
                   neighborhood_RED_HOOK+
                   neighborhood_DOWNTOWN_FULTON_FERRY+
                   tax_class_1+
                   tax_class_2+
                   tax_class_2B+
                   building_class_at_sale_C0+
                   building_class_at_sale_B2+
                   building_class_at_sale_A5+
                   building_class_at_sale_A9+
                   building_class_at_sale_A7+
                   building_class_at_sale_A6+
                   GDP+
                   FEDFUNDS
                 
                 , data = train
                 , na.action=na.omit)

# Stepwise regression model
lm <- stepAIC(full.model, direction = "both", tuneGrid = data.frame(nvmax = 1:7),trace = FALSE)

# step.model$results
saveRDS(full.model, "full.rda")
saveRDS(lm, "lm.rda")

#fit random forest model
#rf<-randomForest(lprice~
#                                                      zip_code+
#                                                      total_units+
#                                                      year_built+
#                                                      CD+
#                                                      SchoolDist+
#                                                      HealthArea+
#                                                      LandUse+
#                                                      LotArea+
#                                                      ResArea+
#                                                      GarageArea+
#                                                      OtherArea+
#                                                      NumFloors+
#                                                      LotFront+
#                                                      BldgDepth+
#                                                      AssessLand+
#                                                      ExemptTot+
#                                                      YearAlter2+
#                                                      BuiltFAR+
#                                                      FacilFAR+
#                                                      CondoNo+
#                                                      YCoord+
#                                                      APPBBL+
#                                                      MAPPLUTO_F+
#                                                      neighborhood_BOROUGH_PARK+
#                                                      neighborhood_OCEAN_PARKWAY_SOUTH+
#                                                      neighborhood_CARROLL_GARDENS+
#                                                      neighborhood_WILLIAMSBURG_EAST+
#                                                      neighborhood_MANHATTAN_BEACH+
#                                                      neighborhood_SUNSET_PARK+
#                                                      neighborhood_FLATBUSH_NORTH+
#                                                      neighborhood_FORT_GREENE+
#                                                      neighborhood_FLATBUSH_EAST+
#                                                      neighborhood_COBBLE_HILL+
#                                                      neighborhood_MADISON+
#                                                      neighborhood_SEAGATE+
#                                                      neighborhood_CLINTON_HILL+
#                                                      neighborhood_WILLIAMSBURG_NORTH+
#                                                      neighborhood_DYKER_HEIGHTS+
#                                                      neighborhood_FLATLANDS+
#                                                      neighborhood_OCEAN_PARKWAY_NORTH+
#                                                      neighborhood_NAVY_YARD+
#                                                      neighborhood_COBBLE_HILL_WEST+
#                                                      neighborhood_DOWNTOWN_FULTON_MALL+
#                                                      tax_class_2A+
#                                                      building_class_at_sale_B1+
#                                                      building_class_at_sale_A2+
#                                                      building_class_at_sale_A4+
#                                                      building_class_at_sale_S2+
#                                                      building_class_at_sale_S0+
#                                                      residential_units+
#                                                      land_sqft+
#                                                      CT2010+
#                                                      Council+
#                                                      PolicePrct+
#                                                      SanitBoro+
#                                                      Easements+
#                                                      BldgArea+
#                                                      OfficeArea+
#                                                      StrgeArea+
#                                                      AreaSource+
#                                                      UnitsRes+
#                                                      LotDepth+
#                                                      LotType+
#                                                      AssessTot+
#                                                      YearBuilt+
#                                                      ResidFAR+
#                                                      Tract2010+
#                                                      TaxMap+
#                                                      SHAPE_Leng+
#                                                      neighborhood_SHEEPSHEAD_BAY+
#                                                      neighborhood_EAST_NEW_YORK+
#                                                      neighborhood_FLATBUSH_CENTRAL+
#                                                      neighborhood_CYPRESS_HILLS+
#                                                      neighborhood_BEDFORD_STUYVESANT+
#                                                      neighborhood_GRAVESEND+
#                                                      neighborhood_OCEAN_HILL+
#                                                      neighborhood_GERRITSEN_BEACH+
#                                                      neighborhood_WINDSOR_TERRACE+
#                                                      neighborhood_BERGEN_BEACH+
#                                                      neighborhood_GREENPOINT+
#                                                      neighborhood_BRIGHTON_BEACH+
#                                                      neighborhood_KENSINGTON+
#                                                      neighborhood_PARK_SLOPE_SOUTH+
#                                                      neighborhood_GOWANUS+
#                                                      neighborhood_PROSPECT_HEIGHTS+
#                                                      neighborhood_CONEY_ISLAND+
#                                                      neighborhood_WYCKOFF_HEIGHTS+
#                                                      neighborhood_BUSH_TERMINAL+
#                                                      neighborhood_DOWNTOWN_METROTECH+
#                                                      neighborhood_BROOKLYN_UNKNOWN+
#                                                      tax_class_4+
#                                                      tax_class_1B+
#                                                      building_class_at_sale_B9+
#                                                      building_class_at_sale_A1+
#                                                      building_class_at_sale_S1+
#                                                      building_class_at_sale_B3+
#                                                      building_class_at_sale_A3+
#                                                      building_class_at_sale_A0+
#                                                      commercial_units+
#                                                      gross_sqft+
#                                                      year_of_sale+
#                                                      CB2010+
#                                                      ZipCode+
#                                                      HealthCent+
#                                                      SanitDistr+
#                                                      ComArea+
#                                                      RetailArea+
#                                                      FactryArea+
#                                                      NumBldgs+
#                                                      UnitsTotal+
#                                                      BldgFront+
#                                                      ProxCode+
#                                                      BsmtCode+
#                                                      ExemptLand+
#                                                      YearAlter1+
#                                                      CommFAR+
#                                                      BBL+
#                                                      XCoord+
#                                                      PLUTOMapID+
#                                                      SHAPE_Area+
#                                                      neighborhood_PARK_SLOPE+
#                                                      neighborhood_CROWN_HEIGHTS+
#                                                      neighborhood_MIDWOOD+
#                                                      neighborhood_CANARSIE+
#                                                      neighborhood_BAY_RIDGE+
#                                                      neighborhood_BROWNSVILLE+
#                                                      neighborhood_BUSHWICK+
#                                                      neighborhood_BENSONHURST+
#                                                      neighborhood_MARINE_PARK+
#                                                      neighborhood_OLD_MILL_BASIN+
#                                                      neighborhood_MILL_BASIN+
#                                                      neighborhood_WILLIAMSBURG_CENTRAL+
#                                                      neighborhood_WILLIAMSBURG_SOUTH+
#                                                      neighborhood_BATH_BEACH+
#                                                      neighborhood_SPRING_CREEK+
#                                                      neighborhood_FLATBUSH_LEFFERTS_GARDEN+
#                                                      neighborhood_BROOKLYN_HEIGHTS+
#                                                      neighborhood_BOERUM_HILL+
#                                                      neighborhood_RED_HOOK+
#                                                      neighborhood_DOWNTOWN_FULTON_FERRY+
#                                                      tax_class_1+
#                                                      tax_class_2+
#                                                      tax_class_2B+
#                                                      building_class_at_sale_C0+
#                                                      building_class_at_sale_B2+
#                                                      building_class_at_sale_A5+
#                                                      building_class_at_sale_A9+
#                                                      building_class_at_sale_A7+
#                                                      building_class_at_sale_A6+
#                    tax_class_2C+
#                    OwnerType_M+
#                    OwnerType_C+
#                    GDP+
#                    FEDFUNDS
#                                                    
#                                                    ,data=train
#                                                    ,na.action=na.omit)
# 
# 
# #save RF model store file
# saveRDS(rf, "RF.rda")



#regularized random forest 
rrf<-RRF(lprice~
                   zip_code+
                   total_units+
                   year_built+
                   CD+
                   SchoolDist+
                   HealthArea+
                   LandUse+
                   LotArea+
                   ResArea+
                   GarageArea+
                   OtherArea+
                   NumFloors+
                   LotFront+
                   BldgDepth+
                   AssessLand+
                   ExemptTot+
                   YearAlter2+
                   BuiltFAR+
                   FacilFAR+
                   CondoNo+
                   YCoord+
                   APPBBL+
                   MAPPLUTO_F+
                   neighborhood_BOROUGH_PARK+
                   neighborhood_OCEAN_PARKWAY_SOUTH+
                   neighborhood_CARROLL_GARDENS+
                   neighborhood_WILLIAMSBURG_EAST+
                   neighborhood_MANHATTAN_BEACH+
                   neighborhood_SUNSET_PARK+
                   neighborhood_FLATBUSH_NORTH+
                   neighborhood_FORT_GREENE+
                   neighborhood_FLATBUSH_EAST+
                   neighborhood_COBBLE_HILL+
                   neighborhood_MADISON+
                   neighborhood_SEAGATE+
                   neighborhood_CLINTON_HILL+
                   neighborhood_WILLIAMSBURG_NORTH+
                   neighborhood_DYKER_HEIGHTS+
                   neighborhood_FLATLANDS+
                   neighborhood_OCEAN_PARKWAY_NORTH+
                   neighborhood_NAVY_YARD+
                   neighborhood_COBBLE_HILL_WEST+
                   neighborhood_DOWNTOWN_FULTON_MALL+
                   tax_class_2A+
                   building_class_at_sale_B1+
                   building_class_at_sale_A2+
                   building_class_at_sale_A4+
                   building_class_at_sale_S2+
                   building_class_at_sale_S0+
                   residential_units+
                   land_sqft+
                   CT2010+
                   Council+
                   PolicePrct+
                   SanitBoro+
                   Easements+
                   BldgArea+
                   OfficeArea+
                   StrgeArea+
                   AreaSource+
                   UnitsRes+
                   LotDepth+
                   LotType+
                   AssessTot+
                   YearBuilt+
                   ResidFAR+
                   Tract2010+
                   TaxMap+
                   SHAPE_Leng+
                   neighborhood_SHEEPSHEAD_BAY+
                   neighborhood_EAST_NEW_YORK+
                   neighborhood_FLATBUSH_CENTRAL+
                   neighborhood_CYPRESS_HILLS+
                   neighborhood_BEDFORD_STUYVESANT+
                   neighborhood_GRAVESEND+
                   neighborhood_OCEAN_HILL+
                   neighborhood_GERRITSEN_BEACH+
                   neighborhood_WINDSOR_TERRACE+
                   neighborhood_BERGEN_BEACH+
                   neighborhood_GREENPOINT+
                   neighborhood_BRIGHTON_BEACH+
                   neighborhood_KENSINGTON+
                   neighborhood_PARK_SLOPE_SOUTH+
                   neighborhood_GOWANUS+
                   neighborhood_PROSPECT_HEIGHTS+
                   neighborhood_CONEY_ISLAND+
                   neighborhood_WYCKOFF_HEIGHTS+
                   neighborhood_BUSH_TERMINAL+
                   neighborhood_DOWNTOWN_METROTECH+
                   neighborhood_BROOKLYN_UNKNOWN+
                   tax_class_4+
                   tax_class_1B+
                   building_class_at_sale_B9+
                   building_class_at_sale_A1+
                   building_class_at_sale_S1+
                   building_class_at_sale_B3+
                   building_class_at_sale_A3+
                   building_class_at_sale_A0+
                   commercial_units+
                   gross_sqft+
                   year_of_sale+
                   CB2010+
                   ZipCode+
                   HealthCent+
                   SanitDistr+
                   ComArea+
                   RetailArea+
                   FactryArea+
                   NumBldgs+
                   UnitsTotal+
                   BldgFront+
                   ProxCode+
                   BsmtCode+
                   ExemptLand+
                   YearAlter1+
                   CommFAR+
                   BBL+
                   XCoord+
                   PLUTOMapID+
                   SHAPE_Area+
                   neighborhood_PARK_SLOPE+
                   neighborhood_CROWN_HEIGHTS+
                   neighborhood_MIDWOOD+
                   neighborhood_CANARSIE+
                   neighborhood_BAY_RIDGE+
                   neighborhood_BROWNSVILLE+
                   neighborhood_BUSHWICK+
                   neighborhood_BENSONHURST+
                   neighborhood_MARINE_PARK+
                   neighborhood_OLD_MILL_BASIN+
                   neighborhood_MILL_BASIN+
                   neighborhood_WILLIAMSBURG_CENTRAL+
                   neighborhood_WILLIAMSBURG_SOUTH+
                   neighborhood_BATH_BEACH+
                   neighborhood_SPRING_CREEK+
                   neighborhood_FLATBUSH_LEFFERTS_GARDEN+
                   neighborhood_BROOKLYN_HEIGHTS+
                   neighborhood_BOERUM_HILL+
                   neighborhood_RED_HOOK+
                   neighborhood_DOWNTOWN_FULTON_FERRY+
                   tax_class_1+
                   tax_class_2+
                   tax_class_2B+
                   building_class_at_sale_C0+
                   building_class_at_sale_B2+
                   building_class_at_sale_A5+
                   building_class_at_sale_A9+
                   building_class_at_sale_A7+
                   building_class_at_sale_A6+
                   tax_class_2C+
                   OwnerType_M+
                   OwnerType_C+
           GDP+
           FEDFUNDS
                 
                 ,data=train
                 ,na.action=na.omit)

#save rrf model store file
saveRDS(rrf, "rrf.rda")

yvars<-c(
  'zip_code',
  'total_units',
  'year_built',
  'CD',
  'SchoolDist',
  'HealthArea',
  'LandUse',
  'LotArea',
  'ResArea',
  'GarageArea',
  'OtherArea',
  'NumFloors',
  'LotFront',
  'BldgDepth',
  'AssessLand',
  'ExemptTot',
  'YearAlter2',
  'BuiltFAR',
  'FacilFAR',
  'CondoNo',
  'YCoord',
  'APPBBL',
  'MAPPLUTO_F',
  'neighborhood_BOROUGH_PARK',
  'neighborhood_OCEAN_PARKWAY_SOUTH',
  'neighborhood_CARROLL_GARDENS',
  'neighborhood_WILLIAMSBURG_EAST',
  'neighborhood_MANHATTAN_BEACH',
  'neighborhood_SUNSET_PARK',
  'neighborhood_FLATBUSH_NORTH',
  'neighborhood_FORT_GREENE',
  'neighborhood_FLATBUSH_EAST',
  'neighborhood_COBBLE_HILL',
  'neighborhood_MADISON',
  'neighborhood_SEAGATE',
  'neighborhood_CLINTON_HILL',
  'neighborhood_WILLIAMSBURG_NORTH',
  'neighborhood_DYKER_HEIGHTS',
  'neighborhood_FLATLANDS',
  'neighborhood_OCEAN_PARKWAY_NORTH',
  'neighborhood_NAVY_YARD',
  'neighborhood_COBBLE_HILL_WEST',
  'neighborhood_DOWNTOWN_FULTON_MALL',
  'tax_class_2A',
  'building_class_at_sale_B1',
  'building_class_at_sale_A2',
  'building_class_at_sale_A4',
  'building_class_at_sale_S2',
  'building_class_at_sale_S0',
  'residential_units',
  'land_sqft',
  'CT2010',
  'Council',
  'PolicePrct',
  'SanitBoro',
  'Easements',
  'BldgArea',
  'OfficeArea',
  'StrgeArea',
  'AreaSource',
  'UnitsRes',
  'LotDepth',
  'LotType',
  'AssessTot',
  'YearBuilt',
  'ResidFAR',
  'Tract2010',
  'TaxMap',
  'SHAPE_Leng',
  'neighborhood_SHEEPSHEAD_BAY',
  'neighborhood_EAST_NEW_YORK',
  'neighborhood_FLATBUSH_CENTRAL',
  'neighborhood_CYPRESS_HILLS',
  'neighborhood_BEDFORD_STUYVESANT',
  'neighborhood_GRAVESEND',
  'neighborhood_OCEAN_HILL',
  'neighborhood_GERRITSEN_BEACH',
  'neighborhood_WINDSOR_TERRACE',
  'neighborhood_BERGEN_BEACH',
  'neighborhood_GREENPOINT',
  'neighborhood_BRIGHTON_BEACH',
  'neighborhood_KENSINGTON',
  'neighborhood_PARK_SLOPE_SOUTH',
  'neighborhood_GOWANUS',
  'neighborhood_PROSPECT_HEIGHTS',
  'neighborhood_CONEY_ISLAND',
  'neighborhood_WYCKOFF_HEIGHTS',
  'neighborhood_BUSH_TERMINAL',
  'neighborhood_DOWNTOWN_METROTECH',
  'neighborhood_BROOKLYN_UNKNOWN',
  'tax_class_4',
  'tax_class_1B',
  'building_class_at_sale_B9',
  'building_class_at_sale_A1',
  'building_class_at_sale_S1',
  'building_class_at_sale_B3',
  'building_class_at_sale_A3',
  'building_class_at_sale_A0',
  'commercial_units',
  'gross_sqft',
  'year_of_sale',
  'CB2010',
  'ZipCode',
  'HealthCent',
  'SanitDistr',
  'ComArea',
  'RetailArea',
  'FactryArea',
  'NumBldgs',
  'UnitsTotal',
  'BldgFront',
  'ProxCode',
  'BsmtCode',
  'ExemptLand',
  'YearAlter1',
  'CommFAR',
  'BBL',
  'XCoord',
  'PLUTOMapID',
  'SHAPE_Area',
  'neighborhood_PARK_SLOPE',
  'neighborhood_CROWN_HEIGHTS',
  'neighborhood_MIDWOOD',
  'neighborhood_CANARSIE',
  'neighborhood_BAY_RIDGE',
  'neighborhood_BROWNSVILLE',
  'neighborhood_BUSHWICK',
  'neighborhood_BENSONHURST',
  'neighborhood_MARINE_PARK',
  'neighborhood_OLD_MILL_BASIN',
  'neighborhood_MILL_BASIN',
  'neighborhood_WILLIAMSBURG_CENTRAL',
  'neighborhood_WILLIAMSBURG_SOUTH',
  'neighborhood_BATH_BEACH',
  'neighborhood_SPRING_CREEK',
  'neighborhood_FLATBUSH_LEFFERTS_GARDEN',
  'neighborhood_BROOKLYN_HEIGHTS',
  'neighborhood_BOERUM_HILL',
  'neighborhood_RED_HOOK',
  'neighborhood_DOWNTOWN_FULTON_FERRY',
  'tax_class_1',
  'tax_class_2',
  'tax_class_2B',
  'building_class_at_sale_C0',
  'building_class_at_sale_B2',
  'building_class_at_sale_A5',
  'building_class_at_sale_A9',
  'building_class_at_sale_A7',
  'building_class_at_sale_A6',
  'tax_class_2C',
  'OwnerType_M',
  'OwnerType_C',
  'GDP',
  'FEDFUNDS'
  
)
trainy<-train[yvars]


#cross validated RF model
rrfcv<-rrfcv(trainy,train$lprice
             ,na.action=na.omit
             ,cv.fold=5)

saveRDS(rrfcv, "rrfcv.rda")

#fit support vector machine model
modelsvm <- svm(lprice~
                  zip_code+
                  total_units+
                  year_built+
                  CD+
                  SchoolDist+
                  HealthArea+
                  LandUse+
                  LotArea+
                  ResArea+
                  GarageArea+
                  OtherArea+
                  NumFloors+
                  LotFront+
                  BldgDepth+
                  AssessLand+
                  ExemptTot+
                  YearAlter2+
                  BuiltFAR+
                  FacilFAR+
                  # CondoNo+
                  YCoord+
                  APPBBL+
                  MAPPLUTO_F+
                  neighborhood_BOROUGH_PARK+
                  neighborhood_OCEAN_PARKWAY_SOUTH+
                  neighborhood_CARROLL_GARDENS+
                  neighborhood_WILLIAMSBURG_EAST+
                  neighborhood_MANHATTAN_BEACH+
                  neighborhood_SUNSET_PARK+
                  neighborhood_FLATBUSH_NORTH+
                  neighborhood_FORT_GREENE+
                  neighborhood_FLATBUSH_EAST+
                  neighborhood_COBBLE_HILL+
                  neighborhood_MADISON+
                  neighborhood_SEAGATE+
                  neighborhood_CLINTON_HILL+
                  neighborhood_WILLIAMSBURG_NORTH+
                  neighborhood_DYKER_HEIGHTS+
                  neighborhood_FLATLANDS+
                  neighborhood_OCEAN_PARKWAY_NORTH+
                  neighborhood_NAVY_YARD+
                  neighborhood_COBBLE_HILL_WEST+
                  neighborhood_DOWNTOWN_FULTON_MALL+
                  tax_class_2A+
                  building_class_at_sale_B1+
                  building_class_at_sale_A2+
                  building_class_at_sale_A4+
                  building_class_at_sale_S2+
                  building_class_at_sale_S0+
                  residential_units+
                  land_sqft+
                  CT2010+
                  Council+
                  PolicePrct+
                  SanitBoro+
                  Easements+
                  BldgArea+
                  OfficeArea+
                  StrgeArea+
                  AreaSource+
                  UnitsRes+
                  LotDepth+
                  LotType+
                  AssessTot+
                  YearBuilt+
                  ResidFAR+
                  Tract2010+
                  TaxMap+
                  SHAPE_Leng+
                  neighborhood_SHEEPSHEAD_BAY+
                  neighborhood_EAST_NEW_YORK+
                  neighborhood_FLATBUSH_CENTRAL+
                  neighborhood_CYPRESS_HILLS+
                  neighborhood_BEDFORD_STUYVESANT+
                  neighborhood_GRAVESEND+
                  neighborhood_OCEAN_HILL+
                  neighborhood_GERRITSEN_BEACH+
                  neighborhood_WINDSOR_TERRACE+
                  neighborhood_BERGEN_BEACH+
                  neighborhood_GREENPOINT+
                  neighborhood_BRIGHTON_BEACH+
                  neighborhood_KENSINGTON+
                  neighborhood_PARK_SLOPE_SOUTH+
                  neighborhood_GOWANUS+
                  neighborhood_PROSPECT_HEIGHTS+
                  neighborhood_CONEY_ISLAND+
                  neighborhood_WYCKOFF_HEIGHTS+
                  neighborhood_BUSH_TERMINAL+
                  neighborhood_DOWNTOWN_METROTECH+
                  # neighborhood_BROOKLYN_UNKNOWN+
                  tax_class_4+
                  tax_class_1B+
                  building_class_at_sale_B9+
                  building_class_at_sale_A1+
                  building_class_at_sale_S1+
                  building_class_at_sale_B3+
                  building_class_at_sale_A3+
                  building_class_at_sale_A0+
                  commercial_units+
                  gross_sqft+
                  year_of_sale+
                  CB2010+
                  ZipCode+
                  HealthCent+
                  SanitDistr+
                  ComArea+
                  RetailArea+
                  FactryArea+
                  NumBldgs+
                  UnitsTotal+
                  BldgFront+
                  ProxCode+
                  BsmtCode+
                  ExemptLand+
                  YearAlter1+
                  CommFAR+
                  BBL+
                  XCoord+
                  PLUTOMapID+
                  SHAPE_Area+
                  neighborhood_PARK_SLOPE+
                  neighborhood_CROWN_HEIGHTS+
                  neighborhood_MIDWOOD+
                  neighborhood_CANARSIE+
                  neighborhood_BAY_RIDGE+
                  neighborhood_BROWNSVILLE+
                  neighborhood_BUSHWICK+
                  neighborhood_BENSONHURST+
                  neighborhood_MARINE_PARK+
                  neighborhood_OLD_MILL_BASIN+
                  neighborhood_MILL_BASIN+
                  neighborhood_WILLIAMSBURG_CENTRAL+
                  neighborhood_WILLIAMSBURG_SOUTH+
                  neighborhood_BATH_BEACH+
                  neighborhood_SPRING_CREEK+
                  neighborhood_FLATBUSH_LEFFERTS_GARDEN+
                  neighborhood_BROOKLYN_HEIGHTS+
                  neighborhood_BOERUM_HILL+
                  neighborhood_RED_HOOK+
                  neighborhood_DOWNTOWN_FULTON_FERRY+
                  tax_class_1+
                  tax_class_2+
                  tax_class_2B+
                  building_class_at_sale_C0+
                  building_class_at_sale_B2+
                  building_class_at_sale_A5+
                  building_class_at_sale_A9+
                  building_class_at_sale_A7+
                  building_class_at_sale_A6+
                  tax_class_2C+
                  # OwnerType_M+
                  OwnerType_C+
                  GDP+
                  FEDFUNDS
                
                ,
                data=train,
                na.action =na.omit)

#save SVM model store file
saveRDS(modelsvm, "svm.rda")




# test$tax_class_2C<-0
# test$lprice<-0
# test$OwnerType_M<-0
# test$OwnerType_C<-0
# 
# test$tax_class_2C<-as.integer(test$tax_class_2C)
# test$OwnerType_M<-as.integer(test$OwnerType_M)
# test$OwnerType_C<-as.integer(test$OwnerType_C)

#change NA values in numeric variables to 0
# continuous_vars <- c("total_units",
#             "LotArea",
#             "ResArea",
#             "GarageArea",
#             "OtherArea",
#             "NumFloors",
#             "LotFront",
#             "BldgDepth",
#             "YearAlter2",
#             "BuiltFAR",
#             "FacilFAR",
#             "SHAPE_Leng",
#             "land_sqft",
#             "BldgArea",
#             "OfficeArea",
#             "StrgeArea",
#             "LotDepth",
#             "XCoord",
#             "SHAPE_Area",
#             "gross_sqft",
#             "BldgFront",
#             "YCoord",
#             "lprice"
# )

# for (i in continuous_vars) {
#   test$i<-as.numeric(test$i)
#   test$i[is.na(test$i)]<-mean(test$i)
# }

# categorical_vars<-c(
#   
#   "X__1",
#   "building_class_category",
#   "zip_code",
#   "year_built",
#   "sale_price",
#   "CD",
#   "SchoolDist",
#   "FireComp",
#   "HealthArea",
#   "ZoneDist1",
#   "ZoneDist4",
#   "SPDist1",
#   "LtdHeight",
#   "LandUse",
#   "IrrLotCode",
#   "AssessLand",
#   "ExemptTot",
#   "Tract2010",
#   "ZoneMap",
#   "TaxMap",
#   "APPDate",
#   "PFIRM15_FL",
#   "neighborhood_GOWANUS",
#   "neighborhood_BROOKLYN_HEIGHTS",
#   "neighborhood_BOERUM_HILL",
#   "neighborhood_FORT_GREENE",
#   "neighborhood_CROWN_HEIGHTS",
#   "neighborhood_PROSPECT_HEIGHTS",
#   "neighborhood_COBBLE_HILL_WEST",
#   "neighborhood_BOROUGH_PARK",
#   "neighborhood_WILLIAMSBURG_SOUTH",
#   "neighborhood_WILLIAMSBURG_EAST",
#   "neighborhood_SUNSET_PARK",
#   "neighborhood_FLATBUSH_LEFFERTS_GARDEN",
#   "neighborhood_NAVY_YARD",
#   "neighborhood_BATH_BEACH",
#   "neighborhood_SHEEPSHEAD_BAY",
#   "neighborhood_MARINE_PARK",
#   "neighborhood_BERGEN_BEACH",
#   "neighborhood_BUSH_TERMINAL",
#   "neighborhood_OLD_MILL_BASIN",
#   "neighborhood_FLATLANDS",
#   "neighborhood_SPRING_CREEK",
#   "tax_class_NA",
#   "tax_class_2B",
#   "building_class_at_sale_A9",
#   "building_class_at_sale_A5",
#   "building_class_at_sale_C0",
#   "building_class_at_sale_A1",
#   "building_class_at_sale_A2",
#   "building_class_at_sale_A6",
#   "OwnerType_X",
#   "OwnerType_M",
#   "borough",
#   "tax_class",
#   "residential_units",
#   "tax_class_at_sale",
#   "sale_date",
#   "CT2010",
#   "Council",
#   "PolicePrct",
#   "SanitBoro",
#   "ZoneDist2",
#   "Overlay1",
#   "SPDist2",
#   "SplitZone",
#   "Easements",
#   "AreaSource",
#   "UnitsRes",
#   "Ext",
#   "LotType",
#   "AssessTot",
#   "YearBuilt",
#   "HistDist",
#   "ResidFAR",
#   "BBL",
#   "ZMCode",
#   "EDesigNum",
#   "PLUTOMapID",
#   "Version",
#   "neighborhood_PARK_SLOPE",
#   "neighborhood_OCEAN_PARKWAY_SOUTH",
#   "neighborhood_COBBLE_HILL",
#   "neighborhood_GRAVESEND",
#   "neighborhood_BAY_RIDGE",
#   "neighborhood_MANHATTAN_BEACH",
#   "neighborhood_CARROLL_GARDENS",
#   "neighborhood_MIDWOOD",
#   "neighborhood_DYKER_HEIGHTS",
#   "neighborhood_PARK_SLOPE_SOUTH",
#   "neighborhood_DOWNTOWN_FULTON_FERRY",
#   "neighborhood_WINDSOR_TERRACE",
#   "neighborhood_MILL_BASIN",
#   "neighborhood_MADISON",
#   "neighborhood_WYCKOFF_HEIGHTS",
#   "neighborhood_OCEAN_HILL",
#   "neighborhood_EAST_NEW_YORK",
#   "neighborhood_SEAGATE",
#   "neighborhood_BROWNSVILLE",
#   "neighborhood_GERRITSEN_BEACH",
#   "tax_class_2",
#   "tax_class_1B",
#   "tax_class_2A",
#   "building_class_at_sale_B1",
#   "building_class_at_sale_A4",
#   "building_class_at_sale_B3",
#   "building_class_at_sale_B2",
#   "building_class_at_sale_S0",
#   "OwnerType_P",
#   "OwnerType_O",
#   "OwnerType_C",
#   "neighborhood",
#   "easement",
#   "commercial_units",
#   "building_class_at_sale",
#   "year_of_sale",
#   "CB2010",
#   "ZipCode",
#   "HealthCent",
#   "SanitDistr",
#   "ZoneDist3",
#   "Overlay2",
#   "SPDist3",
#   "BldgClass",
#   "OwnerType",
#   "ComArea",
#   "RetailArea",
#   "FactryArea",
#   "NumBldgs",
#   "UnitsTotal",
#   "ProxCode",
#   "BsmtCode",
#   "ExemptLand",
#   "YearAlter1",
#   "Landmark",
#   "CommFAR",
#   "CondoNo",
#   "Sanborn",
#   "APPBBL",
#   "FIRM07_FLA",
#   "MAPPLUTO_F",
#   "neighborhood_WILLIAMSBURG_NORTH",
#   "neighborhood_RED_HOOK",
#   "neighborhood_FLATBUSH_EAST",
#   "neighborhood_OCEAN_PARKWAY_NORTH",
#   "neighborhood_GREENPOINT",
#   "neighborhood_DOWNTOWN_FULTON_MALL",
#   "neighborhood_FLATBUSH_CENTRAL",
#   "neighborhood_CLINTON_HILL",
#   "neighborhood_BEDFORD_STUYVESANT",
#   "neighborhood_BENSONHURST",
#   "neighborhood_KENSINGTON",
#   "neighborhood_BRIGHTON_BEACH",
#   "neighborhood_WILLIAMSBURG_CENTRAL",
#   "neighborhood_BUSHWICK",
#   "neighborhood_CONEY_ISLAND",
#   "neighborhood_DOWNTOWN_METROTECH",
#   "neighborhood_FLATBUSH_NORTH",
#   "neighborhood_CYPRESS_HILLS",
#   "neighborhood_CANARSIE",
#   "neighborhood_BROOKLYN_UNKNOWN",
#   "tax_class_1",
#   "tax_class_4",
#   "building_class_at_sale_S2",
#   "building_class_at_sale_A3",
#   "building_class_at_sale_S1",
#   "building_class_at_sale_B9",
#   "building_class_at_sale_A7",
#   "building_class_at_sale_A0",
#   "OwnerType_NA",
#   "tax_class_2C",
#   "train"
# )
# 
# for (i in categorical_vars){
#   data$i<-as.factor(data$i)
# }

###################to test your model, swap out "rf" for the name of your model######################
#read in model data from store file
rf<-readRDS('rf.rda')
modelsvm<-readRDS('svm.rda')
lm<-readRDS('lm.rda')
rrf<-readRDS("C:/Users/nyoungblood/Documents/rrf.rda")
full<-readRDS("C:/Users/nyoungblood/Documents/BK HPI/full.rda")

# predict sale prices of test dataset from stored model
newtest <- na.omit(test)
preds<-data.frame(predict(lm,newdata=newtest, na.action = na.omit))
diag<-cbind(newtest,preds)
# diag$fit<-diag$predict.rrf..newdata...test..na.action...na.omit.
diag$fit<-diag$predict.lm..newdata...newtest..na.action...na.omit.

#compute mean absolute percent error
diag$APE<-(abs(diag$lprice-diag$fit)/diag$lprice)
MAPE<-mean(diag$APE*(is.finite(diag$APE)),na.rm=TRUE)
MAPE

#compute RMSPE
diag$SPE<-((diag$lprice-diag$fit)/diag$lprice)^2
RMSPE<-sqrt(mean(diag$SPE*(is.finite(diag$SPE)),na.rm=TRUE))
RMSPE




#show histograms of actual and predicted values
hist(diag$lprice,breaks=100,col='blue', xlab = "Log of sale price", add=T)
hist(diag$fit,breaks=100,col='green', add=T)
legend("topleft", c("Actual", "Predicted"), col=c("blue", "green"), lwd=10)

explain_forest(rrf, interactions = TRUE, data = test)
varImpPlot(rrf)

#produce sample tree for explanation
tree1<-tree(lprice~
              zip_code+
              total_units+
              year_built+
              CD+
              SchoolDist+
              HealthArea+
              LandUse+
              LotArea+
              ResArea+
              GarageArea+
              OtherArea+
              NumFloors+
              LotFront+
              BldgDepth+
              AssessLand+
              ExemptTot+
              YearAlter2+
              BuiltFAR+
              FacilFAR+
              CondoNo+
              YCoord+
              APPBBL+
              MAPPLUTO_F+
              neighborhood_BOROUGH_PARK+
              neighborhood_OCEAN_PARKWAY_SOUTH+
              neighborhood_CARROLL_GARDENS+
              neighborhood_WILLIAMSBURG_EAST+
              neighborhood_MANHATTAN_BEACH+
              neighborhood_SUNSET_PARK+
              neighborhood_FLATBUSH_NORTH+
              neighborhood_FORT_GREENE+
              neighborhood_FLATBUSH_EAST+
              neighborhood_COBBLE_HILL+
              neighborhood_MADISON+
              neighborhood_SEAGATE+
              neighborhood_CLINTON_HILL+
              neighborhood_WILLIAMSBURG_NORTH+
              neighborhood_DYKER_HEIGHTS+
              neighborhood_FLATLANDS+
              neighborhood_OCEAN_PARKWAY_NORTH+
              neighborhood_NAVY_YARD+
              neighborhood_COBBLE_HILL_WEST+
              neighborhood_DOWNTOWN_FULTON_MALL+
              tax_class_2A+
              building_class_at_sale_B1+
              building_class_at_sale_A2+
              building_class_at_sale_A4+
              building_class_at_sale_S2+
              building_class_at_sale_S0+
              residential_units+
              land_sqft+
              CT2010+
              Council+
              PolicePrct+
              SanitBoro+
              Easements+
              BldgArea+
              OfficeArea+
              StrgeArea+
              AreaSource+
              UnitsRes+
              LotDepth+
              LotType+
              AssessTot+
              YearBuilt+
              ResidFAR+
              Tract2010+
              TaxMap+
              SHAPE_Leng+
              neighborhood_SHEEPSHEAD_BAY+
              neighborhood_EAST_NEW_YORK+
              neighborhood_FLATBUSH_CENTRAL+
              neighborhood_CYPRESS_HILLS+
              neighborhood_BEDFORD_STUYVESANT+
              neighborhood_GRAVESEND+
              neighborhood_OCEAN_HILL+
              neighborhood_GERRITSEN_BEACH+
              neighborhood_WINDSOR_TERRACE+
              neighborhood_BERGEN_BEACH+
              neighborhood_GREENPOINT+
              neighborhood_BRIGHTON_BEACH+
              neighborhood_KENSINGTON+
              neighborhood_PARK_SLOPE_SOUTH+
              neighborhood_GOWANUS+
              neighborhood_PROSPECT_HEIGHTS+
              neighborhood_CONEY_ISLAND+
              neighborhood_WYCKOFF_HEIGHTS+
              neighborhood_BUSH_TERMINAL+
              neighborhood_DOWNTOWN_METROTECH+
              neighborhood_BROOKLYN_UNKNOWN+
              tax_class_4+
              tax_class_1B+
              building_class_at_sale_B9+
              building_class_at_sale_A1+
              building_class_at_sale_S1+
              building_class_at_sale_B3+
              building_class_at_sale_A3+
              building_class_at_sale_A0+
              commercial_units+
              gross_sqft+
              year_of_sale+
              CB2010+
              ZipCode+
              HealthCent+
              SanitDistr+
              ComArea+
              RetailArea+
              FactryArea+
              NumBldgs+
              UnitsTotal+
              BldgFront+
              ProxCode+
              BsmtCode+
              ExemptLand+
              YearAlter1+
              CommFAR+
              BBL+
              XCoord+
              PLUTOMapID+
              SHAPE_Area+
              neighborhood_PARK_SLOPE+
              neighborhood_CROWN_HEIGHTS+
              neighborhood_MIDWOOD+
              neighborhood_CANARSIE+
              neighborhood_BAY_RIDGE+
              neighborhood_BROWNSVILLE+
              neighborhood_BUSHWICK+
              neighborhood_BENSONHURST+
              neighborhood_MARINE_PARK+
              neighborhood_OLD_MILL_BASIN+
              neighborhood_MILL_BASIN+
              neighborhood_WILLIAMSBURG_CENTRAL+
              neighborhood_WILLIAMSBURG_SOUTH+
              neighborhood_BATH_BEACH+
              neighborhood_SPRING_CREEK+
              neighborhood_FLATBUSH_LEFFERTS_GARDEN+
              neighborhood_BROOKLYN_HEIGHTS+
              neighborhood_BOERUM_HILL+
              neighborhood_RED_HOOK+
              neighborhood_DOWNTOWN_FULTON_FERRY+
              tax_class_1+
              tax_class_2+
              tax_class_2B+
              building_class_at_sale_C0+
              building_class_at_sale_B2+
              building_class_at_sale_A5+
              building_class_at_sale_A9+
              building_class_at_sale_A7+
              building_class_at_sale_A6+
              tax_class_2C+
              OwnerType_M+
              OwnerType_C+
              GDP+
              FEDFUNDS
            
            ,data=train
            ,na.action=na.omit)

#SVM diagnostics--only works with 1 predictor
plot(modelsvm,newtest)
