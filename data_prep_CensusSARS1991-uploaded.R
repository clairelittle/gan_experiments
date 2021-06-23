###########################################################################################################################################
## June 2021
## r version 4.1.0, rstudio version 1.4.1106
##
## Looking at the 1991 Census SARS data to use as a test set for generating synthetic data
## Data downloaded from UK Data Service 19/05/21
##
###########################################################################################################################################


############ Load data

## set working directory - e.g. where data is stored
setwd('E:/Data/1991CensusSARS-UKDS-210519-7210tab_4d6e5df1ed7be3618b2e1f9b9a0cd5ea/UKDA-7210-tab/tab')

## read in original data - it is a tab delimited file
data <- read.delim('gb91ind.tab', sep = '\t')

## summary of the data
summary(data)
# 1116181 records
# 67 variables



############ Data Prep
#### Select a subset of columns, and also subset on Manchester and North West 

# It is unlikely we'd need to synthesize all attributes as some are dependent upon others

# Select attributes which provide a good representation of the data (e.g. age, sex, ethnic group,...), and include 
# area and region in order to subset on those

# AREAP, REGIONP, AGE, COBIRTH, ECONPRIM, ETHGROUP, MSTATUS, QUALNUM, SEX, SOCLASS, HHSPTYPE, TENURE
data_sub <- data[,c(1:3,6,9,11,17,20,25,26,38,40)]
summary(data_sub)


# change country of birth to 'UK' or 'other', as there are a lot of categories
# denote it as a derived attribute and keep the original
data_sub$COBIRTH_dv[data_sub$COBIRTH <= 5] <- 0 # born UK
data_sub$COBIRTH_dv[data_sub$COBIRTH > 5] <- 1 # not born UK
data_sub$COBIRTH_dv <- factor(data_sub$COBIRTH_dv, levels=c(0,1), labels=c('UK', 'other'))
table(data_sub$COBIRTH_dv)

# make sure the original is set as a factor
data_sub$COBIRTH <- factor(data_sub$COBIRTH)



# Here I have inputted the actual labels for each variable, as it is easier to see when demonstrating the data
# It is completely unnecessary to do this! 
# But do need to make sure that categorical variables are set to factors, and numerical to int

data_sub$AREAP <- factor(data_sub$AREAP) # don't set labels as there are too many

data_sub$REGIONP <- factor(data_sub$REGIONP,labels=c('North', 'Yorks Humb', 'E Midlands', 'E Anglia', 'Inner Lon', 'OUter Lon', 
                                                           'Rest S.East', 'S West', 'W Midlands', 'N West', 'Wales', 'Scotland'))

data_sub$ECONPRIM <- factor(data_sub$ECONPRIM,labels=c('Emp FT', 'Emp PT', 'Self-emp w', 'Self-emp wo', 'Gov scheme', 'Unemp', 
                                                       'Student', 'Perm sick', 'Ret', 'Other'))

data_sub$ETHGROUP <- factor(data_sub$ETHGROUP, labels=c('White','Black Car', 'Black Afr', 'Black Oth', 'Indian', 'Pakistani',
                                                        'Bangladeshi', 'Chinese', 'Other Asian', 'Other'))

data_sub$MSTATUS <- factor(data_sub$MSTATUS, labels=c('Single','Married','Remarried','Divorced','Widowed'))

data_sub$QUALNUM <- factor(data_sub$QUALNUM, labels=c('0','1','2+'))

data_sub$SEX <- factor(data_sub$SEX, labels=c('Male','Female'))

data_sub$SOCLASS <- factor(data_sub$SOCLASS, labels=c('I Prof', 'II Manag Tech', 'IIIN Skilled', 'IIIM Skilled','IV Part Skill',
                                                      'V Unskill', 'Armed Forces', 'Inad descr', 'Not stated'))

data_sub$HHSPTYPE <- factor(data_sub$HHSPTYPE, labels=c('Detached', 'Semi-det','Terraced','Flat-res','Flat-comm','Conv Flat',
                                                        'Conv Flatlet','Not SC flat','Not SC rooms','Not SC bedsit','Oth NotSC flat',
                                                        'Oth NotSC rooms','Oth NotSC bedsit', 'Non perm acc'))

data_sub$TENURE <- factor(data_sub$TENURE, labels=c('Own-occ outr','Own-occ buy','Rent priv furn','Rent priv unf','Rent job',
                                                    'Rent HsAssoc', 'Rent LA','Rent LA Scot', 'Rent NT Scot', 'Rent Scot Hmes'))

summary(data_sub)


##### save datasets, include derived country of birth and original country of birth attribute, so we can choose which we want to use:
## Manchester
data_man <- subset(data_sub, AREAP == 35) # 8173 records
# no longer need areap, or region
data_man <- data_man[,c(3,5:13,4)] # order so the country of birth attributes are next to each other
summary(data_man) 
write.csv(data_man, 'manchester_subset.csv', row.names=FALSE)
str(data_man)

## North West
data_nw <- subset(data_sub, REGIONP == 'N West') # 126251
# no longer need region, for consistency remove areap too (although this does have values)
data_nw <- data_nw[,c(3,5:13,4)]
summary(data_nw) 
write.csv(data_nw, 'northwest_subset.csv', row.names=FALSE)




################################################# Synthpop
# create a synthetic Manchester and North West

###################### Manchester
library(synthpop)

# look at the Manchester data using synthpop
codebook.syn(data_man)$tab 
# age is an integer, and all others are factors, as expected

set.seed(123) # so results can be reproduced
# use default settings, and use the derived country of birth variable to start with
manchester_syn <- syn(data_man[,c(1:10)])
# took a few seconds to run


# look at a summary
summary(manchester_syn)
# compare to original data
compare(manchester_syn, data_man, stat = "counts")

# Save synthetic data
write.syn(manchester_syn, filename = "Synthetic_manchester", filetype = "csv")



#######
# Try this again using the original country of birth attribute instead of derived

set.seed(123) # so results can be reproduced
# use default settings, and use the original country of birth variable
manchester_syn1 <- syn(data_man[,c(1:9,11)])
# took a few seconds to run

# look at a summary
summary(manchester_syn1)
# compare to original data
compare(manchester_syn1, data_man, stat = "counts")

# Save synthetic data
write.syn(manchester_syn1, filename = "Synthetic_manchester1", filetype = "csv")



###################### North West

# look at the North West data using synthpop
codebook.syn(data_nw)$tab 
# age is an integer, and all others are factors, as expected

set.seed(123) # so results can be reproduced
# use default settings, and use the derived country of birth variable to start with
nw_syn <- syn(data_nw[,c(1:10)])
# took < minute to run


# look at a summary
summary(nw_syn)
# compare to original data
compare(nw_syn, data_nw, stat = "counts")

# Save synthetic data
write.syn(nw_syn, filename = "Synthetic_NorthWest", filetype = "csv")



#######
# Try this again using the original country of birth attribute instead of derived

set.seed(123) # so results can be reproduced
# use default settings, and use the original country of birth variable
nw_syn1 <- syn(data_nw[,c(1:9,11)])
# took < minute to run

# look at a summary
summary(nw_syn1)
# compare to original data
compare(nw_syn1, data_nw, stat = "counts")

# Save synthetic data
write.syn(nw_syn1, filename = "Synthetic_NorthWest1", filetype = "csv")
