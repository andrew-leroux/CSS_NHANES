rm(list=ls())
## The code below is divided into the following "chunks"
##  - Section 0: Install and load all necessary packages including the rnhanesdata package
##               which contains the accelerometry data used in our analysis. 
##               Load relevant pre-processed data in the rnhanesdata package.
##               Also includes some set-up (defining directories to download data/save figures, etc.).
##  - Section 1: Data processing 
##              1a. Download and merge additional data from the CDC website
##              1b: Prep accelerometry data for analysis and merge all data
##              1c. Process new variables of interest which were downloaded in 1a
##                  * Self reported overall health status
##                  * number of bad mental days in the last 30 days
##                  * poverty-to-income ratio
##                  * employment status
##              1d. Some exploratory plots looking at estimated wear-time protocol compliance
##              1e. Apply accelerometry exclusion criteria based on whether they have sufficient "good" accelerometry data
##                  and calculate some features of interest
##                  * Exclude indviduals with data quality flags (calibration, NHANES derived flag)
##                  * Exclude days with less than 10 hour of estimated wear time
##                  * Exclude indviduals with fewer than 3 days with >= 10 hours of wear time
##                  * Calculate:
##                       - Total activity counts (TAC)
##                       - Total log activity counts (TLAC)
##                       - Average profiles
##  - Section 2: Exploratory data analysis
##               Note that we do not account for survey weights in our EDA (plots, summary statistics), 
##               though it is possible to do so using functions from the survey package. Complex survey 
##               analyses are beyond the scope of this talk. 
##  - Section 2: Data analysis
##              2a. Function-on-function regression
##                  * Model physical activity patterns as a function of age in the population
##              2b. Function-on-function regression
##                  * Model 5-year mortality as a function of activity via FGLM
##                  * Model 5-year mortality as a function of activity via FGAM




########################################
##                                    ##
##  Section 0: load required packages ##
##                                    ##
########################################

## Check for packages needed to run analyses/install the rnhanesdata package.
## Note: all these packages are available on CRAN and can therefore be downloaded using the install.packages() function,
##       the rnhanesdata package is not on CRAN due to package size
pckgs <- c("gridExtra",                       ## package for plotting >1 ggplot objects in a single figure
           "tidyverse",                       ## package(s) for data manipulation/plotting
           "fields",                          ## package for making heatmaps
           "devtools",                        ## package used to download R packages stored on GitHub
           "mgcv","refund",                   ## packages used for smoothing/functional regression
           "survey"                           ## package used for analyzing (complex) survey data
           )

sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
    install.packages(x)
    require(x, character.only=TRUE)
})
rm(list=c("pckgs"))

## Install the rnhanesdata package and dependencies.
## This may take a few minutes because of the size of the data package.
if(!require("rnhanesdata")){
    install_github("andrew-leroux/rnhanesdata")
    require("rnhanesdata")
}

## set up directory paths
code_path    <- file.path(".","code")    ## path to supplemental (figure) code
figure_path  <- file.path(".","figures") ## path to save figures
data_path    <- file.path(".","data")    ## path to save data
make_plots   <- TRUE                     ## change to FALSE if you don't want to create figures

## Load data pre-processed in the rnhanesdata package
## Note the naming convention _* denotes NHANES wave. 
##      _C = 2003-2004 
##      _D = 2005-2006
data("PAXINTEN_C");data("PAXINTEN_D")    ## activity count data matrices
data("Flags_C");data("Flags_D")          ## wear/non-wear flag data matrices
data("Covariate_C");data("Covariate_D")  ## demographic/comorbidity data matrices


## set up some theme options for plotting done later
textsize <- 24
theme_set(theme_bw(base_size=textsize) + 
          theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                plot.title = element_text(hjust = 0.5))
          )




###########################################################################
##                                                                       ##
##  Section 1a: Download and merge additional data from the CDC website  ##
##                                                                       ##
###########################################################################


## We are interested in demographic data not pre-processed in the Covariate_C/Covariate_D data files. 
## However, the rnhanesdata package includes a few key "raw" NHANES data files, including the demographic data. 
## We can either download these data from the CDC ourselves (example using employment data below), or we can 
## use the process_covar function to extract the variables we're interested in.
## For the purposes of this, we download demographic, occupational, and health status questionairres from the 
## CDC and merge them with data pre-processed in package.
files_external_C <- c("DEMO_C.XPT", "OCQ_C.XPT", "HSQ_C.XPT")
files_external_D <- c("DEMO_D.XPT", "OCQ_D.XPT", "HSQ_D.XPT")
files_download_C <- files_external_C[!file.exists(file.path(data_path, files_external_C))]
files_download_D <- files_external_D[!file.exists(file.path(data_path, files_external_D))]
for(k in seq_along(files_download_C)){
    download.file(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/", files_download_C[k]), 
                  destfile=file.path(data_path, files_download_C[k]))
}
for(k in seq_along(files_download_D)){
    download.file(paste0("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/", files_download_D[k]), 
                  destfile=file.path(data_path, files_download_D[k]))
}


## load and merge variables of interest across the files we just downloaded using the rnhanesdata::process_covar function
vars_interest <- c("INDFMPIR",
                   "HSD010","HSQ480",
                   "OCD150","OCQ180","OCQ210","OCQ380")
Covar_new <- process_covar(waves=c("C","D"), 
                           varnames=vars_interest, localpath=data_path)

## merge new variables in with the Covariate_C and Covariate_D data files
Covariate_C <- left_join(Covariate_C, Covar_new$Covariate_C, by="SEQN")
Covariate_D <- left_join(Covariate_D, Covar_new$Covariate_D, by="SEQN")

## clear up the workspace a bit
rm(list=c("k",
          "files_external_C","files_external_D",
          "files_download_C","files_download_D",
          "vars_interest",
          "Covar_new")
    )







###########################################################################
##                                                                       ##
##  Section 1b: Prep accelerometry data for analysis and merge all data  ##
##                                                                       ##
###########################################################################

## Re-code activity counts which are considered "non-wear" to be 0.
## This doesn't impact much data, most estimated non-wear times correspond to 0 counts anyway
PAXINTEN_C[,paste0("MIN",1:1440)] <- PAXINTEN_C[,paste0("MIN",1:1440)]*Flags_C[,paste0("MIN",1:1440)]
PAXINTEN_D[,paste0("MIN",1:1440)] <- PAXINTEN_D[,paste0("MIN",1:1440)]*Flags_D[,paste0("MIN",1:1440)]


## Merge accelerometry (activity counts + wear/non-wear flags) and covariate data.
## We will drop the flag information shortly, but we first use it to identify "good" days of data based on
## estimated wear time
data_C <- 
    PAXINTEN_C %>% 
    ## note that both PAXINTEN_* and Covariate_* have a column
    ## called "SDDSRVYR" indicating which NHANES wave the data is associated with.
    left_join(Covariate_C, by=c("SEQN","SDDSRVYR")) %>% 
    ## Similarly, the activity count (PAXINTEN_*) and wear/non-wear flag matrices (Flags_*) share 
    ## SEQN, PAXCAL, PAXSTAT, WEEKDAY, SDDSRVR variables.
    ## In addition, when we join activity and flag data we have duplicated column names.
    ## Supply meaningful suffixes so we can differentiate them
    left_join(Flags_C, by=c("SEQN","PAXCAL","PAXSTAT","WEEKDAY","SDDSRVYR"), suffix=c(".AC",".Flag"))
data_D <- 
    PAXINTEN_D %>% 
    left_join(Covariate_D, by=c("SEQN","SDDSRVYR")) %>% 
    left_join(Flags_D, by=c("SEQN","PAXCAL","PAXSTAT","WEEKDAY","SDDSRVYR"), suffix=c(".AC",".Flag"))

## Combine 2003-2004 and 2005-2006 data into a single data frame
data <- bind_rows(data_C, data_D)

## Estimate total daily wear time and determine whether a day is "good" based on
## >= 10 hours (600 minutes) of wear time + device calibration/quality flags.
## Calculate number of good days per participant (this will be used as an exclusion criteria later -- the standard is >= 3 days),
## Then remove wear/non-wear flags from the data since we no longer need them for this analysis.
data <- 
    data %>% 
    mutate("wear_time" = rowSums(select(., one_of(paste0("MIN",1:1440,".Flag"))), na.rm=TRUE),
           "good_day"  = as.numeric(wear_time >= 600),
           "good_day"  = good_day * (PAXCAL %in% 1) * (PAXSTAT %in% 1)
    ) %>% 
    group_by(SEQN) %>% 
    mutate("n_good_days" = sum(good_day)) %>% 
    ungroup() %>% 
    select(-one_of(paste0("MIN",1:1440,".Flag"))) %>% 
    rename_at(vars(paste0("MIN",1:1440,".AC")), ~paste0("MIN",1:1440))

## clean up the workspace (free up RAM)
rm(list=c("data_C","data_D",
          "PAXINTEN_C","PAXINTEN_D",
          "Flags_C","Flags_D",
          "Covariate_C","Covariate_D"))




##############################################################################
##                                                                          ##
##  Section 1c: create new variables/relevel factor variables for analyses  ##
##                                                                          ##
##############################################################################


## Here we re-code some variables for convenience 
## and create new derived variables using the questionairre data downlaoded in
## Section 1a.
## NOTE: There is some subtlety to how we choose to define employment here.
##       Specifically, a (small) number of individuals (n=278 in this dataset) report that they are 
##       "with a job or business, but not at work" in the last week. 
##       From the questionairre it's unclear why they didn't work in the last week
##       (vacation? parental leave? medical leave?). We choose to classify these indivdiuals as 
##       missing, though it could be reasonable to consider these individuals as being either 
##       full/part time employed based on their response to "do you usually work 35 or more hours per week" (OCQ210). 
data <- 
    data %>% 
    mutate(
           ## re-code age in months at examination (accelerometer wear) to years
           ## bin age into certain groups (will use for plotting later)
           Age = RIDAGEEX/12,
           Age_cat = cut(Age, c(0, 1, 3, 6, 12, 16, 20, 30, 40, 50, 60, 70, 80, 85), right=FALSE),
           
           ## re-code day of the week to be a meaningful factor variable
           DoW = factor(WEEKDAY, levels=1:7, labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")),
           
           ## re-code self reported overall health to be a meaningful factor variable
           Overall_health = factor(HSD010, levels=c(1,2,3,4,5), labels=c("Excellent","Very good","Good","Fair","Poor")),
           
           ## re-name # of bad mental health day variable, recode refused/don't know as missing
           n_bad_mental_days = replace(HSQ480, HSQ480 %in% c(77,99), NA),
           
           ## re-code poverty-income ratio, binning into three levels 
           PIR = cut(INDFMPIR, breaks=c(0,1,2.5,Inf), right=FALSE),
           
           ## Derive employement variable using responses to OCD150, OCQ180, and OCQ380
           Employed = ifelse(OCD150 %in% 1 & OCQ180 >= 35, "Employed: full time", 
                             ifelse(OCD150 %in% 1 & OCQ180 < 35, "Employed: part time", 
                                    ifelse((OCD150 %in% 3) | (OCD150 %in% c(3,4) & OCQ380 %in% c(1,2,3,5,7)), "Unemployed: healthy",
                                           ifelse(OCD150 %in% c(3,4) & OCQ380 %in% c(4,6), "Unemployed: unhealthy", NA)
                                    )
                             )
           ),
           ## This is a bit redundant as the factor levels would be the same without supplying the levels/labels arguments
           Employed = factor(Employed, 
                             levels=c("Employed: full time","Employed: part time","Unemployed: healthy","Unemployed: unhealthy"),
                             labels=c("Employed: full time","Employed: part time","Unemployed: healthy","Unemployed: unhealthy"))
    ) %>% 
    ## re-order the data such that the activity columns are last
    ## absolutely not necessary, just a personal preference 
    select(-one_of(paste0("MIN",1:1440)), one_of(paste0("MIN",1:1440))) 







#####################################################################################################################
##                                                                                                                 ##
##  Section 1d: Some exploratory plots looking at individual profiles and estimated wear-time protocol compliance  ##
##                                                                                                                 ##
#####################################################################################################################


if(make_plots){
    source(file.path(code_path, "make_profile_plots.R"))
}

if(make_plots){
    source(file.path(code_path, "make_compliance_eda_plots.R"))
}


    



#############################################################################################
##                                                                                         ##
##  Section 1e: Some exploratory plots looking at estimated wear-time protocol compliance  ##
##                                                                                         ##
#############################################################################################



data_analysis <- 
    data %>% 
    filter(n_good_days >= 3 & good_day == 1 & Age >= 6) 
















    


