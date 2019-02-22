## load the data
data("PAXINTEN_C");data("PAXINTEN_D")
data("Flags_C");data("Flags_D")
data("Covariate_C");data("Covariate_D")


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
                   "HSQ480",
                   "OCD150","OCQ180","OCQ380")
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

## clean up the workspace (free up RAM)
rm(list=c("data_C","data_D",
          "PAXINTEN_C","PAXINTEN_D",
          "Flags_C","Flags_D",
          "Covariate_C","Covariate_D"))



############################################################################
##                                                                        ##
##  Section 1a: Subset data to individuals with good accelerometery data  ##
##                                                                        ##
############################################################################

data_analysis <- 
    data %>% 
        mutate("wear_time" = rowSums(select(., one_of(paste0("MIN",1:1440,".Flag"))), na.rm=TRUE),
               "good_day" = as.numeric(wear_time >= 600),
               "good_day" = good_day * (PAXCAL %in% 1) * (PAXSTAT %in% 1)
               ) %>% 
        group_by(SEQN) %>% 
        mutate("n_good_days" = sum(good_day)) %>% 
        ungroup() %>% 
        select(-one_of(paste0("MIN",1:1440,".Flag"))) %>% 
        filter(n_good_days >= 3)


##############################################################################
##                                                                          ##
##  Section 1b: create new variables/relevel factor variables for analyses  ##
##                                                                          ##
##############################################################################


data_analysis <- 
    data_analysis %>% 
        mutate(Age = RIDAGEEX/12,
               DoW = factor(WEEKDAY, levels=1:7, labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
               )








## Create Age in years using the age at examination (i.e. when participants wore the device)
AllAct$Age <- AllFlags$Age <- AllAct$RIDAGEEX/12

## Re-level comorbidities to assign refused/don't know as not having the condition
## Note that in practice this does not affect many individuals, but it is an assumption we're making.
levels(AllAct$CHD)    <- levels(AllFlags$CHD)    <- list("No" = c("No","Refused","Don't know"), "Yes" = c("Yes"))
levels(AllAct$CHF)    <- levels(AllFlags$CHF)    <- list("No" = c("No","Refused","Don't know"), "Yes" = c("Yes"))
levels(AllAct$Stroke) <- levels(AllFlags$Stroke) <- list("No" = c("No","Refused","Don't know"), "Yes" = c("Yes"))
levels(AllAct$Cancer) <- levels(AllFlags$Cancer) <- list("No" = c("No","Refused","Don't know"), "Yes" = c("Yes"))
levels(AllAct$Diabetes) <- levels(AllFlags$Diabetes) <- list("No" = c("No","Borderline", "Refused","Don't know"), "Yes" = c("Yes"))


## Re-level education to have 3 levels and categorize don't know/refused to be missing
levels(AllAct$EducationAdult) <- levels(AllFlags$EducationAdult) <- list("Less than high school" = c("Less than 9th grade", "9-11th grade"),
                                                                         "High school" = c("High school grad/GED or equivalent"),
                                                                         "More than high school" = c("Some College or AA degree", "College graduate or above"))

## Re-level alcohol consumption to include a level for "missing"
levels(AllAct$DrinkStatus) <- levels(AllFlags$DrinkStatus) <- c(levels(AllAct$DrinkStatus), "Missing alcohol")
AllAct$DrinkStatus[is.na(AllAct$DrinkStatus)] <- AllFlags$DrinkStatus[is.na(AllAct$DrinkStatus)] <- "Missing alcohol"

## day of the week as a factor variabe
AllAct$DoW <- AllFlags$DoW <- factor(AllAct$WEEKDAY, levels=1:7, labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

## Re-order columns so that activity and wear/non-wear flags are the last 1440 columns of our two
## data matrices. This is a personal preference and is absolutely not necessary.
act_cols <- which(colnames(AllAct) %in% paste0("MIN",1:1440))
oth_cols <- which(!colnames(AllAct) %in% paste0("MIN",1:1440))
AllAct   <- AllAct[,c(oth_cols,act_cols)]
AllFlags <- AllFlags[,c(oth_cols,act_cols)]
rm(list=c("act_cols","oth_cols"))

## create a plot of individual profiles
if(make_figures){
    source(file.path(code_path,"create_individual_profile_plot.R"))
}



############################################################################
##                                                                        ##
##  Section 1c: Subset data to individuals with good accelerometery data  ##
##                                                                        ##
############################################################################

## "Full" data 102,417 days across 14,631 participants
## subset to under 85 (85 and older are coded as NA in NHANES for confidentiality reasons)
AllAct   <- subset(AllAct, !is.na(Age))
AllFlags <- subset(AllFlags, !is.na(Age))

## Remove days with fewer than 10 hours of wear time and/or device calibration issues
## data calibration citations
## wear time citations
##
## "Good" data 64,228 days across 12,532 participants
keep_inx <- exclude_accel(AllAct, AllFlags)
AllAct   <- AllAct[keep_inx,]
AllFlags <- AllFlags[keep_inx,]

## Remove subjects with fewer than 3 days of good data
## Data 61,837 days across 10,953 participants
ids_3days <- names(table(AllAct$SEQN))[table(AllAct$SEQN) >= 3]
AllAct    <- subset(AllAct, SEQN %in% ids_3days)
AllFlags  <- subset(AllFlags, SEQN %in% ids_3days)
rm(list=c("keep_inx","ids_3days"))

## 2 remaining days of data have NAs in their data even though they are associated with 
## >= 10 hours of estimated weartime. 
## These occur on the last day of observation.
## Impute these as 0 activity counts
act_mat_full <- as.matrix(AllAct[,paste0("MIN",1:1440)])
table(apply(act_mat_full,1,function(x) sum(is.na(x))))
act_mat_full[is.na(act_mat_full)] <- 0

## smooth both raw and log transformed activity counts using fpca
act_mat_full_sm     <- fpca.face(act_mat_full, knots=50)$Yhat
act_log_mat_full_sm <- fpca.face(log(1+act_mat_full), knots=50)$Yhat


# compute total log activity count in each window of length tlen
tlen <- 5
nt   <- floor(1440/tlen)
tind <- seq(0,1,len=nt)
# create a list of indices for binning into tlen minute windows
inx_col_ls       <- split(1:1440, rep(1:nt,each=tlen))
act_mat_bin     <- sapply(inx_col_ls, function(x) rowMeans(act_mat_full_sm[,x,drop=FALSE]))
act_log_mat_bin <- sapply(inx_col_ls, function(x) rowMeans(act_log_mat_full_sm[,x,drop=FALSE]))

# get average profiles within subjects
inx_row_ls  <- split(1:nrow(act_mat_full), f=factor(AllAct$SEQN, levels=unique(AllAct$SEQN)))
act_mat     <- t(vapply(inx_row_ls, function(x) colMeans(act_mat_bin[x,,drop=FALSE]), numeric(nt)))
act_log_mat <- t(vapply(inx_row_ls, function(x) colMeans(act_log_mat_bin[x,,drop=FALSE]), numeric(nt)))
rm(list=c("inx_col_ls","inx_row_ls"))




## get a data frame with one row per subject
data_analysis <- data.frame(AllAct[!duplicated(AllAct$SEQN),-which(colnames(AllAct) %in% paste0("MIN",1:1440))],
                            "act_mat" = I(act_mat),
                            "act_log_mat" = I(act_log_mat)
)

## clean up the workspace
rm(list=c("act_mat","act_log_mat","act_mat_bin","act_mat_full","act_mat_full_sm",
          "AllAct","AllFlags","act_log_mat_full_sm","act_log_mat_bin"))






if(save_data){
    saveRDS(data_analysis, file=file.path(data_path, "data_analysis.rds"), compress="xz")
}
