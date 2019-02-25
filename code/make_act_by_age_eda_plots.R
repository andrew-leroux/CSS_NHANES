
### This tidyverse code is extremely slow! 
### I've commented it out, but it's here for your reference
# data_plt_wknd_age <- 
#     data %>%
#     filter(n_good_days >= 3 & good_day == 1 & Age >= 6) %>% 
#     mutate("Weekend" = factor(as.numeric(DoW %in% c("Saturday","Sunday")), levels=c(0,1), labels=c("Weekday","Weekend"))) %>% 
#     group_by(SEQN, Weekend, Age) %>%
#     summarize_at(vars(MIN1:MIN1440),.funs=function(x) mean(log(1+x),na.rm=TRUE))  %>%
#     group_by(Age_yrs = round(Age), Weekend) %>%
#     summarize_at(vars(MIN1:MIN1440),mean,na.rm=TRUE)  %>%
#     gather(key="time", value="AC", MIN1:MIN1440) %>% 
#     mutate("time" = as.numeric(str_extract(time, "[0-9]+"))) 


## Much faster using base R, but code not as "clean"
data_plt_wknd_age <- 
    data %>%
    filter(n_good_days >= 3 & good_day == 1 & Age >= 6) %>% 
    mutate("Weekend" = factor(as.numeric(DoW %in% c("Saturday","Sunday")), levels=c(0,1), labels=c("Weekday","Weekend")))

uid <- unique(data_plt_wknd_age$SEQN)
nid <- length(uid)

## get identifier vectors for our 3 criteria
id_vec    <- data_plt_wknd_age$SEQN
wknd_vec  <- data_plt_wknd_age$Weekend
age_vec   <- round(data_plt_wknd_age$Age)
## create an empty vector for storing each subjects' age
## could also do something along the lines of:
##   age_vec_i <- round(data_plt_wknd_age$Age[!duplicated(data_plt_wknd_age$SEQN)])
## then you wouldn't have to fill in the vector inside the for() loop below
age_vec_i <- rep(NA, nid)
## create empty matrices to hold profiles separately by weekend and weekday
profiles_wkday <- profiles_wknd <- matrix(NA, ncol=1440, nrow=nid)
## extract the activity count data, log transform it and convert it to a matrix
## for efficiency
act_mat <- log(1+as.matrix(data_plt_wknd_age[,paste0("MIN",1:1440)]))

## Loop over participants, find their weekend/weekday data
## average at each time point, t, within participants
for(i in seq_along(uid)){
    inx_wkday_i <- which(id_vec == uid[i] & wknd_vec == "Weekday")
    inx_wknd_i <- which(id_vec == uid[i] & wknd_vec == "Weekend")
    
    age_vec_i[i] <-age_vec[which(id_vec == uid[i])][1]
    
    if(length(inx_wkday_i) > 0){
        profiles_wkday[i,] <- colMeans(act_mat[inx_wkday_i,,drop=FALSE],na.rm=TRUE)
    }
    if(length(inx_wknd_i) > 0){
        profiles_wknd[i,] <- colMeans(act_mat[inx_wknd_i,,drop=FALSE],na.rm=TRUE)
    }
    rm(list=c("inx_wkday_i","inx_wknd_i"))
    if(i %% 1000 == 0) print(i)
}
rm(list=c("i","nid","uid","id_vec","wknd_vec","act_mat"))


## Given our subject-specific weekend/weekday profiles, we can 
## calculate averages within each age (rounded to the nearest year)
## Same logic here in the for loop
uage <- sort(unique(age_vec))
nage <- length(uage)
profiles_wkday_age <- profiles_wknd_age <- matrix(NA, ncol=1440,nrow=nage)
for(i in seq_along(uage)){
    inx_wkday_i <- which(age_vec_i == uage[i])
    inx_wknd_i  <- which(age_vec_i == uage[i])
    if(length(inx_wkday_i) > 0){
        profiles_wkday_age[i,] <- colMeans(profiles_wkday[inx_wkday_i,,drop=FALSE],na.rm=TRUE)   
    }
    if(length(inx_wknd_i) > 0){
        profiles_wknd_age[i,] <- colMeans(profiles_wknd[inx_wknd_i,,drop=FALSE],na.rm=TRUE)     
    }
    rm(list=c("inx_wkday_i","inx_wknd_i"))
}
rm(list=c("i","profiles_wknd","profiles_wkday","age_vec","age_vec_i"))

## Combine the profiles in a "tidy" format that is compatible with ggplot syntax.
## If you aren't determined to use ggplot, you can use the function fields::image.plot()
##   fields::image.plot(t(profiles_wkday)); fields::image.plot(t(profiles_wknd))
data_plt_wknd_age <- 
    data.frame(Age_yrs = rep(rep(uage, each=1440),2), 
               time = rep(rep(1:1440, nage),2), 
               Weekend = rep(c("Weekend","Weekday"), each=1440*nage),
               AC = c(as.vector(t(profiles_wknd_age)),
                      as.vector(t(profiles_wkday_age))
                    )
            )


## plot the data
jpeg(file.path(figure_path, paste0("PA_profiles_by_age_by_wknd.jpeg")), height=800, width=650, quality=100)
data_plt_wknd_age %>% 
    ggplot(aes(x=time, y=Age_yrs)) + geom_raster(aes(fill=AC)) + scale_y_continuous(breaks=seq(10,80,by=10),limits=c(6,85)) + 
    facet_wrap(~Weekend, ncol=1, strip.position = "right") + xlab("Time of Day") + ylab("Age (years)") +
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"),limits=c(0,1440)) + 
    scale_fill_gradientn(colours=tim.colors(100))+ theme(legend.position="none")
dev.off()

rm(list=c("data_plt_wknd_age",
          "profiles_wknd_age",
          "profiles_wkday_age",
          "nage","uage"))


