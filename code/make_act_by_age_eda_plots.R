data_plt_age <- 
    data %>% 
    filter(n_good_days >= 3 & good_day == 1 & Age >= 6) %>% 
    mutate("Weekend" = factor(as.numeric(DoW %in% c("Saturday","Sunday")), levels=c(0,1), labels=c("Weekday","Weekend"))) %>% 
    group_by(SEQN) %>%
    summarize_at(vars(MIN1:MIN1440),.funs=function(x) mean(log(1+x),na.rm=TRUE))  %>%
    ungroup() %>% 
    group_by(Age_yrs = round(Age)) %>%
    summarize_at(vars(MIN1:MIN1440),mean,na.rm=TRUE)  %>%
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+"))) 


jpeg(file.path(figure_path, paste0("PA_profiles_by_age.jpeg")), height=400, width=650, quality=100)
data_plt_age %>% 
    ggplot(aes(x=time, y=Age_yrs)) + geom_raster(aes(fill=AC)) + scale_y_continuous(breaks=seq(10,80,by=10),limits=c(6,85)) + 
    xlab("Time of Day") + ylab("Age (years)") +
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"),limits=c(0,1440)) + 
    scale_fill_gradientn(colours=tim.colors(100))+ theme(legend.position="none")
dev.off()



data_plt_wknd_age <- 
    data %>% 
    filter(n_good_days >= 3 & good_day == 1 & Age >= 6) %>% 
    mutate("Weekend" = factor(as.numeric(DoW %in% c("Saturday","Sunday")), levels=c(0,1), labels=c("Weekday","Weekend"))) %>% 
    group_by(SEQN, Weekend) %>%
    summarize_at(vars(MIN1:MIN1440),.funs=function(x) mean(log(1+x),na.rm=TRUE))  %>%
    ungroup() %>% 
    group_by(Age_yrs = round(Age), Weekend) %>%
    summarize_at(vars(MIN1:MIN1440),mean,na.rm=TRUE)  %>%
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+"))) 


jpeg(file.path(figure_path, paste0("PA_profiles_by_age_by_wknd.jpeg")), height=800, width=650, quality=100)
data_plt_wknd_age %>% 
    ggplot(aes(x=time, y=Age_yrs)) + geom_raster(aes(fill=AC)) + scale_y_continuous(breaks=seq(10,80,by=10),limits=c(6,85)) + 
    facet_wrap(~Weekend, ncol=1, strip.position = "right") + xlab("Time of Day") + ylab("Age (years)") +
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"),limits=c(0,1440)) + 
    scale_fill_gradientn(colours=tim.colors(100))+ theme(legend.position="none")
dev.off()

rm(list=c("data_plt_age", "data_plt_wknd_age"))