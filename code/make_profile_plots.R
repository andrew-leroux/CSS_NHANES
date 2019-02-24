set.seed(16)
data_profile_plt <- 
    data %>% 
    filter(n_good_days %in% c(4,7)) %>%
    distinct(SEQN, n_good_days) %>% 
    group_by(n_good_days) %>%
    sample_n(1, replace=FALSE) %>% 
    inner_join(data, ., by="SEQN") %>% 
    group_by(SEQN) %>% 
    mutate("day_obs" = 1:n()) %>% 
    ungroup()

jpeg(file.path(figure_path, paste0("profile_id1.jpeg")), height=800, width=650, quality=100)
data_profile_plt %>% 
    filter(SEQN == unique(.$SEQN)[1]) %>%
    select(one_of(c("DoW","day_obs", paste0("MIN",1:1440)))) %>% 
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+"))) %>%
    arrange(day_obs, time) %>% 
    mutate(DoW = factor(DoW, levels = unique(.$DoW)))%>%
    ggplot(aes(x=time, xend=time, y=0, yend=AC)) + geom_segment(col=rgb(0,0,0,0.5)) + 
    facet_wrap(~ DoW,ncol=1,strip.position = "right") + xlab("Time of Day") + ylab("Activity Count") + 
    geom_smooth(aes(x=time, y=AC,color="firebrick2"), method="gam",formula=y~s(x,bs="cr",k=100),se=FALSE,color="firebrick2")+
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"))
dev.off()   


jpeg(file.path(figure_path, paste0("profile_id2.jpeg")), height=800, width=650, quality=100)
data_profile_plt %>% 
    filter(SEQN == unique(.$SEQN)[2]) %>%
    select(one_of(c("DoW","day_obs", paste0("MIN",1:1440)))) %>% 
    gather(key="time", value="AC", MIN1:MIN1440) %>% 
    mutate("time" = as.numeric(str_extract(time, "[0-9]+"))) %>%
    arrange(day_obs, time) %>% 
    mutate(DoW = factor(DoW, levels = unique(.$DoW)))%>%
    ggplot(aes(x=time, xend=time, y=0, yend=AC)) + geom_segment(col=rgb(0,0,0,0.5)) + 
    facet_wrap(~ DoW,ncol=1,strip.position = "right") + xlab("Time of Day") + ylab("Activity Count") + 
    geom_smooth(aes(x=time, y=AC), method="gam",formula=y~s(x,bs="cr",k=100),se=FALSE,color="firebrick2")+
    scale_x_continuous(breaks=c(1,6,12,18,23)*60 + 1, 
                       labels=c("01:00","06:00","12:00","18:00","23:00"))
dev.off()   

rm(list=c("data_profile_plt"))