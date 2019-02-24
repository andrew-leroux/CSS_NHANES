## Is there differential day-of-week wear by age?
## These boxplots do not account for survey weights!
plt_wt_dow_unwgt <- 
    data %>% 
    filter(good_day==1) %>% 
    ggplot(aes(x=DoW, y=Age)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) +
    xlab("") + ggtitle("Unweighted")
## Account for examination weights. Note, these are -technically- not 4 year survey weights
## and will give misleading population estimates (i.e. estimated total number of 40 year old women), 
## but are sufficient for exploratry analyses.
plt_wt_dow_wgt <- 
    data %>% 
    filter(good_day==1) %>% 
    ggplot(aes(x=DoW, y=Age, weight=WTMEC2YR/mean(WTMEC2YR))) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) +
    xlab("") + ggtitle("Survey weighted") 

jpeg(file.path(figure_path, "wear_time_by_dow.jpeg"), height=550, width=1000, quality=100)
grid.arrange(plt_wt_dow_unwgt, plt_wt_dow_wgt,ncol=2,
             top=textGrob("Age at accelerometer wear vs day of week",gp=gpar(fontsize=textsize*1.25,font=3)))    
dev.off()   
rm(list=c("plt_wt_dow_unwgt","plt_wt_dow_wgt"))




## plot number of days of "good" data versus age
plt_ndays_by_age_unwgt <- 
    data %>% 
    group_by(SEQN) %>% 
    distinct(Age, Age_cat, n_good_days) %>% 
    ggplot(aes(x=Age, y=n_good_days)) + geom_jitter(height=0.1, width=0, color=rgb(0,0,0,0.2)) + 
    geom_smooth(method="gam",formula=y ~ s(x, bs="tp",k=30),colour="red") + 
    ylab("# of days") + 
    scale_y_continuous(breaks=0:7) + ggtitle("Unweighted") 

plt_ndays_by_age_wgt <- 
    data %>% 
    group_by(SEQN) %>% 
    distinct(Age, Age_cat, n_good_days, WTMEC2YR) %>% 
    ggplot(aes(x=Age, y=n_good_days, weight=WTMEC2YR/mean(WTMEC2YR))) + geom_jitter(height=0.1, width=0, color=rgb(0,0,0,0.2)) + 
    geom_smooth(method="gam",formula=y ~ s(x, bs="tp",k=30),colour="red") + 
    ylab("# of days") + 
    scale_y_continuous(breaks=0:7) + ggtitle("Survey weighted") 
jpeg(file.path(figure_path, "ndays_accel_by_age.jpeg"), height=500, width=1000, quality=100)
grid.arrange(plt_ndays_by_age_unwgt, plt_ndays_by_age_wgt,ncol=2,
             top=textGrob("# of good days of accelerometer data vs age at wear",gp=gpar(fontsize=textsize*1.25,font=3)))    
dev.off()   
rm(list=c("plt_ndays_by_age_wgt","plt_ndays_by_age_unwgt"))


## Another way to look at this same quesiton would be to 
## consider, within each age strata, what is the proportion of each day of the week?
data %>% 
    filter(good_day==1) %>% 
    select(Age_cat, DoW) %>% 
    table() %>% prop.table(margin=1) %>% round(2)





## Does average low/light activity vary by day of week? Does this pattern vary by age? 
## Stratify by age and plot TAC
plt_TLAC_by_age_category <- 
    data %>% 
    filter(good_day==1 & !is.na(Age)) %>% 
    mutate("TLAC" = rowSums(log(1+select(., one_of(paste0("MIN",1:1440)))),na.rm=TRUE)) %>%
    ggplot(aes(x=DoW, y=TLAC)) + geom_boxplot() + facet_wrap(~Age_cat,nrow=2) + ylab("Total Log Activity Count (TLAC)") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

jpeg(file.path(figure_path, "TLAC_by_age_category.jpeg"), height=1000, width=1400, quality=100)
plt_TLAC_by_age_category
dev.off()
rm(list=c("plt_TLAC_by_age_category"))


