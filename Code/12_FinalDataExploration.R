
#descriptive analysis

#load packages
  list.of.packages <- c("tidytext","tidyverse","dplyr","tidyr", "formattable", "lubridate", "readxl", "texreg","ggpmisc", "plm", "jtools", "lmtest")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, require, character.only = TRUE)

#load data
  final <- read.csv("Data/tweets_final_postclassifier.csv", stringsAsFactors = FALSE, numerals = "no.loss")
  final <- final %>% mutate(date = ymd(date), day=day(date),month=month(date),year=year(date))
  final <- final %>% filter(!is.na(date))
  
  events <- read_xlsx("Data/events.xlsx")            
  events$date <- ymd(events$date)
  
#prepare data  
  final_count_days <- final %>% 
    group_by(date) %>% 
    dplyr::summarise(hate=sum(hatenomNA),nohate = n()-hate,tweetstotal=n(),hatescaleM=mean(hatescaleM), hatescale=mean(hatescale)) %>%
    mutate(hatepercentage = hate/(hate+nohate)) %>% 
    arrange(desc(hate)) 
  final_count_days_events <- final_count_days %>% left_join(events, by="date")
  
  
#########################################
#wikipedia data
  wikidata <- read_csv("Data/GretaWikiPageViews.csv")
  names(wikidata) = c("date","pagecount")
  final_comb_wiki <- final_count_days %>% left_join(wikidata, by=c("date"))
  
  ggplot(final_comb_wiki %>% filter(date >= "2019-01-01"),
         aes(x=tweetstotal,y=pagecount)) + 
    scale_color_grey(start = 0.2) + 
    geom_point() + 
    geom_smooth(formula = y ~ x, method="lm", colour = "black") + 
    scale_y_log10() + 
    scale_x_log10()+
    stat_fit_tb(digits = 2)+
    theme_classic()+
    ylab(label = "daily Wikipedia searches of Thunberg (log)")+
    xlab(label = "daily tweets at Thunberg (log)")
  
  
##########################################  
#hate vs. nohate    
    plot_day <- ggplot(final_count_days,aes(x=date))
  
  #total frequency distribution of hate over time    
    plot_hate_total <-  plot_day +
      #geom_smooth(aes(y = tweetstotal, colour="TOTAL")) +
      geom_line(aes(y = tweetstotal, colour = "TOTAL")) + 
      geom_line(aes(y=hate, colour="HATE")) +
      #geom_smooth(aes(y = hate, colour="HATE")) +
      geom_line(aes(y=nohate,colour="NOHATE")) 
      #geom_smooth(aes(y = nohate, colour="NOHATE"))
    plot_hate_total+
      labs(title = "total frequency distribution of hate over time")
  
  
    hatedistribution <- final_count_days %>% 
      dplyr::select(date, hate, nohate, tweetstotal) %>%
      pivot_longer(cols=hate:tweetstotal, names_to="tweets",values_to="freq") 
    
    ggplot(hatedistribution,
           aes(fill=tweets, y=freq, x=date)) + 
      geom_area(position="dodge", stat="identity", colour = "black", fill = "#71CA97") + 
      facet_grid(rows = vars(tweets)) +
      theme_classic()+
      ylab("number of tweets")+
      theme(legend.position="none")+
      scale_color_grey(start=0.0, end=0.8)
    
    
  #table hate
    table_hate <- data.frame(
      Frequency = c(sum(final_count_days$hate), sum(final_count_days$nohate), sum(final_count_days$tweetstotal)), 
      Percentage = c(mean(final_count_days$hate/final_count_days$tweetstotal), mean(final_count_days$nohate/final_count_days$tweetstotal), 
                     mean(final_count_days$tweetstotal/final_count_days$tweetstotal)),
      Growth_fit = c("0.0057***", "0.0048***", "0.0050***"),
      row.names = c("Hate", "No Hate", "Total"))
    table_hate$Percentage <- round(table_hate$Percentage, digits = 2)
    table_hate$Frequency <- comma(table_hate$Frequency, digits = 0)
    formattable(table_hate)        

  #growth of relation hate vs nohate
    plot_day +
      geom_line(aes(y=hatepercentage, colour = "HATE")) + 
      geom_line(aes(y=(nohatepercentage = nohate/(hate+nohate)), colour = "NOHATE"))+
      geom_smooth(method = "lm",aes(y=hatepercentage, colour = "HATE"))+
      stat_fit_tb(aes(y = hatepercentage), digits = 2)+
      labs(title = "percentage of hate vs. no hate over time")+
      ylab("percentage of tweets")
    
    
  #analysis with hate
    plot_day + 
      geom_point(aes(y = hate, colour = "hate"))+
      geom_smooth(method = "lm",aes(y = hate, colour = "hate"))+
      scale_y_log10()+
      geom_point(aes(y = nohate, colour = "nohate"))+
      geom_smooth(method = "lm",aes(y = nohate, colour = "nohate"))+
      geom_point(aes(y = tweetstotal, colour = "total"))+
      geom_smooth(method = "lm",aes(y = tweetstotal, colour = "total"))+
      #stat_fit_tb(aes(y = hate), digits = 3)+
      #stat_fit_tb(aes(y = nohate), digits = 3)+
      stat_fit_tb(aes(y = hate), digits = 2)+
      scale_color_grey(start=0.0, end=0.8) +
      theme_classic()+
      ylab(label = "change in number of tweets (log)")+
      labs(caption = "Note: The regression summary is given for the change in hate tweets.")+
      theme(legend.position="bottom")
    
    plot_day + 
      geom_point(aes(y = hatescale))+
      geom_smooth(method = "lm",aes(y = hatescale))+
      stat_fit_tb(aes(y = hatescale), digits = 2)
  
    plot_day + 
      geom_point(aes(y = hatescaleM))+
      geom_smooth(method = "lm",aes(y = hatescaleM))+
      stat_fit_tb(aes(y = hatescaleM), digits = 2)
    
##############################################################
#incidents
    
    
  ##plot Timing of Hate Dec 2019
    plot_event <- ggplot(final_count_days_events %>% filter(date >= "2019-12-01"), aes(x=date))
    plot_event_stack <- plot_event + 
      geom_line(aes(y=hate))+
      theme_classic()
    plot_event_stack
    
    dummy <- final_count_days_events %>% filter(date >= "2019-12-01") %>% filter(!is.na(incident)) %>% arrange(date) #%>% filter(date>"2019-07-01")
    
    for(i in 1:length((final_count_days_events %>% filter(!is.na(incident)))$date)){
      plot_event_stack <- plot_event_stack + 
        ggplot2::annotate("pointrange", x = dummy$date[i], y = i*1200+15000, ymin =0, ymax = i*1200+15000,    #ifelse(j-5000<0,0,j-5000)
                          colour = "#71CA97", size = 1) + 
        ggplot2::annotate("text", x = dummy$date[i], y = i*1200+15000, label = dummy$incident[i],angle=-45,size=3.5)
      
    }
    plot_event_stack+
      ylim(0,34000)
    
    
  ##plot Timing of Hate Sept 2019
    plot_event <- ggplot(final_count_days_events %>% filter(date >= "2019-09-01" & date < "2019-10-01"), aes(x=date))
    plot_event_stack <- plot_event + 
      geom_line(aes(y=hate))+
      theme_classic()
    plot_event_stack
    
    dummy <- final_count_days_events %>% filter(date >= "2019-09-01" & date < "2019-10-01") %>% filter(!is.na(incident)) %>% arrange(date) #%>% filter(date>"2019-07-01")
    
    for(i in 1:length((final_count_days_events %>% filter(!is.na(incident)))$date)){
      plot_event_stack <- plot_event_stack + 
        ggplot2::annotate("pointrange", x = dummy$date[i], y = i*1500+30000, ymin =0, ymax = i*1500+30000,    #ifelse(j-5000<0,0,j-5000)
                          colour = "#71CA97", size = 1) + 
        ggplot2::annotate("text", x = dummy$date[i], y = i*1500+30000, label = dummy$incident[i],angle=-45,size=3.5)
    }
    plot_event_stack+
      ylim(0,52000)
    
    
  ##plot timing of hate Aug 2018 - Jun 2019
    plot_event <- ggplot(final_count_days_events %>% filter(date < "2019-07-01"), aes(x=date)) 
    plot_event_stack <- plot_event + 
      geom_line(aes(y=hate)) +
      theme_classic()
    plot_event_stack
    dummy <- final_count_days_events %>% filter(date < "2019-07-05") %>% filter(!is.na(incident)) %>% arrange(date) #%>% filter(date>"2019-07-01")
    dummy$incident[1] <- "protest Sweden"
    dummy$incident[22] <- "Ambassador of Conscience Award"
    dummy$incident[11] <- "first international climate change protests"
    for(i in 1:length((final_count_days_events %>% filter(!is.na(incident)))$date)){
      plot_event_stack <- plot_event_stack + 
        ggplot2::annotate("pointrange", x = dummy$date[i], y = i*170+1000, ymin =0, ymax = i*170+1000,    #ifelse(j-5000<0,0,j-5000)
                          colour = "#71CA97", size = 1) + 
        ggplot2::annotate("text", x = dummy$date[i], y = i*170+1000, label = dummy$incident[i],angle=-50,size=3.5)
    }
    plot_event_stack+
      ylim(0,6000)
    
    
  ##plot timing of hate Oct - Nov 2019
    plot_event <- ggplot(final_count_days_events %>% filter(date >= "2019-09-30" & date < "2019-12-02"), aes(x=date)) 
    plot_event_stack <- plot_event + 
      geom_line(aes(y=hate)) +
      theme_classic()
    plot_event_stack
    dummy <- final_count_days_events %>% filter(date >= "2019-09-28" & date < "2019-12-02") %>% filter(!is.na(incident)) %>% arrange(date) #%>% filter(date>"2019-07-01")
    dummy$incident[1] <- "invitation parliament Russia"
    dummy$incident[2] <- "Vladimir Putin statement"
    
    for(i in 1:length((final_count_days_events %>% filter(!is.na(incident)))$date)){
      plot_event_stack <- plot_event_stack + 
        ggplot2::annotate("pointrange", x = dummy$date[i], y = i*200+3000, ymin =0, ymax = i*200+3000,    #ifelse(j-5000<0,0,j-5000)
                          colour = "#71CA97", size = 1) + 
        ggplot2::annotate("text", x = dummy$date[i], y = i*200+3000, label = dummy$incident[i],angle=-55,size=3.5)
      
    }
    plot_event_stack+
      ylim(0,5500)
    
  ##plot timing of hate Jul - Aug 2019
    plot_event <- ggplot(final_count_days_events %>% filter(date >= "2019-06-30" & date < "2019-09-06"), aes(x=date)) 
    plot_event_stack <- plot_event + 
      geom_line(aes(y=hate)) +
      theme_classic()
    plot_event_stack
    dummy <- final_count_days_events %>% filter(date >= "2019-06-30" & date < "2019-09-06") %>% filter(!is.na(incident)) %>% arrange(date) #%>% filter(date>"2019-07-01")
    dummy$incident[1] <- "French parliamentarian tweet"
    dummy$incident[2] <- "French right-wing boycott in parliament"
    dummy$incident[3] <- "speech National Assembly in France"
    dummy$incident[6] <- "start of sailing trip in Plymouth, UK"
    dummy$incident[7] <- "Arron Banks tweet"
    dummy$incident[8] <- "end of sailing trip in New York, USA"
    
    
    for(i in 1:length((final_count_days_events %>% filter(!is.na(incident)))$date)){
      plot_event_stack <- plot_event_stack + 
        ggplot2::annotate("pointrange", x = dummy$date[i], y = i*250+2500, ymin =0, ymax = i*250+2500,    #ifelse(j-5000<0,0,j-5000)
                          colour = "#71CA97", size = 1) + 
        ggplot2::annotate("text", x = dummy$date[i], y = i*250+2500, label = dummy$incident[i],angle=-45,size=3.5)
      
    }
    plot_event_stack+
      ylim(0,5500)
    
    
    
  #regression incidents 
    final_reg <- final_count_days_events %>%
      mutate(inc = ifelse(is.na(incident), 0, ifelse (!is.na(incident), 1, 0)))
    final_reg$category <- final_reg$category %>% replace_na("no incident")
    final_reg$category <- factor(final_reg$category)
    final_reg$inc <- factor(final_reg$inc)
    final_reg <- within(final_reg, category <- relevel(category, ref = 2))
    final_reg <- final_reg %>% 
      mutate(cattotal = ifelse(category == "offline action other", "action other", 
                               ifelse(category == "online action other", "action other", 
                                      ifelse(category == "offline action Greta", "action Greta", 
                                             ifelse(category == "online action other", "action Greta", 
                                                    ifelse(category == "event", "events", "no incident"))))))
    final_reg$cattotal <- factor(final_reg$cattotal)
    final_reg <- within(final_reg, cattotal <- relevel(cattotal, ref = 4))

    reg_hate1 <- lm(dplyr::lead(hate, order_by = date) ~ inc + hate + dplyr::lag(hate, order_by = date), data = final_reg)
    reg_hate2 <- lm(dplyr::lead(hate, order_by = date) ~ cattotal + hate + dplyr::lag(hate, order_by = date), data = final_reg)
    model_hate1 <- summary(reg_hate1)
    model_hate1$coefficients <- unclass(coeftest(reg_hate1, vcov=vcovHC(reg_hate1, type="HC1")))
    model_hate1$coefficients <- round(model_hate1$coefficients, digits = 2)
    model_hate2 <- summary(reg_hate2)
    model_hate2$coefficients <- unclass(coeftest(reg_hate2, vcov=vcovHC(reg_hate2, type="HC1")))
    model_hate2$coefficients <- round(model_hate2$coefficients, digits = 2)
    screenreg(list(reg_hate1, reg_hate2), digits = 2)
    htmlreg(list(reg_hate1, reg_hate2), file = "hatereg.doc", 
            inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
            head.tag = TRUE, body.tag = TRUE)
    
    
##############################################################
    
#categories
    hate_count <- final %>%
      subset(hatenomNA == 1 | hatescale >1 | hatescaleM > 1) %>%
      group_by(date) %>% #, factor(hatescale) for language regression
      summarise(hatetotal = n(), hatescaleM=mean(hatescaleM)) %>%
      arrange(desc(hatetotal))
  final$hatescale <- as.numeric(final$hatescale)
    
    final_count_days_cat <- final %>% 
      subset(hatenomNA == 1 | hatescale >1 | hatescaleM > 1) %>%
      group_by(date) %>% 
      dplyr::summarise_at(names(final)[str_detect(names(final),"^X")], sum) %>%
      left_join(hate_count, by = "date")
    
    final_count_days_cat_events <- final_count_days_cat %>% 
      left_join(events, by="date")
    
    plot_day_cat <- ggplot(final_count_days_cat %>% filter(date >= "2019-01-01"),aes(x=date))  #%>% filter(date >= "2019-01-01")
    plot_day_cat_events <- ggplot(final_count_days_cat_events %>% filter(date >= "2019-01-01"),aes(x=date))  #%>% filter(date >= "2019-01-01")
    

###############################################################    
    
#explicit vs implicit
      
    #growth of relation explicit vs implicit
    perc_plot <- plot_day_cat +
      geom_smooth(method = "lm", aes(y=(explicit = X5_explicitlanguage/hatetotal), colour = "Explicit Language"))+
      geom_point(aes(y=(explicit = X5_explicitlanguage/hatetotal), colour = "Explicit Language")) + 
      geom_smooth(method = "lm", aes(y=(implicit = X5_implicitlanguage/hatetotal), colour = "Implicit Language"))+
      geom_point(aes(y=(implicit = X5_implicitlanguage/hatetotal), colour = "Implicit Language"))+
      labs(title = "percentage of vs. no hate over time")+
      ylab("percentage of tweets")+
      #stat_fit_tb(aes(y = (explicit = X5_explicitlanguage/hatetotal)), digits = 2)+
      stat_fit_tb(aes(y = (X5_explicitlanguage/hatetotal)), digits = 2)+
      labs(title = "Change of Explicit and Implicit Language Use in 2019", caption = "Source: Own Illustration")+
      scale_color_grey(start=0.5, end=0.0) + 
      theme_classic()+
      ylab("percentage %")

  #percentage frequencies  
    language <- final_count_days_cat %>%
      mutate(exp_perc = X5_explicitlanguage/hatetotal) %>%
      mutate(imp_perc = X5_implicitlanguage/hatetotal) 
    mean(language$exp_perc)
    mean(language$imp_perc)
    
  #relationship with degree of hate
    final$hatescale <- as.factor(final$hatescale)
    hate_count <- final %>%
      subset(hatenomNA == 1 | hatescale != "1" | hatescaleM > 1) %>%
      group_by(date, hatescale) %>% #, factor(hatescale) for language regression
      summarise(hatetotal = n(), hatescaleM=mean(hatescaleM)) %>%
      arrange(desc(hatetotal))
    final$hatescale <- as.numeric(final$hatescale)
    
    final_count_days_cat <- final %>% 
      subset(hatenomNA == 1 | hatescale >1 | hatescaleM > 1) %>%
      group_by(date) %>% 
      dplyr::summarise_at(names(final)[str_detect(names(final),"^X")], sum) %>%
      left_join(hate_count, by = "date")
    
    final_reg <- final_count_days_cat %>%
      mutate(percexplicit = X5_explicitlanguage/X5_implicitlanguage) 
   
    reg_lang <- plm(percexplicit ~ hatetotal*hatescale + hatetotal + hatescale + X3_violence + X3_insult + X3_disparagement + X3_prejudice + X3_mocking, 
                    data = final_reg %>% filter(date >= "2019-01-01"), index = ("date"),
                   effect = "time", model = "within")
    model <- summary(reg_lang)
    model$coefficients <- unclass(coeftest(reg_lang, vcov=vcovHC(reg_lang, type="sss", cluster="time")))
    model$coefficients <- round(model$coefficients, digits = 6)                                  
    screenreg(list(reg_lang), digits = 6)
    htmlreg(reg_lang, file = "hatelang.doc", digits= 6,
            inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
            head.tag = TRUE, body.tag = TRUE)


#########################################################################    
    
#type of message    

    #percentage distribution
    perc_plot <- plot_day_cat +
      geom_smooth(aes(y = hatetotal/hatetotal, colour="TOTAL")) +
      geom_smooth(aes(y= X3_insult/hatetotal, colour="INSULT")) +
      geom_smooth(aes(y= X3_prejudice/hatetotal,colour="PREJUDICE")) +
      geom_smooth(aes(y= X3_violence/hatetotal  ,colour="VIOLENCE")) +
      geom_smooth(aes(y= X3_disparagement/hatetotal,colour="DISPARAGEMENT")) +
      geom_smooth(aes(y= X3_mocking/hatetotal ,colour="MOCKING")) +
      geom_smooth(aes(y= X3_other/hatetotal,colour="OTHER")) +
      geom_line(aes(y= X3_insult/hatetotal, colour="INSULT")) +
      geom_line(aes(y= X3_prejudice/hatetotal,colour="PREJUDICE")) +
      geom_line(aes(y= X3_violence/hatetotal  ,colour="VIOLENCE")) +
      geom_line(aes(y= X3_disparagement/hatetotal,colour="DISPARAGEMENT")) +
      geom_line(aes(y= X3_mocking/hatetotal ,colour="MOCKING")) +
      geom_line(aes(y= X3_other/hatetotal,colour="OTHER"))
      geom_smooth(aes(y= X3_dontknow,colour="NOINFO")) 
    
    
  #pecentage frequencies
    type <- final_count_days_cat %>%
      mutate(insult_perc = X3_insult/hatetotal) %>%
      mutate(prejudice_perc = X3_prejudice/hatetotal) %>%
      mutate(violence_perc = X3_violence/hatetotal) %>%
      mutate(disparagement_perc = X3_disparagement/hatetotal) %>%
      mutate(mocking_perc = X3_mocking/hatetotal) %>%
      mutate(other_perc = X3_other/hatetotal) %>%
      mutate(dontknow_perc = X3_dontknow/hatetotal)
    
    mean(type$insult_perc)
    mean(type$prejudice_perc)
    mean(type$violence_perc)
    mean(type$disparagement_perc)
    mean(type$mocking_perc)
    mean(type$other_perc)
    mean(type$dontknow_perc)
    
    #looking at events
      dummy <- final_count_days_cat_events %>% filter(date >= "2019-01-01") %>% filter(!is.na(incident)) %>% arrange(date) #%>% filter(date>"2019-07-01")
      
      for(i in 1:length((final_count_days_cat_events %>% filter(!is.na(incident)))$date)){
        #v<-c(1,10^0.5,10,10^1.5,100,10^2.5,1000,10^3.5,10000)
        #j<- v[i%%1]
        
       perc_plot <- perc_plot + 
          annotate("pointrange", x = dummy$date[i], y = i/100+0.8, ymin =0.1, ymax = i/100+0.8,    
                   colour = "black", size = 0.25) + 
          annotate("text", x = dummy$date[i], y = i/100+0.8, label = dummy$incident[i],angle=-45,size=3)
      }
      perc_plot
    
    
  #graph
      hate_count <- final %>%
        subset(hatenomNA == 1 | hatescale >1 | hatescaleM > 1) %>%
        group_by(date) %>% #, factor(hatescale) for language regression
        summarise(hatetotal = n(), hatescaleM=mean(hatescaleM)) %>%
        arrange(desc(hatetotal))
      final$hatescale <- as.numeric(final$hatescale)
      
      final_count_days_cat <- final %>% 
        subset(hatenomNA == 1 | hatescale >1 | hatescaleM > 1) %>%
        group_by(date) %>% 
        dplyr::summarise_at(names(final)[str_detect(names(final),"^X")], sum) %>%
        left_join(hate_count, by = "date")
      
      final_count_days_cat_events <- final_count_days_cat %>% 
        left_join(events, by="date")
   
    type_graph <- final_count_days_cat_events %>% 
      dplyr::select(date, X3_insult, X3_violence,X3_mocking,X3_disparagement,X3_prejudice) %>%
      mutate(tot = X3_insult+X3_violence+X3_mocking+X3_disparagement+X3_prejudice) %>% 
      mutate(X3_insult = X3_insult/tot, X3_violence = X3_violence/tot ,X3_mocking = X3_mocking/tot,X3_disparagement=X3_disparagement/tot,X3_prejudice = X3_prejudice /tot) %>% 
      mutate(X3_insult = (X3_insult-dplyr::lag(X3_insult)) , X3_violence = X3_violence - dplyr::lag(X3_violence) ,
             X3_mocking = X3_mocking-dplyr::lag(X3_mocking),X3_disparagement=X3_disparagement-dplyr::lag(X3_disparagement),
             X3_prejudice = X3_prejudice-dplyr::lag(X3_prejudice)) %>%
      pivot_longer(cols=X3_insult:X3_prejudice, names_prefix="^X3_",names_to="type",values_to="freq")
    
    ggplot(type_graph %>% filter(date >= "2019-01-01") 
           %>% filter(!is.na(freq)), aes(fill=type, y=freq, x=date)) + 
      geom_bar(position="dodge", stat="identity", fill = "#71CA97") + 
      scale_colour_gradient(high="#DeF7E9",low= "#71CA97")+
      theme_classic()+
      facet_grid(rows = vars(type), scales = "free_y") +
      ylab("change in percentage frequency")+
      theme(legend.position="none")
      

#################################################################
#subjects of hate
    
    #percentage distribution
    perc_plot <- plot_day_cat_events +
      geom_smooth(aes(y = hatetotal/hatetotal, colour="TOTAL")) +
      geom_line(aes(y = hatetotal/hatetotal, colour = "TOTAL")) + 
      geom_line(aes(y=X4_age/hatetotal, colour="AGE")) +
      geom_smooth(aes(y=X4_age/hatetotal, colour="AGE")) +
      geom_line(aes(y= X4_mentalphysicalcapacity/hatetotal,colour="MENTAL/PHYSICAL")) +
      geom_smooth(aes(y= X4_mentalphysicalcapacity/hatetotal,colour="MENTAL/PHYSICAL")) +
      geom_line(aes(y= X4_class/hatetotal  ,colour="CLASS")) +
      geom_smooth(aes(y= X4_class/hatetotal ,colour="CLASS")) +
      geom_line(aes(y= X4_gender/hatetotal,colour="GENDER")) +
      geom_smooth(aes(y= X4_gender/hatetotal,colour="GENDER")) +
      geom_line(aes(y= X4_race/hatetotal ,colour="RACE")) +
      geom_smooth(aes(y= X4_race/hatetotal,colour="RACE")) +
      geom_line(aes(y= X4_climatechange/hatetotal ,colour="CLIMATECHANGE")) +
      geom_smooth(aes(y= X4_climatechange/hatetotal,colour="CLIMATECHANGE")) +
      geom_line(aes(y= X4_manipulation/hatetotal,colour="MANIPULATION")) +
      geom_smooth(aes(y= X4_manipulation/hatetotal,colour="MANIPULATION"))
      
    plot_day_cat_events+
      geom_line(aes(y=X4_manip_age/hatetotal, colour="MANIP_AGE")) +
      geom_smooth(aes(y=X4_manip_age/hatetotal, colour="MANIP_AGE")) +
      geom_line(aes(y= X4_manip_cc/hatetotal,colour="MANIP_CC")) +
      geom_smooth(aes(y= X4_manip_cc/hatetotal,colour="MANIP_CC")) +
      geom_line(aes(y= X4_mental_age/hatetotal  ,colour="MENTAL_AGE")) +
      geom_smooth(aes(y= X4_mental_age/hatetotal ,colour="MENTAL_AGE")) +
      geom_line(aes(y= X4_cc_age/hatetotal,colour="CC_AGE")) +
      geom_smooth(aes(y= X4_cc_age/hatetotal,colour="CC_AGE")) +
      geom_line(aes(y= X4_manip_mental/hatetotal ,colour="MANIP_MENTAL")) +
      geom_smooth(aes(y= X4_manip_mental/hatetotal,colour="MANIP_MENTAL")) +
      geom_line(aes(y= X4_manip_cc_age/hatetotal ,colour="MANIP_CC_AGE")) +
      geom_smooth(aes(y= X4_manip_cc_age/hatetotal,colour="MANIP_CC_AGE")) +
      geom_line(aes(y= X4_manip_mental_age /hatetotal,colour="MANIP_MENTAL_AGE")) +
      geom_smooth(aes(y= X4_manip_mental_age/hatetotal,colour="MANIP_MENTAL_AGE")) +
      geom_line(aes(y= X4_gender_age/hatetotal,colour="GENDER_AGE")) +
      geom_smooth(aes(y= X4_gender_age/hatetotal,colour="GENDER_AGE")) +
      geom_line(aes(y= X4_manip_class/hatetotal,colour="MANIP_CLASS")) +
      geom_smooth(aes(y= X4_manip_class/hatetotal,colour="MANIP_CLASS"))  +
      geom_line(aes(y= X4_mental_cc/hatetotal,colour="MENTAL_CC")) +
      geom_smooth(aes(y= X4_mental_cc/hatetotal,colour="MENTAL_CC"))  +
      geom_line(aes(y= X4_gender_manip_age/hatetotal,colour="GENDER_MANIP_AGE")) +
      geom_smooth(aes(y= X4_gender_manip_age/hatetotal,colour="GENDER_MANIP_AGE")) +
      geom_line(aes(y= X4_gender_manip/hatetotal,colour="GENDER_MANIP")) +
      geom_smooth(aes(y= X4_gender_manip/hatetotal,colour="GENDER_MANIP"))   +
      geom_line(aes(y= X4_class_age/hatetotal,colour="CLASS_AGE")) +
      geom_smooth(aes(y= X4_class_age/hatetotal,colour="CLASS_AGE")) +
      geom_line(aes(y= X4_other/hatetotal,colour="OTHER")) +
      geom_smooth(aes(y= X4_other/hatetotal,colour="OTHER")) +
      geom_line(aes(y= X4_dontknow/hatetotal,colour="NOINFO")) +
      geom_smooth(aes(y= X4_dontknow/hatetotal,colour="NOINFO")) 
  
  #percentage frequencies  
    content <- final_count_days_cat %>%
      mutate(age_perc = X4_age/hatetotal) %>%
      mutate(mental_perc = X4_mentalphysicalcapacity/hatetotal) %>%
      mutate(class_perc = X4_class/hatetotal) %>%
      mutate(gender_perc = X4_gender/hatetotal) %>%
      mutate(cc_perc = X4_climatechange/hatetotal) %>%
      mutate(manip_perc = X4_manipulation/hatetotal) %>%
      mutate(race_perc = X4_race/hatetotal) %>%
      mutate(other_perc = X4_other/hatetotal) %>%
      mutate(manip_age_perc = X4_manip_age/hatetotal) %>%
      mutate(manip_cc_perc = X4_manip_cc/hatetotal) %>%
      mutate(mental_age_perc = X4_mental_age/hatetotal) %>%
      mutate(cc_age_perc = X4_cc_age/hatetotal) %>%
      mutate(manip_mental_perc = X4_manip_mental/hatetotal) %>%
      mutate(manip_cc_age_perc = X4_manip_cc_age/hatetotal) %>%
      mutate(manip_mental_age_perc = X4_manip_mental_age/hatetotal) %>%
      mutate(gender_age_perc = X4_gender_age/hatetotal) %>%
      mutate(manip_class_perc = X4_manip_class/hatetotal) %>%
      mutate(mental_cc_perc = X4_mental_cc/hatetotal) %>%
      mutate(gender_manip_age_perc = X4_gender_manip_age/hatetotal) %>%
      mutate(gender_manip_perc = X4_gender_manip/hatetotal) %>%
      mutate(class_age_perc = X4_class_age/hatetotal) %>%
      mutate(dontknow_perc = X4_dontknow/hatetotal)
      
      mean(content$age_perc)
      mean(content$mental_perc)
      mean(content$class_perc)
      mean(content$gender_perc)
      mean(content$cc_perc)
      mean(content$other_perc)
      mean(content$manip_perc)
      mean(content$race_perc)
      mean(content$manip_age_perc)
      mean(content$manip_cc_perc)
      mean(content$mental_age_perc)
      mean(content$cc_age_perc)
      mean(content$manip_mental_perc)
      mean(content$manip_cc_age_perc)
      mean(content$manip_mental_age_perc)
      mean(content$gender_age_perc)
      mean(content$manip_class_perc)
      mean(content$mental_cc_perc)
      mean(content$gender_manip_age_perc)
      mean(content$gender_manip_perc)
      mean(content$class_age_perc)
      mean(content$dontknow_perc)
    
  #intersectionality gender and age - parallel    
    plot_day_cat_events +
      geom_line(aes(y = (X4_gender - X4_gender_age), colour="gender-gender_age"))+
      geom_line(aes(y = X4_gender, colour = "gender"))+
      geom_line(aes(y = X4_gender_age, colour = "intersectional"))
    
    
    ###look at changes
    plot_subject <- final_count_days_cat_events %>% 
      filter(date >= "2019-01-01") %>%
      dplyr::select(date, X4_age, X4_manipulation, X4_climatechange, 
             X4_mentalphysicalcapacity, X4_class, X4_gender, X4_race, 
             X4_manip_age, X4_mental_age, X4_manip_cc, X4_cc_age, 
             X4_manip_mental, X4_gender_age, X4_manip_class) %>%
      mutate(tot = X4_age + X4_manipulation + X4_climatechange + X4_mentalphysicalcapacity + X4_class + X4_gender + X4_race + 
               X4_manip_age + X4_mental_age + X4_manip_cc + X4_cc_age + X4_manip_mental + X4_gender_age + X4_manip_class) %>% 
      mutate(X4_age = X4_age/tot, 
             X4_manipulation = X4_manipulation/tot ,
             X4_climatechange = X4_climatechange/tot,
             X4_mentalphysicalcapacity=X4_mentalphysicalcapacity/tot,
             X4_class = X4_class /tot, 
             X4_gender = X4_gender /tot, 
             X4_race = X4_race /tot, 
             X4_manip_age = X4_manip_age /tot, 
             X4_mental_age = X4_mental_age /tot, 
             X4_manip_cc = X4_manip_cc /tot, 
             X4_cc_age = X4_cc_age /tot, 
             X4_manip_mental = X4_manip_mental /tot, 
             X4_gender_age = X4_gender_age /tot, 
             X4_manip_class = X4_manip_class /tot) %>% 
    mutate(X4_age = (X4_age-mean(X4_age)) , 
           X4_manipulation = X4_manipulation - mean(X4_manipulation) ,
           X4_climatechange = X4_climatechange-mean(X4_climatechange),
           X4_mentalphysicalcapacity=X4_mentalphysicalcapacity-mean(X4_mentalphysicalcapacity),
           X4_class = X4_class-mean(X4_class), 
           X4_gender = X4_gender-mean(X4_gender), 
           X4_race = X4_race-mean(X4_race), 
           X4_manip_age = X4_manip_age-mean(X4_manip_age), 
           X4_mental_age = X4_mental_age-mean(X4_mental_age), 
           X4_manip_cc = X4_manip_cc-mean(X4_manip_cc),
           X4_cc_age = X4_cc_age-mean(X4_cc_age), 
           X4_manip_mental = X4_manip_mental-mean(X4_manip_mental), 
           X4_gender_age = X4_gender_age-mean(X4_gender_age), 
           X4_manip_class = X4_manip_class-mean(X4_manip_class)) %>%   
      pivot_longer(cols=X4_age:X4_manip_class, names_prefix="^X4_",names_to="type",values_to="freq") 
    
    
    plot_subject <- ggplot(plot_subject %>% filter(!is.na(freq)) %>% 
            dplyr::filter(type != "manip_class" & type != "manip_mental" & type != "cc_age"), 
           aes(fill=type, y=freq, x=date)) + 
      geom_bar(position="dodge", stat="identity", fill = "black") +
      theme_classic()+
      facet_grid(rows = vars(type), scales = "free_y") +
      theme_classic()+
      ylab("percentage frequency - average percentage")+
      theme(legend.position="bottom")+
      theme(strip.text = element_text(size = 7))
    
    
    dummy <- final_count_days_cat_events %>% filter(date >= "2019-01-01") %>% filter(!is.na(incident)) #%>% arrange(date) #%>% filter(date>"2019-07-01")
    
    for(i in 1:length((final_count_days_cat_events %>% filter(!is.na(incident)))$date)){
            plot_subject <- plot_subject + 
        ggplot2::annotate("pointrange", x = dummy$date[i], y = 0.1, ymin =0, ymax = 0.1,    
                 colour = "#71CA97", size = 0.000000)  
        #annotate("text", x = dummy$date[i], y = i/100+0.1, label = dummy$incident[i],angle=-45,size=2.5)
    }
    plot_subject

##########################################
    #retweets
    
    retweet_fav <- final %>% dplyr::select(retweets_count,favorites_count,hatenomNA, date) %>% 
      group_by(hatenomNA) %>% 
      summarise(sumhate = sum(hatenomNA), sumRT = sum(retweets_count),meanRT = mean(retweets_count), sumFA = sum(favorites_count),meanFA = mean(favorites_count))
    
