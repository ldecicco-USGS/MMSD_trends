library(dplyr)

adjust_discharge_data <- function(flow.dat, site.dat) {
  
  site.dat$rules <- site.dat$adjustment
  site.dat$rules[grep('scaling', site.dat$rules, ignore.case = TRUE)] <- 'scale'
  site.dat$rules[grep('regression', site.dat$rules, ignore.case = TRUE)] <- 'regression'
  site.dat$rules[grep('interpolation', site.dat$rules, ignore.case = TRUE)] <- 'interpolate'
  
  flow.revised <- list()
  sites <- site.dat$Q_1
  
  # adjust KK site up front because it is used in multiple places for
  # either scaling or other adjustments
  # gage only goes to 1982, but WQ starts in 1979. 
  # start by creating relationship between honey ~ kk to 
  # backfill record
  
  kk <- flow.dat %>% 
    filter(site_no == '04087159') %>%
    rename(Flow_kk = Flow)
  
  honey <- flow.dat %>%
    filter(site_no == '04087119') %>%
    rename(Flow_honey = Flow)
  
  kk.hon <- full_join(honey, kk, by = c('agency_cd', 'Date'))
  
  mod_predict_honey <- lm(Flow_honey~Flow_kk, data = kk.hon)
  mod_predict_kk <- lm(Flow_kk~Flow_honey, data = kk.hon)
  
  new.honey <- kk.hon[is.na(kk.hon$Flow_honey), ]
  new.kk <- kk.hon[is.na(kk.hon$Flow_kk), ]

  new.honey$Flow_honey <- round(predict(mod_predict_honey, newdata = new.honey), 1)
  new.kk$Flow_kk <- round(predict(mod_predict_kk, newdata = new.kk), 1)
  
  new.honey <- new.honey %>%
    mutate(site_no = '04087119') %>%
    select(agency_cd, site_no, Date, Flow_honey, Flow_cd.y) %>%
    rename(Flow = Flow_honey, Flow_cd = Flow_cd.y)
  
  new.kk <- new.kk %>%
    mutate(site_no = '04087159') %>%
    select(agency_cd, site_no, Date, Flow_kk, Flow_cd.x) %>%
    rename(Flow = Flow_kk, Flow_cd = Flow_cd.x)
  
  flow.dat <- rbind(flow.dat, new.honey, new.kk)

  # now fill in some data for KK using a regression with Underwood
  kk <- flow.dat %>% 
    filter(site_no == '04087159') %>%
    rename(Flow_kk = Flow)
  
  unwd <- flow.dat %>%
    filter(site_no == "04087088") %>%
    rename(Flow_unwd = Flow)
  
  kk.unwd <- full_join(unwd, kk, by = c('agency_cd', 'Date')) %>%
    arrange(Date)
  
  mod <- lm(log10(Flow_kk)~log10(Flow_unwd), data = kk.unwd)
  mse.mod <- mean(mod$residuals^2)
  log.adjust <- 10^(mse.mod/2)
  
  test <- new.kk <- filter(kk.unwd, is.na(Flow_kk))
  
  new.kk <- filter(kk.unwd, is.na(Flow_kk)) %>%
    mutate(Flow = round((10^as.numeric(predict(mod, .)))*log.adjust, 1)) %>%
    mutate(Flow_cd = Flow_cd.x) %>%
    mutate(site_no = '04087159') %>%
    select(agency_cd, site_no, Date, Flow, Flow_cd)
  
  flow.dat <- rbind(flow.dat, new.kk)
  
  for (i in 1:length(sites)) {
    
    # for "As is" sites
    if (site.dat$rules[i] == "As is") {
      flow <- flow.dat %>%
        filter(site_no == sites[i]) %>%
        filter(Date >= site.dat$begin[i]) %>%
        mutate(sample_site <- site.dat$SITE[i])
      flow.revised[[i]] <- flow
      next
    }
    
    # for scale sites, simply multiple by drainage area scaling ratio
    if (site.dat$rules[i] == "scale") {
      flow <- flow.dat %>%
        filter(site_no == sites[i]) %>%
        filter(Date >= site.dat$begin[i]) %>%
        mutate(Flow = round(site.dat$DA_scale[i]*Flow, 1)) %>%
        mutate(sample_site = site.dat$SITE[i])
      flow.revised[[i]] <- flow
      next
    }
    
    # for interpolation, take the median (or mean) value
    # between the two sites
    if (site.dat$rules[i] == 'interpolate') {
      flow1 <- subset(flow.dat, site_no == site.dat$Q_1[i])
      flow2 <- subset(flow.dat, site_no == site.dat$Q_2[i])
      flow <- full_join(flow1, flow2, by = 'Date') %>%
        arrange(Date) %>%
        rowwise %>%
        mutate(Flow = median(c(Flow.x, Flow.y))) %>%
        filter(Date >= site.dat$begin[i]) %>%
        mutate(Flow_cd = paste0(Flow_cd.x, Flow_cd.y))
      
      flow$Flow_cd[grep("A e", flow$Flow_cd)] <- "A e"
      flow$Flow_cd[grep("AA", flow$Flow_cd)] <- "A"
      
      # some interpolated sites don't completely overlap
      # if they don't, create a lm between the median flow and the 
      # flow with complete data, fill in missing values
      
      if (anyNA(flow$Flow)) {
        # find which var has data
        flow.temp <- subset(flow, is.na(flow$Flow))
        
        if (anyNA(flow.temp$Flow.x)) {
          mod <- lm(Flow ~ Flow.y, data = flow)
          flow$Flow[is.na(flow$Flow)] <- as.numeric(predict(mod, newdata = flow.temp))
          flow$Flow_cd <- flow$Flow_cd.y
        } else {
          mod <- lm(Flow ~ Flow.x, data = flow)
          flow$Flow[is.na(flow$Flow)] <- as.numeric(predict(mod, newdata = flow.temp))
          flow$Flow_cd <- flow$Flow_cd.x
        }
      }
      
      # clean up merged data
      flow.interpolated <- flow %>%
        select(Date, Flow, Flow_cd) %>%
        mutate(agency_cd = "USGS") %>%
        mutate(site_no = site.dat$Q_1[i]) %>%
        mutate(sample_site = site.dat$SITE[i]) %>%
        select(agency_cd, site_no, Date, Flow, Flow_cd, sample_site)
      
      flow.revised[[i]] <- flow.interpolated
      next
    }
    
    # for regression sites, find which site has missing data, 
    # create a regression of flow.missing ~ flow.complete
    # predict missing instances of flow.missing
    if (site.dat$rules[i] == 'regression') {
      flow1 <- subset(flow.dat, site_no == site.dat$Q_1[i])
      
      if (site.dat$SITE[i] == 'RR-04'){
        # this gives the upstream site at Root River
        # a + 1 day offset to better match downstream site
        flow1$Date <- flow1$Date + 1
      }
      
      flow2 <- subset(flow.dat, site_no == site.dat$Q_2[i])
      flow <- full_join(flow1, flow2, by = 'Date') %>%
        arrange(Date) %>%
        filter(Date >= site.dat$begin[i])
      
      
      if (anyNA(flow$Flow.x)) {
        # keep observed flow.x values
        flow$Flow <- flow$Flow.x
        flow$Flow_cd <- flow$Flow_cd.x
        
        temp.flow <- subset(flow, is.na(flow$Flow))
        
        # create relationship between flow x and y
        mod <- lm(log10(Flow.x) ~ log10(Flow.y), data = flow)
        
        #mod2 <- lm(Flow.x ~ Flow.y, data = flow)
        #predict.3 <- as.numeric(predict(mod2, temp.flow))
        # adjust for prediction in log-log space per Newman 1993
        mse.mod <- mean(mod$residuals^2)
        log.adjust <- 10^(mse.mod/2)
        
        flow$Flow[is.na(flow$Flow)] <- (10^as.numeric(predict(mod, temp.flow)))*log.adjust
        flow$Flow_cd[is.na(flow$Flow_cd)] <- flow$Flow_cd.y[is.na(flow$Flow_cd)]
        
      } else {
        
        flow$Flow <- flow$Flow.y
        flow$Flow_cd <- flow$Flow_cd.y
        
        temp.flow <- subset(flow, is.na(flow$Flow))
        
        # create relationship between flow x and y
        mod <- lm(log10(Flow.y) ~ log10(Flow.x), data = flow)
        mse.mod <- mean(mod$residuals^2)
        log.adjust <- 10^(mse.mod/2)
        
        flow$Flow[is.na(flow$Flow)] <- (10^as.numeric(predict(mod, temp.flow)))*log.adjust
        flow$Flow_cd[is.na(flow$Flow_cd)] <- flow$Flow_cd.x[is.na(flow$Flow_cd)]
        
      }
      
      flow.regressed <- flow %>%
        select(Date, Flow, Flow_cd) %>%
        mutate(agency_cd = "USGS") %>%
        mutate(site_no = site.dat$Q_1[i]) %>%
        mutate(sample_site = site.dat$SITE[i]) %>%
        select(agency_cd, site_no, Date, Flow, Flow_cd, sample_site)
      
      flow.revised[[i]] <- flow.regressed
      next
    }
    
    if (site.dat$SITE[i] == 'OH-01') {
      flow1 <- flow.dat %>%
        filter(site_no == site.dat$Q_1[i]) %>%
        rename(FlowQ1 = Flow, FlowQ1_cd = Flow_cd)
      
      flow2 <- flow.dat %>%
        filter(site_no == site.dat$Q_2[i]) %>%
        rename(FlowQ2 = Flow, FlowQ2_cd = Flow_cd)
      
      flow3 <- flow.dat %>%
        filter(site_no == site.dat$Q_3[i]) %>%
        rename(FlowQ3 = Flow, FlowQ3_cd = Flow_cd)
      
      # note that site 4 (Jones Island) has already been corrected
      # under low flow conditions (as the sum of upstream sites)
      # so, really just need to create a relationship between sum and 
      # JI, and the fill in time gaps
      flow4 <- flow.dat %>%
        filter(site_no == site.dat$Q_4[i]) %>%
        rename(FlowQ4 = Flow, FlowQ4_cd = Flow_cd)      
      
      flows <- full_join(flow1, flow2, by = c('agency_cd', 'Date')) %>%
        full_join(flow3, by = c('agency_cd', 'Date')) %>%
        full_join(flow4, by = c('agency_cd', 'Date')) %>%
        filter(Date >= site.dat$begin[i]) %>%
        mutate(Flow_sums = rowSums(.[c('FlowQ1', 'FlowQ2', 'FlowQ3')])) %>%
        mutate(Flow = FlowQ4)    

      # model Jones Island vs sum of three sites
      mod <- lm(FlowQ4 ~ Flow_sums, data = flows)
      
      rows.replace <- which(is.na(flows$Flow)&!is.na(flows$Flow_sums))
      flows$Flow[rows.replace] <- predict(mod, newdata = flows[rows.replace,])
      
      flows$Flow_cd = flows$FlowQ4_cd
      flows$Flow_cd[rows.replace] <- paste0(flows$FlowQ1_cd[rows.replace], flows$FlowQ2_cd[rows.replace], flows$FlowQ3_cd[rows.replace])
      
      flows$Flow_cd[grep("A e", flows$Flow_cd)] <- "A e"
      flows$Flow_cd[grep("AA", flows$Flow_cd)] <- "A"
      
      flow.fixed <- flows %>%
        mutate(site_no = site.dat$Q_4[i]) %>%
        mutate(sample_site = site.dat$SITE[i]) %>%
        select(agency_cd, site_no, Date, Flow, Flow_cd, sample_site)
      
      flow.revised[[i]] <- flow.fixed
      next
  }
    
  }
  
  flow.revised.out <- rbindlist(flow.revised)
  return(flow.revised.out)
}