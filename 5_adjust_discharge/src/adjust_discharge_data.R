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

  for (i in 1:length(sites)) {
    
    # for "As is" sites
    if (site.dat$rules[i] == "As is") {
      flow <- subset(flow.dat, site_no == sites[i])
      flow$sample_site <- site.dat$SITE[i]
      flow.revised[[i]] <- flow
      next
    }
    
    # for scale sites, simply multiple by drainage area scaling ratio
    if (site.dat$rules[i] == "scale") {
      flow <- subset(flow.dat, site_no == sites[i])
      flow$Flow <- site.dat$DA_scale[i]*flow$Flow
      flow$sample_site <- site.dat$SITE[i]
      flow.revised[[i]] <- flow
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
      
      if (site.dat$SITE == 'RR-04'){
        # this gives the upstream site at Root River
        # a + 1 day offset to better match downstream site
        flow1$Date <- flow1$Date +1
      }
      
      flow2 <- subset(flow.dat, site_no == site.dat$Q_2[i])
      flow <- full_join(flow1, flow2, by = 'Date') %>%
        arrange(Date) %>%
        filter(Date >= site.dat$begin[i])
      
      flow.mod <- full_join(flow1.mod, flow2, by = 'Date') %>%
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
      
      flows <- left_join(flow1, flow2, by = c('agency_cd', 'Date')) %>%
        left_join(flow3, by = c('agency_cd', 'Date')) %>%
        left_join(flow4, by = c('agency_cd', 'Date'))
      
      flows$Flow_sums = rowSums(flows[,c('FlowQ1', 'FlowQ2', 'FlowQ3')])
      
      
      complete.flows <- subset(flows, !is.na(flows$Flow))
      complete.flows$sum <-  rowSums(complete.flows[,c('Flow.x', 'Flow.y', 'Flow')], na.rm = TRUE)
      complete.flows <- left_join(complete.flows, flow4, by = c('agency_cd', 'Date'))
      plot(complete.flows$Flow.y.y ~ complete.flows$sum)
      
      flows$sum12 <- rowSums(flows[,c('Flow.x', 'Flow.y')], na.rm = TRUE)
      plot(flows$sum12 ~ flows$Flow)
      flows$sum <- rowSums(flows[,c('Flow.x', 'Flow.y', 'Flow')], na.rm = TRUE)
      
      
      flowsup <- full_join(flow1, flow2, by = 'Date') %>%
        full_join(flow3, by = 'Date') %>%
        full_join(flow4, by = 'Date') %>%
        mutate(flow_up = rowSums('Flow.x', 'Flow.y', 'Flow.x.x', na.rm = TRUE))
        
    
      
  }
    
    
    
    
    
  }
}