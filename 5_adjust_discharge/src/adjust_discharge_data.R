adjust_discharge_data <- function(flow.dat, site.dat) {
  
  site.dat$rules <- site.dat$adjustment
  site.dat$rules[grep('scaling', site.dat$rules, ignore.case = TRUE)] <- 'scale'
  site.dat$rules[grep('regression', site.dat$rules, ignore.case = TRUE)] <- 'regression'
  site.dat$rules[grep('interpolation', site.dat$rules, ignore.case = TRUE)] <- 'interpolate'
  
  flow.revised <- list()
  sites <- site.dat$Q_1
  for (i in 1:length(sites)) {
    
    if (site.dat$rules[i] == "As is") {
      flow <- subset(flow.dat, site_no == sites[i])
      flow$sample_site <- site.dat$SITE[i]
      flow.revised[[i]] <- flow
      next
    }
    
    if (site.dat$rules[i] == "scale") {
      flow <- subset(flow.dat, site_no == sites[i])
      flow$Flow <- site.dat$DA_scale*flow$Flow
      flow$sample_site <- site.dat$SITE[i]
      flow.revised[[i]] <- flow
    }
    
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
      # if they don't, create a lm between the adjust flow and the 
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
      
      flow.interpolated <- flow %>%
        select(Date, Flow, Flow_cd) %>%
        mutate(agency_cd = "USGS") %>%
        mutate(site_no = site.dat$Q_1[i]) %>%
        mutate(sample_site = site.dat$SITE[i]) %>%
        select(agency_cd, site_no, Date, Flow, Flow_cd, sample_site)
      
      flow.revised[[i]] <- flow.interpolated
    }
  
        
    }
  
}