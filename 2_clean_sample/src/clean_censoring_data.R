censor.dat <- function(cleaned.dat) {

  dat.censored <- dat %>%
  filter(DATE < as.POSIXct("2002-03-25")) %>%
  mutate(year = year(DATE))

# censored limits for nh3 are as follows
# (after examining low values through time)
# start - end of 1995 = 0.02
# 1996-1997 = 0.06
# 1998 - end of 2001 = 0.004 (conservatively, wobbled between 0.002 and 0.004)

dat.censored$`NH3 (mg/L)`[which(dat.censored$year < 1996 & dat.censored$`NH3 (mg/L)` <= 0.02)] <- 0.02
dat.censored$`rmk_NH3 (mg/L)`[which(dat.censored$year < 1996 & dat.censored$`NH3 (mg/L)` <= 0.02)] <- "<"

dat.censored$`NH3 (mg/L)`[which(dat.censored$year >= 1996 & dat.censored$year <= 1997 & dat.censored$`NH3 (mg/L)` <= 0.06)] <- 0.06
dat.censored$`rmk_NH3 (mg/L)`[which(dat.censored$year >= 1996 & dat.censored$year <= 1997 & dat.censored$`NH3 (mg/L)` <= 0.06)] <- "<"

dat.censored$`NH3 (mg/L)`[which(dat.censored$year > 1997  & dat.censored$`NH3 (mg/L)` <= 0.004)] <- 0.004
dat.censored$`rmk_NH3 (mg/L)`[which(dat.censored$year > 1997 & dat.censored$`NH3 (mg/L)` <= 0.004)] <- "<"

# 3 is the most common censoring level for FC, so will 
# add this as a censoring level pre-2002
# March 25, 2002 seems to be the earliest date with
# documented censoring, so only need to apply this to FC_combined
# and FC_MPN

dat.censored$`rmk_FC (MPN/100mL)`[which(dat.censored$`FC (MPN/100mL)`<=3)] = "<"
dat.censored$`FC (MPN/100mL)`[which(dat.censored$`FC (MPN/100mL)`<=3)] = 3

dat.censored$rmk_FC_combined[which(dat.censored$FC_combined<=3)] = "<"
dat.censored$FC_combined[which(dat.censored$FC_combined<=3)] = 3  

# BOD 5 should be censored at 2 throughout entire record

dat.censored$`rmk_BOD5 (mg/L)`[which(dat.censored$`BOD5 (mg/L)`<=2)] = "<"
dat.censored$`BOD5 (mg/L)`[which(dat.censored$`BOD5 (mg/L)`<=2)] = 2

# TP has multiple levels throughout that aren't clear
# going to conservatively set the last 2002 level (0.01)
# as the censoring level pre-2002

dat.censored$`rmk_TP (mg/L)`[which(dat.censored$`TP (mg/L)`<= 0.011)] = "<"
dat.censored$`TP (mg/L)`[which(dat.censored$`TP (mg/L)`<= 0.011)] = 0.011

# TSS
# level once censoring is documented = 1
# Censoring looks like it may jump up to 2 from 1993-2002
# Censroing at 3 from 1979 through 1992

dat.censored$`rmk_Total Suspended Solids (mg/L)`[which(dat.censored$DATE >= as.POSIXct("1993-01-01") & dat.censored$`Total Suspended Solids (mg/L)` <= 2)] = "<"
dat.censored$`Total Suspended Solids (mg/L)`[which(dat.censored$DATE >= as.POSIXct("1993-01-01") & dat.censored$`Total Suspended Solids (mg/L)` <= 2)] = 2

dat.censored$`rmk_Total Suspended Solids (mg/L)`[which(dat.censored$DATE < as.POSIXct("1993-01-01") & dat.censored$`Total Suspended Solids (mg/L)` <= 3)] = "<"
dat.censored$`Total Suspended Solids (mg/L)`[which(dat.censored$DATE < as.POSIXct("1993-01-01") & dat.censored$`Total Suspended Solids (mg/L)` <= 3)] = 3

# put everything back together

dat.final <- dat %>%
  filter(DATE >= as.POSIXct("2002-03-25")) %>%
  bind_rows(dat.censored)
  
return(dat.final)
}