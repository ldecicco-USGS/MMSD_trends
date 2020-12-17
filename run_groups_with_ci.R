library(pbdMPI, quietly = TRUE)
library(EGRET, quietly = TRUE)
library(EGRETci, quietly = TRUE)

# master_list <- readr::read_csv("master_list.csv")
# merged_eLists <- readRDS("eLists_out.rds")

# master_list <- readr::read_csv("D:/LADData/MMSD_trends/6_merge/out/site_sample_flow_summary.csv")
# merged_eLists_orig <- readRDS("fromYeti/eLists_out.rds")
merged_eLists <- readRDS("6_merge/out/all_eLists.rds")
# 
# new_names <- names(merged_eLists)[!names(merged_eLists) %in% names(merged_eLists_orig)]

# So, re-running the remake stuff loses OC-02 and OC-05,
# but gains LC-05

ids <- c("OH-01_FC_combined", "OH-01_NH3",                   
         "OH-01_Total Suspended Solids", "OH-01_TP",                    
         "RI-01_BOD5", "RI-01_FC_combined",           
          "RI-01_NH3", "RI-01_Total Suspended Solids",
          "RI-01_TP", "RI-02_BOD5",                  
          "RI-02_FC_combined", "RI-02_NH3",                   
          "RI-02_Total Suspended Solids", "RI-02_TP",                    
          "RI-04_BOD5", "RI-04_FC_combined",          
          "RI-04_NH3", "RI-04_Total Suspended Solids",
          "RI-04_TP", "RI-07_BOD5",                  
          "RI-07_FC_combined", "RI-07_NH3",                   
          "RI-07_Total Suspended Solids", "RI-07_TP",                    
          "RI-09_BOD5", "RI-09_FC_combined",           
          "RI-09_NH3", "RI-09_Total Suspended Solids",
          "RI-09_TP", "RI-11_BOD5",                  
          "RI-11_FC_combined", "RI-11_NH3",                   
          "RI-11_Total Suspended Solids", "RI-11_TP",                    
          "RI-13_BOD5", "RI-13_FC_combined",           
          "RI-13_NH3", "RI-13_Total Suspended Solids",
          "RI-13_TP", "RI-14_BOD5",                  
          "RI-14_FC_combined", "RI-14_NH3",                   
          "RI-14_Total Suspended Solids", "RI-14_TP",                    
          "RI-16_BOD5",  "RI-16_FC_combined",           
          "RI-16_NH3", "RI-16_Total Suspended Solids",
          "RI-16_TP", "RI-21_BOD5",                  
          "RI-21_FC_combined", "RI-21_NH3",                  
          "RI-21_Total Suspended Solids", "RI-21_TP" )

ids_do_over <- eList_names[grep("OC-02", eList_names)]

run_job_groups <- function(eList){
  
  df <- data.frame(site = c("RI-01", "RI-02", "LC-04", "LC-05", "RI-04", "RI-07",
                            "RI-16", "RI-21", "UC-07", "HC-03", "RI-09",
                            "RI-11", "RI-13",
                            "RI-14", "OH-01", "OC-02", "OC-05", "RR-04","RR-05"),
                   inCAS = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                             FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
                             TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
                   stringsAsFactors = FALSE)
  
  min_year <- min(eList$Sample$waterYear)+1
  max_year <- max(eList$Sample$waterYear)-2
  
  eList$Sample$Q[eList$Sample$Q <= 0] <- min(eList$Sample$Q[eList$Sample$Q > 0], na.rm = TRUE)
  eList$Daily <- eList$Daily[eList$Daily$Q > 0, ]
  
  more_than_half_a_water_year <- (max(eList$Sample$DecYear) - trunc(max(eList$Sample$DecYear)) - 0.75) > 0.5
  
  id <- paste0(eList$INFO$shortName, "_", eList$INFO$paramShortName)
  
  wall <- df$inCAS[df$site == eList$INFO$shortName]
  
  cat(id)
  
  if(more_than_half_a_water_year){
    surfaceEnd <- max(eList$Sample$Date)
  } else {
    surfaceEnd <- as.Date(paste0(max(eList$Sample$waterYear)-1,
                                 "-09-30"))
  }

  does_group_1_exist <- range(eList$Sample$waterYear)
  does_group_1_exist <- any(does_group_1_exist < 1994)

  does_group_2_exist <- any(range(eList$Sample$waterYear) > 2006)
  does_group_mid_exist <- any(range(eList$Sample$waterYear > 1993 &
                                      eList$Sample$waterYear < 2003))

  # if(does_group_1_exist & does_group_2_exist){
    
    if(wall){

      groupResults2 <- runGroups(eList, windowSide = 7,
                                 group1firstYear = 1986, group1lastYear = 1993,
                                 group2firstYear = 2006, group2lastYear = 2017,
                                 surfaceEnd = surfaceEnd,
                                 flowBreak = FALSE,
                                 wall = wall,  sample1EndDate = "1993-08-08",
                                 verbose = FALSE, minNumUncen = 30)
    } else {
      groupResults2 <- runGroups(eList, windowSide = 7,
                                 group1firstYear = 1986, group1lastYear = 1993,
                                 group2firstYear = 1994, group2lastYear = 2002,
                                 surfaceEnd = surfaceEnd,
                                 flowBreak = FALSE,
                                 wall = wall, verbose = FALSE, minNumUncen = 30)
    }
  
    saveRDS(groupResults2, file = paste0("group_new/group_2_",id,".rds"))
    # groupResults2 <- readRDS(paste0("group_new/group_2_",id,".rds"))
    group_out_boot <- runGroupsBoot(eList, groupResults2)
    saveRDS(group_out_boot, file = paste0("group_new/group_2_boot_",id,".rds"))
  # } 
  
  eListOut <- eList
  return(eListOut)
}

little_list <- merged_eLists[ids_do_over ]
little_list <- eList_list[ids_do_over[-3]]

for(eList in little_list){
  run_job_groups(eList)
}

sapply(little_list, run_job_groups)

for(eList in merged_eLists[group_2_ids]){
  run_job_groups(eList)
}

merged_lists_left <- sapply(merged_eLists, function(x){
  min(x$Sample$waterYear+1) >= 1987
})

merged_lists_left <- merged_eLists[merged_lists_left]
rm(merged_eLists)

sapply(merged_lists_left, function(x){
  does_group_1_exist <- range(x$Sample$waterYear)
  does_group_1_exist <- any(does_group_1_exist < 1994)
})

with_early_data <- 
does_group_1_exist <- range(eList$Sample$waterYear)
does_group_1_exist <- any(does_group_1_exist < 1994)



init()

groups_out <- pbdLapply(merged_lists_left, run_job_groups)

finalize()
