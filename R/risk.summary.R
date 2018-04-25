risk.summary <-
function(data, Raw_Ind=1){
    ### for further analysis, create output in a data frame that included age, duration of the projection time interval, covariates and the projected risk
    check_cov <- recode.check(data, Raw_Ind)
    CharRace <- as.character(check_cov$CharRace)
    RR_Star  <- relative.risk(data,Raw_Ind)
    RR_Star1 <- RR_Star$RR_Star1
    RR_Star2 <- RR_Star$RR_Star2
    AbsRisk <- absolute.risk(data, Raw_Ind, 0)
    AbsRisk_Avg <- absolute.risk(data, Raw_Ind, 1)
    time_intvl <- data$T2 - data$T1
    time_intvl <- round(time_intvl,3)
    risk_table <- cbind(data$ID, data$T1, data$T2, time_intvl, data$N_Biop, data$HypPlas, data$AgeMen, data$Age1st, data$N_Rels, data$Race, CharRace, RR_Star1, RR_Star2, AbsRisk, AbsRisk_Avg)
    colnames(risk_table) <- c("ID", "T1", "T2", "Proj_Intvl", "N_Biop", "HypPlas", "AgeMen", "Age1st", "N_Rels", "Race", "CharRace", "RR_Star1", "RR_Star2", "AbsRisk", "AbsRisk_Avg")
    risk_table<-data.frame(risk_table, row.names=NULL)
    #write.csv(risk_table, "risk_summary.csv", row.names=F)
    return(risk_table)     
}
