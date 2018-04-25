error.table <-
function(data, Raw_Ind=1){
    ### list all parameters for IDs with missing AbsRisk
    check_cov <- recode.check(data, Raw_Ind)
    Error_Ind <- check_cov$Error_Ind
    RR_Star <- relative.risk(data,Raw_Ind)
    RR_Star1 <- RR_Star$RR_Star1
    RR_Star2 <- RR_Star$RR_Star2
    PatternNumber <- RR_Star$PatternNumber
    AbsRisk <- absolute.risk(data, Raw_Ind, Avg_White=0)
    R_Hyp <- as.numeric(as.character(check_cov$R_Hyp))
    NB_Cat <- as.character(check_cov$NB_Cat)
    AM_Cat <- as.character(check_cov$AM_Cat)
    AF_Cat <- as.character(check_cov$AF_Cat)
    NR_Cat <- as.character(check_cov$NR_Cat)
    CharRace <- as.character(check_cov$CharRace)

    set_T1_missing     <- as.numeric(as.character(check_cov$set_T1_missing))
    set_T2_missing     <- as.numeric(as.character(check_cov$set_T2_missing)) 
    set_HyperP_missing <- as.character(check_cov$set_HyperP_missing)
    set_R_Hyp_missing  <- as.character(check_cov$set_R_Hyp_missing)

    if (length(which(Error_Ind!=0))==0){
        error_table <- NULL
        print("NO ERROR!") 
    }
    if (length(which(Error_Ind!=0))!=0){
        error_table <- array("*", dim=c(2*length(which(is.na(AbsRisk))),14))
        origin_update <- cbind(data$ID, data$T1, data$T2, data$N_Biop, data$HypPlas, R_Hyp, data$AgeMen, data$Age1st, data$N_Rels, data$Race, RR_Star1, RR_Star2, AbsRisk, PatternNumber)
        compare_update <- cbind(data$ID, set_T1_missing, set_T2_missing, NB_Cat, set_HyperP_missing, set_R_Hyp_missing, AM_Cat, AF_Cat, NR_Cat, CharRace, RR_Star1, RR_Star2, AbsRisk, PatternNumber)
        ID_update <- which(is.na(AbsRisk))
        origin_update <- origin_update[ID_update,]
        compare_update <- compare_update[ID_update,]
        if (length(ID_update)==1){
            error_table[1,] <- origin_update
            error_table[2,] <- compare_update
            error_table[2, 11:14] <- ""
        }
        if (length(ID_update)>1){
            for (i in 1:length(ID_update)){
                 error_table[2*i-1,] <- origin_update[i,]
                 error_table[2*i,] <- compare_update[i,]
                 error_table[2*i, 11:14] <- ""
            }
        }
        colnames(error_table) <- c("ID", "T1", "T2", "N_Biop", "HypPlas", "R_Hyp", "AgeMen", "Age1st", "N_Rels", "Race", "RR_Star1", "RR_Star2", "AbsRisk", "PatternNum")
        error_table<-data.frame(error_table, row.names=NULL)
        return(error_table)
    }
}
