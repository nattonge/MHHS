#Loading everything you need
setwd("/Users/kayleighfenton/R_Folder/HonorsThesis")
View("First_DataMHHS")
library(psych)
library(tidyverse)
library(apaTables)
library(tableone)
library(dplyr)
library(purrr)



First_DataMHHS<-read.csv("kdata219.csv")

#Separating into Minorities 
  mhhs_mindemo <- First_DataMHHS %>%
    filter(Race_Ethn2 != "White" & Race_Ethn2 != "Not answered" & 
             (AC1 == 1 | AC4 == 1) & (AC2 == 6 | AC3 == 3)&(Duration_in_seconds >= 1200 & Duration_in_seconds < 71649)) %>%
    select(
      Progress,Finished,Duration_in_seconds, AC1:AC4,
      EDUC_Self,
      Income,
      Race_Ethn2,
      GHSQL_Pers_1:GHSQL_Pers_10,
      GHSQL_SUI_1:GHSQL_SUI_10,
      ISMI29_1:ISMI29_29,
      TPS_1:TPS_11,
      EIS_Ethni_1:EIS_Ethni_9,
      BICII_1:BICII_10,
      TRS_1:TRS_8, DASS21_1:DASS21_21)%>%
    mutate(
      Dass21_Total = rowSums(select(., DASS21_1:DASS21_21)) * 2,
      Dass21_Stress = rowSums(select(., DASS21_1, DASS21_6, DASS21_8, DASS21_11, DASS21_12, DASS21_14, DASS21_18), na.rm = TRUE),
      Dass21_Anxiety = rowSums(select(., DASS21_2, DASS21_4, DASS21_7, DASS21_9, DASS21_15, DASS21_19, DASS21_20), na.rm = TRUE),
      Dass21_Depression = rowSums(select(., DASS21_3, DASS21_5, DASS21_10, DASS21_13, DASS21_16, DASS21_17, DASS21_21), na.rm = TRUE),
      Dep_cuts = case_when(
        Dass21_Depression < 10 ~ "Normal",
        Dass21_Depression < 14 ~ "Mild",
        Dass21_Depression < 21 ~ "Moderate",
        Dass21_Depression < 28 ~ "Severe",
        Dass21_Depression > 27 ~ "Extremely_Severe"
      ),
      Anx_cuts = case_when(
        Dass21_Anxiety < 8 ~ "Normal",
        Dass21_Anxiety < 10 ~ "Mild",
        Dass21_Anxiety < 15 ~ "Moderate",
        Dass21_Anxiety < 20 ~ "Severe",
        Dass21_Anxiety > 19 ~ "Extremely_Severe"
      ),
      Stress_cuts = case_when(
        Dass21_Stress < 15 ~ "Normal",
        Dass21_Stress < 19 ~ "Mild",
        Dass21_Stress < 26 ~ "Moderate",
        Dass21_Stress < 34 ~ "Severe",
        Dass21_Stress > 33 ~ "Extremely_Severe"
      )) %>%
    filter(Dep_cuts != "Normal" | Anx_cuts != "Normal")
      view(mhhs_mindemo)
      CreateTableOne(data=mhhs_mindemo)
      
mhhsminboxplot<-boxplot(mhhs_mindemo$Duration_in_seconds)$stats

medianmin_outlier_value <- median(boxplot(mhhs_mindemo$Duration_in_seconds)$out)
meta_data_minmhhsmedianfilter <- mhhs_mindemo %>%
  filter(Duration_in_seconds < 71649)
filtered_minmean <- mean(meta_data_minmhhsmedianfilter$Duration_in_seconds)
mhhsminboxplot<-boxplot(meta_data_minmhhsmedianfilter$Duration_in_seconds)$stats


        
#GHSQL        
    ghsql<-mhhs_mindemo %>% select(contains("GHSQL"))
    
    ghsql<-ghsql %>% mutate(General=rowSums((select(ghsql,GHSQL_Pers_1:GHSQL_Pers_9,GHSQL_SUI_1:GHSQL_SUI_9)))) %>% 
      rowwise() %>%  
      mutate(Personal=sum(c_across(GHSQL_Pers_1:GHSQL_Pers_8)),
             Suicide=sum(c_across(GHSQL_SUI_1:GHSQL_SUI_8)),
             Informal_all=sum(c(GHSQL_Pers_1,GHSQL_SUI_1,GHSQL_Pers_2,GHSQL_SUI_2,GHSQL_Pers_3,GHSQL_SUI_3,GHSQL_Pers_4,GHSQL_SUI_4)),
             Formal_all=sum(c(GHSQL_Pers_5,GHSQL_SUI_5,GHSQL_Pers_6,GHSQL_SUI_6,GHSQL_Pers_7,GHSQL_SUI_7,GHSQL_Pers_8,GHSQL_SUI_8)),
             None_all=sum(c(GHSQL_Pers_9,GHSQL_SUI_9))) %>% 
      ungroup() %>% #remove rowwise grouping 
      mutate(Personal_z=scale(Personal),Suicide_z=scale(Suicide),Informal_all_z=scale(Informal_all),Formal_all_z=scale(Formal_all),None_all_z=scale(None_all))%>%
      mutate(
        FormalOfrank_transformation_z = scale(FormalOfrank_transformation)
      )
    
    #ASSESSINGNORMALITY
          ggplot(data = ghsql, aes(x = Formal_all_z)) +
            geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
            labs(title = "Histogram of Formal Z",
                 x = "Formal Z",
                 y = "Frequency")
          shapiro.test(ghsql$Formal_all_z) NEEDSTRANSFORMED BIMODAL DIST
            data:  ghsql$Formal_all_z
            W = 0.9551, p-value = 0.000269
            
            #TRANSFORMINGFORMAL FORMALRANKWILLBEUSED 
            FormalOfrank_transformation <- qnorm((rank(ghsql$Formal_all, na.last = "keep") - 0.5) / length(ghsql$Formal_all))
            
            hist(FormalOfrank_transformation, breaks = 20, main = "Histogram of Rank-based Transformed Formal_all")
              view(Formalrank_transformation)
       
            shapiro.test(FormalOfrank_transformation)
              data:  Formalrank_transformation
              W = 0.98165, p-value = 0.07386
                #NOTUSINGLOGTRANSFORMATIONDUETOFAILURE 
                Formallog_transformed <- log(ghsql$Formal_all + 1)  # Adding 1 to avoid log(0)
                shapiro.test(Formallog_transformed)
                  data:  Formallog_transformed
                  W = 0.94437, p-value = 4.009e-05
          
          ggplot(data = ghsql, aes(x = Informal_all_z)) +
            geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
            labs(title = "Histogram of Informal Z",
                 x = "Informal Z",
                 y = "Frequency")
          shapiro.test(ghsql$Informal_all_z) NORMAL 
            data:  ghsql$Informal_all_z
            W = 0.98542, p-value = 0.1726
          
    
    #make a dataframe with only the items to be scored/that need reliability
    ghsql_scoring<-ghsql %>% dplyr::select(1:9,12:20)
    key_ghsql.list<-list(General=c(1:18),Personal=c(1:9),Suicide=c(10:18),Formal=c(5,14,6,15,7,16,8,17),Informal=c(1,10,2,11,3,12,4,13))
    key_ghsql<-make.keys(18,key_ghsql.list,colnames(ghsql_scoring))
    
    scales_ghsql<-scoreItems(key_ghsql.list,ghsql_scoring)
    summary(scales_ghsql)
    
    #histogram to explore the range 
    ghsql %>% select(GHSQL_Pers_1:GHSQL_Pers_9) %>% gather() %>% ggplot(aes(y=value,x=key)) + geom_boxplot() + scale_x_discrete(labels=c("Intimate partner", "Friend","Parent","Other relative","MHP","Phone helpline","Doctor","Minister","Not seek help"))+ theme(axis.text.x = element_text(angle=90))
    
    ghsql %>% select(GHSQL_SUI_1:GHSQL_SUI_9) %>% gather() %>% ggplot(aes(y=value,x=key)) + geom_boxplot() + scale_x_discrete(labels=c("Intimate partner", "Friend","Parent","Other relative","MHP","Phone helpline","Doctor","Minister","Not seek help"))+ theme(axis.text.x = element_text(angle=90))
    
    ghsql %>% select(Informal_all_z,Formal_all_z,None_all_z) %>% gather() %>% ggplot(aes(y=value,x=key)) + geom_boxplot() + scale_x_discrete(labels=c("Informal (Pers + Sui)","Formal (Pers + Sui)","None (Pers + Sui)"))+ theme(axis.text.x = element_text(angle=90))
    
    ghsql %>% select(GHSQL_Pers_10_TEXT,GHSQL_SUI_10_TEXT) %>% filter(GHSQL_Pers_10_TEXT!=""|GHSQL_SUI_10_TEXT!="")
    
#ISMI29
      29-item measure assessing self stigma of mental illness. Includes subscales: alienation, stereotype endorsement, discrimination experience, social withdrawal, stigma resistance. Scored from 1-4. The entire Stigma resistance subscale is reverse scored.
      
      Traditional scoring is dichotomous and uses means. 
          
          ```{r ISMI 29}
          ismi<-mhhs_mindemo %>% select(contains("ISMI"))
          
          ismi<-ismi %>% rowwise() %>%  
            mutate(ISMI_Alientation=sum(ISMI29_1,ISMI29_5,ISMI29_8,ISMI29_16,ISMI29_17,ISMI29_21, na.rm = TRUE),
                   ISMI_Stere=sum(ISMI29_2, ISMI29_6,ISMI29_10,ISMI29_18,ISMI29_19,ISMI29_23,ISMI29_29, na.rm = TRUE),
                   ISMI_Discr=sum(ISMI29_3,ISMI29_15,ISMI29_22,ISMI29_25,ISMI29_28, na.rm = TRUE),
                   ISMI_Withdrawal=sum(ISMI29_4,ISMI29_9,ISMI29_11,ISMI29_12,ISMI29_13,ISMI29_20, na.rm = TRUE)) %>% 
            mutate(ISMI29_7=recode(ISMI29_7,`1`=4,`2`=3,`3`=2,`4`=1),
                   ISMI29_14=recode(ISMI29_14,`1`=4,`2`=3,`3`=2,`4`=1),
                   ISMI29_24=recode(ISMI29_24,`1`=4,`2`=3,`3`=2,`4`=1),
                   ISMI29_26=recode(ISMI29_26,`1`=4,`2`=3,`3`=2,`4`=1),
                   ISMI29_27=recode(ISMI29_27,`1`=4,`2`=3,`3`=2,`4`=1)) %>%  
            mutate(ISMI_Stigmar=sum(ISMI29_7,ISMI29_14,ISMI29_24,ISMI29_26,ISMI29_27)) %>% 
            ungroup() %>% 
            mutate(
              ISMIAlien_z = scale(ISMI_Alientation),
              ISMIStere_z = scale(ISMI_Stere),
              ISMIDiscr_z = scale(ISMI_Discr),
              ISMIWithDr_z = scale(ISMI_Withdrawal),
              ISMIStigR_z = scale(ISMI_Stigmar)
            ) %>%
            mutate(
              Composite_ISMIZ_Score = rowSums(select(., ends_with("_z")), na.rm = TRUE))
          
          
       view(ismi)   
      #ASSESSINGISMINORMALITY 
       ggplot(ismi, aes(x = Composite_ISMIZ_Score)) +
         geom_density(fill = "skyblue", color = "black") +
         labs(title = "Density Plot of Composite ISMI-Z Score",
              x = "Composite ISMI_Z_Score",
              y = "Density")
       shapiro.test(ismi$Composite_ISMIZ_Score) NORMAL
         data:  ismi$Composite_ISMIZ_Score
         W = 0.99393, p-value = 0.8414 
           
          #make a dataframe with only the items to be scored/that need reliability
          ismi_scoring<-ismi %>% dplyr::select(ISMI29_1:ISMI29_29)
          key_ismi.list<-list(Alien=c(1,5,8,16,17,21),Stereotype=c(2,6,10,18,19,23,29),Discrimination=c(3,15,22,25,28),Withdrawal=c(4,9,11,12,13,20),StigmaR=c(7,14,24,26,27))
          
          key_ismi<-make.keys(29,key_ismi.list,colnames(ismi_scoring))
          
          scales_ismi<-scoreItems(key_ismi.list,ismi_scoring)
          summary(scales_ismi)
          
          ismi$ISMI29_14
          cor(ismi$ISMI_Stigmar,ismi$ISMI_Alientation,use="complete.obs")
          
          ismi %>% select(ISMI_Alientation,ISMI_Stere,ISMI_Discr,ISMI_Withdrawal,ISMI_Stigmar) %>% gather() %>%  ggplot(aes(y=value,x=key,color=value))+geom_boxplot()+geom_jitter()
          
          ```
          For some reason, the rescored stigma resistance is a negative correlation

#Trust respect scale
      8 questions 1-strongly disagree; 7 - strongly agree
      2,3,5,6 reverse-scored
    
    ```{r TRS}
    trs <- mhhs_mindemo %>% 
      select(contains("trs")) %>%
      mutate(
        across(
          c(TRS_2, TRS_3, TRS_5, TRS_6),
          ~ 8 - .
        ),
        TRS_Tot = rowSums(select(., starts_with("TRS")))
      ) %>%
      mutate(TRS_Z = scale(TRS_Tot)) %>%
      mutate(
        TRS_Tot_rank = rank(TRS_Tot),
        TRS_Tot_quantile = qnorm((TRS_Tot_rank - 0.5) / length(TRS_Tot_rank)),
        TRS_Tot_transformedZ = scale(TRS_Tot_quantile)
      )
    #AssessingTRSTransformed
      shapiro.test(trs$TRS_Tot_transformedZ) GOOD 
          data:  trs$TRS_Tot_transformedZ
          W = 0.99614, p-value = 0.9784
      ggplot(trs, aes(x = TRS_Tot_transformedZ)) +
        geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
        labs(title = "Histogram of TRS_Transformed",
             x = "TRS_Transformed Score",
             y = "Frequency")

    #ASSESSINGOriginalTRSNORMALITY
      ggplot(trs, aes(x = TRS_Z)) +
        geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
        labs(title = "Histogram of TRS_Z Score",
             x = "TRS_Z Score",
             y = "Frequency")
      shapiro.test(trs$TRS_Z)  NEEDSTRANSFORMATION
        data:  trs$TRS_Z
        W = 0.95543, p-value = 0.0003219
    
              
#Ethnic Identity Scale

reverse score 4,5,8
    
    ```{r EIS}
    eis<-mhhs_mindemo %>% select(contains("eis"), contains("Race_Ethn2"))
    
    # identity questions
    eis_resp<-eis %>% select(EIS_Ethni_1:EIS_Ethni_9)
 
    
    # selecting responses only
    eis_resp<-eis_resp %>% mutate(across(c(EIS_Ethni_4,EIS_Ethni_5,EIS_Ethni_8),~5-.)) %>% 
      rowwise() %>% mutate(EIS_Tot=sum(c_across(EIS_Ethni_1:EIS_Ethni_9))) %>% 
      ungroup() %>% mutate(EIS_Z=scale(EIS_Tot))
    
    # Create histogram
    ggplot(eis_resp, aes(x = EIS_Tot)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = "Histogram of EIS_Tot Scores",
           x = "EIS_Tot Scores",
           y = "Frequency")
    
    eis_scoring<-eis_resp %>% dplyr::select(EIS_Ethni_1:EIS_Ethni_9)
    key_eis.list<-list(Total=c(1:9))
    key_eis<-make.keys(9,key_eis.list,colnames(eis_scoring))
    
    scales_eis<-scoreItems(key_eis.list,eis_scoring)
    summary(scales_eis)
    
    eis_resp %>% select(EIS_Ethni_1:EIS_Ethni_9) %>% gather() %>%  ggplot(aes(y=value,x=key,color=value))+geom_boxplot()+geom_jitter()   
    
view(eis_resp)
    

#BICII
      Harmony: 1, 2*, 3, 6 Higher scores mean higher harmony (affective)
      Conflict: 7, 8*, 11, 12, 13, 14
    bicii<-mhhs_mindemo %>%   select(contains("bicii"), contains("Race_Ethn2"))
    
    BICII <- bicii %>%
      rowwise() %>%
      mutate(BICII_Harmony = sum(c_across(BICII_1:BICII_4), na.rm = TRUE),
             BICII_5 = recode(BICII_5, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
             BICII_6 = recode(BICII_6, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
             BICII_7 = recode(BICII_7, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
             BICII_8 = recode(BICII_8, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
             BICII_9 = recode(BICII_9, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
             BICII_10 = recode(BICII_10, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)) %>%
      mutate(BICII_Conflict = sum(c_across(BICII_5:BICII_10), na.rm = TRUE)) %>% mutate(BICII_Tot=sum(BICII_Conflict,BICII_Harmony)) %>% 
      ungroup() %>% 
      mutate(BICII_Z=scale(BICII_Tot))
      view(BICII)    
    
    BICII_scoring<-BICII %>% dplyr::select(BICII_1:BICII_10)
    key_BICII.list<-list(Total=c(1:10))
    key_BICII<-make.keys(10,key_BICII.list,colnames(BICII_scoring))
    
    scales_BICII<-scoreItems(key_BICII.list,BICII_scoring)
    summary(scales_BICII)
    
#INGROUP COMPOSITE
    combined_data <- bind_cols(BICII, eis_resp)
    In_Groupcomposite<-combined_data %>% 
      rowwise() %>% mutate(In_GroupTot=sum(BICII_Z,EIS_Z)) %>% 
      ungroup() 
    #ASSESSINGCOMPOSITENORMALITY
      ggplot(data = In_Groupcomposite, aes(x = In_GroupTot)) +
      geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
      labs(title = "Histogram of In Group Composite Score",
           x = "In Group Score",
           y = "Frequency")
      shapiro.test(In_Groupcomposite$In_GroupTot) 
        NORMAL 
        data:  In_Groupcomposite$In_GroupTot
        W = 0.98656, p-value = 0.237

#MHHSMINFINAL DATA
        final_mhhsmindata <- bind_cols(mhhs_mindemo, ghsql, ismi, trs, eis_resp, BICII, combined_data, In_Groupcomposite)

        #PRIMARYREGRESSIONS
        ISMImodel <- lm(Composite_ISMIZ_Score ~ In_GroupTot, data = final_mhhsmindata)
        
        SecondaryISMImodel<- lm(composite_ISMIZ_score ~ In_GroupTot + TRS_Tot_transformedZ, data = final_mhhsmindata)
          
        FormalHelpmodel <- lm(FormalOFrank_transformation_z ~ In_GroupTot, data = final_mhhsmindata)
      
        InformalHelpmodel <- lm(Informal_all_z ~ In_GroupTot, data = final_mhhsmindata)
        
        
        
      