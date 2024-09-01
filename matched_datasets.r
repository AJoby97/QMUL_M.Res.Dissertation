library(MatchIt) # This package includes several popular approaches to matching 
#library(pacman) # This package  is an R package management tool
library(knitr) # Provides a general-purpose tool for dynamic report generation in R
library(tableone) # This package can summarize both continuous and categorical variables mixed within one table
#library(captioner) # This package allows you to store figure and table captions and print them later.
library(zoo)
library(lmtest) # this is required for coeftest function 
library(sandwich) # this is required for the vcovCL function
#library(optmatch)
#library(cobalt)
library(ggplot2)
library(dplyr)

file <- "/genesandhealth/red/AshithaJoby/results/Matched_data_files/GNH/before_matching.csv"
mydata<-read.csv(file,header=T)

filtered_data_control<-mydata %>%
  select("PseudoNHSnumber","Group","Gender","Ethnicity","Index_age")%>%
  filter(Group==0)

column_names<-colnames(mydata)
print(column_names)
cancer_columns<-c('Oro.pharyngeal_Cancer',
                 'Skin..other._Cancer',
                 'Biliary.tract_Cancer',
                 'Bladder_Cancer',
                 'Bone_Cancer',
                 'CNS_Cancer',
                 'Breast_Cancer',
                 'Cervical_Cancer',
                 'Pancreatic_Cancer',
                 'Prostate_Cancer',
                 'Colorectal_Cancer',
                 'Kidney_Cancer',
                 'Liver_Cancer',
                 'Stomach_Cancer',
                 'Testicular_Cancer',
                 'Thyroid_Cancer',
                 'Uterine_Cancer',
                 'Ovarian_Cancer',
                 'Lung_Cancer',
                 'Melanoma_Cancer',
                 'Mesothelioma_Cancer',
                 'Oesophageal_Cancer',
                 'Hodgkin.Lymphoma_Cancer',
                 'Leukaemia_Cancer',
                 'Myeloma_Cancer',
                 'Non.Hodgkin.Lymphoma_Cancer',
                 'Other_Cancer')

gender_specific_cancers<-c("Breast_Cancer",
                           'Cervical_Cancer',
                           'Prostate_Cancer',
                           'Testicular_Cancer',
                           'Uterine_Cancer',
                           'Ovarian_Cancer')


for (cancer in cancer_columns){
  if(!(cancer %in% gender_specific_cancers)){
    main_data <- mydata %>% select("PseudoNHSnumber","Group","Gender","Ethnicity","Index_age",all_of(cancer))
    filtered_data_case<-main_data %>% 
      filter(!!sym(cancer)!=0) %>%
      select("PseudoNHSnumber","Group","Gender","Ethnicity","Index_age")
    
    combined_df<-rbind(filtered_data_case,filtered_data_control)
    #knn matching 4:1,order=data
    result_8<-matchit(Group~Gender+Ethnicity+Index_age, data=combined_df, method='nearest', distance = 'glm',m.order = "data", ratio = 4, replace = FALSE)
    result_8
    summary(result_8, un=FALSE)
    mydata8<- match.data(result_8)
    
    file_path <- "/genesandhealth/red/AshithaJoby/results/Matched_data_files/"
    file_name = cancer
    write.csv(mydata8, file = paste0(file_path,file_name,".csv"),row.names = FALSE)
    }
  
  else{
    main_data <- mydata %>% select("PseudoNHSnumber","Group","Gender","Ethnicity","Index_age",all_of(cancer))
    filtered_data_case<-main_data %>% 
      filter(!!sym(cancer)!=0) %>%
      select("PseudoNHSnumber","Group","Ethnicity","Index_age")
    
    combined_df<-rbind(filtered_data_case,filtered_data_control[,c("PseudoNHSnumber","Group","Ethnicity","Index_age")])
    #knn matching 4:1,order=data
    result_8<-matchit(Group~Ethnicity+Index_age, data=combined_df, method='nearest', distance = 'glm',m.order = "data", ratio = 4, replace = FALSE)
    result_8
    summary(result_8, un=FALSE)
    mydata8<- match.data(result_8)
    
    file_path <- "/genesandhealth/red/AshithaJoby/results/Matched_data_files/"
    file_name = cancer
    write.csv(mydata8, file = paste0(file_path,file_name,".csv"),row.names = FALSE)
    }
   
  }































































































