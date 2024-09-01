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
# Convert multiple columns to factors
cols_to_factor <- c("Group", "Gender", "Ethnicity")
mydata[cols_to_factor] <- lapply(mydata[cols_to_factor], as.factor)



filtered_data_control<-mydata %>%
  select("PseudoNHSnumber","Group","Gender","Ethnicity","Index_age")%>%
  filter(Group==0)


main_data <- mydata %>% select("PseudoNHSnumber","Group","Gender","Ethnicity","Index_age",all_of('Colorectal_Cancer'))
filtered_data_case<-main_data %>% 
  filter(!!sym('Colorectal_Cancer')!=0) %>%
  select("PseudoNHSnumber","Group","Gender","Ethnicity","Index_age")


combined_df<-rbind(filtered_data_case,filtered_data_control)
#before matching
result_0<-matchit(Group~Gender+Ethnicity+Index_age, data=combined_df,method=NULL, distance = 'glm')

#summary(result_0)

#knn matching 4:1,order=data
result_8<-matchit(Group~Gender+Ethnicity+Index_age, data=combined_df, method='nearest', distance = 'glm',m.order = "data", ratio = 4, replace = FALSE)
mydata1<- match.data(result_8)


#Gender distribution of unmatched data
combined_df %>%
  count(Group, Gender) %>%       
  group_by(Group) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Group, pct, fill=Gender) +
  geom_bar(stat="identity") +
  ylab("Percentage") +
  xlab("Group (0-Control, 1-Case)")+
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Percentage Distribution by Gender and Group [unmatched data]") +
  theme_bw()
#Ethnicity distribution of unmatched data
combined_df %>%
  count(Group, Ethnicity) %>%       
  group_by(Group) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Group, pct, fill=Ethnicity) +
  geom_bar(stat="identity") +
  ylab("Percentage") +
  xlab("Group (0-Control, 1-Case)")+
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Percentage Distribution by Ethnicity and Group [unmatched data]") +
  theme_bw()

# age density plot of unmatched data
ggplot(combined_df, aes(x=Index_age, fill=Group)) +
  geom_density(alpha=0.5) +
  labs(title="Age Density by Group [unmatched data]",
       x="Age",
       y="Density") +
  theme_minimal()+
  scale_fill_manual(values=c("0"="#1f77b4", "1"="#ff7f0e"),
                    name="Group",
                    labels=c("Control", "Case"))


# Gender distribution of matched data
mydata1 %>%
  count(Group, Gender) %>%       
  group_by(Group) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Group, pct, fill=Gender) +
  geom_bar(stat="identity") +
  ylab("Percentage") +
  xlab("Group (0-Control, 1-Case)")+
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Percentage Distribution by Gender and Group [matched data]") +
  theme_bw()

# Ethnicity distribution of matched data
mydata1 %>%
  count(Group, Ethnicity) %>%       
  group_by(Group) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Group, pct, fill=Ethnicity) +
  geom_bar(stat="identity") +
  ylab("Percentage") +
  xlab("Group (0-Control, 1-Case)")+
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Percentage Distribution by Ethnicity and Group [matched data]") +
  theme_bw()

#Age density plot of matched data
ggplot(mydata1, aes(x=Index_age, fill=Group)) +
  geom_density(alpha=0.5) +
  labs(title="Age Density by Group [matched data]",
       x="Age",
       y="Density") +
  theme_minimal()+
  scale_fill_manual(values=c("0"="#1f77b4", "1"="#ff7f0e"),
                    name="Group",
                    labels=c("Control", "Case"))




#Demographic distribution of case and control (total data)
#Gender distribution of unmatched data
mydata %>%
  count(Group, Gender) %>%       
  group_by(Group) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Group, pct, fill=Gender) +
  geom_bar(stat="identity") +
  ylab("Percentage") +
  xlab("Group (0-Control, 1-Case)")+
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Percentage Distribution by Gender and Group") +
  theme_bw()
#Ethnicity distribution of unmatched data
mydata %>%
  count(Group, Ethnicity) %>%       
  group_by(Group) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Group, pct, fill=Ethnicity) +
  geom_bar(stat="identity") +
  ylab("Percentage") +
  xlab("Group (0-Control, 1-Case)")+
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  ggtitle("Percentage Distribution by Ethnicity and Group") +
  theme_bw()

# age density plot of unmatched data
ggplot(mydata, aes(x=Index_age, fill=Group)) +
  geom_density(alpha=0.5) +
  labs(title="Age Density by Group",
       x="Age",
       y="Density") +
  theme_minimal()+
  scale_fill_manual(values=c("0"="#1f77b4", "1"="#ff7f0e"),
                    name="Group",
                    labels=c("Control", "Case"))


























