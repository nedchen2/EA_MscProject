setwd("C:/Users/彼岸花开/Documents/Silwood/Project/Rproject")
##packages were used in this project 
library("ggplot2")
library("ggpubr")
library("dplyr")
require(tidyverse)
library(ggthemes)
require(lme4)
library(lmerTest)
require(readxl)

####set a data frame contains earliest telomere length(T/SRatio)
TS <- read.csv("./TSRatio.csv")
Full_table <- read.csv("./20220321BirdLifeHistFull(1).csv")
ID <- TS %>% mutate(date = as.Date(CaptureDate)) %>% 
  arrange(BirdID,date) %>% 
  distinct(BirdID, .keep_all = TRUE) %>% 
  select(BirdID,TSRatio,CaptureDate)

merged_table <- ID %>% left_join(Full_table)
write.csv(x = merged_table, file = "./merged_table.csv",row.names = F)
Birdlife<-merged_table


##=====Calculating social(foster) parents age at breeding event=======##  
brood <- read_excel(path =  "./tblBroods.xlsx")
merged_id_birth_dad <- Birdlife %>% rename(SocialDadID = BirdID) %>% select(SocialDadID,Cohort)  %>% rename(Cohort_dad = Cohort)
merged_id_birth_mum <- Birdlife %>% rename(SocialMumID = BirdID) %>% select(SocialMumID,Cohort)  %>% rename(Cohort_mum = Cohort)

SocialParentAge <- Birdlife %>% select(BirdID,Cohort,NatalBrood,TSRatio) %>% rename(BroodRef = NatalBrood) %>% 
  left_join(brood) %>% left_join(merged_id_birth_dad) %>% left_join(merged_id_birth_mum) %>% mutate( age_dad = Cohort-  Cohort_dad,
                                                                                                     age_mum = Cohort - Cohort_mum) 
write.csv(x=SocialParentAge,file = "./SocialParentAge.csv",row.names = F)


##==========calculating genetic parents age at breeding event=========##
genetic_data <- read_excel("./20210317PedUpTo2019.xlsx") %>% 
  rename(Genetic_dad = Dam,
         Genetic_mum = Sire,
         BirdID = Offspring) %>% 
  mutate(Genetic_dad = as.integer(Genetic_dad),
         Genetic_mum = as.integer(Genetic_mum)) 

Genetic_id_birth_dad <- Full_table %>% select(BirdID,Cohort)  %>% rename(Cohort_dad = Cohort,
                                                                         Genetic_dad = BirdID) 
Genetic_id_birth_mum <- Full_table %>% select(BirdID,Cohort)  %>% rename(Cohort_mum = Cohort,
                                                                         Genetic_mum = BirdID)
                                                                         
social_parent_TSratio <-  read.csv("SocialParentAge.csv") %>% select(BirdID,TSRatio)                                                                         
                                                                                                                                               
GeneticParentAge <- genetic_data %>% left_join(Genetic_id_birth_dad) %>% left_join(Genetic_id_birth_mum) %>% 
  mutate( genetic_dad_age = Cohort-  Cohort_dad,
          genetic_mum_age = Cohort - Cohort_mum)   %>% 
  filter(!(is.na(Genetic_dad)|is.na(Genetic_mum))) %>% 
  filter(!is.na(TSRatio))

write.csv(x=GeneticParentAge,file = "./GeneticParentAge.csv",row.names = F)

##=======calculating within-in subject centering of genetic parent age ========##
GeneticParentAge %>% group_by(Cohort) %>% summarize(mean_genetic_dad = mean(genetic_dad_age,na.rm=T),
                                                    mean_genetic_mum = mean(genetic_mum_age,na.rm=T))

GeneticParentAge2<-GeneticParentAge %>% left_join(df_mean_age,by="Cohort") %>% mutate(deltaGeneticDad = genetic_dad_age - mean_genetic_dad,
                                                                        deltaGeneticMum = genetic_mum_age - mean_genetic_mum)
write_csv("./GeneticParentAge2.csv")

##=========calculating with-in subject centering of social parent age  
SocialParentAge<-SocialParentAge %>% mutate(age_dad = as.numeric(age_dad))

df_mean_age <- SocialParentAge %>% group_by(Cohort) %>% summarize(mean_social_dad = mean(age_dad,na.rm=T))
df_social_parent_age %>% left_join(df_mean_age,by="Cohort") %>% mutate(deltaSocialDad = age_dad - mean_social_dad,
                                                                       deltaSocialMum = age_mum - mean_social_mum) %>% 
write_csv("./SocialParentAge2.csv")                                                                      

###=======calculating offspring age =======##

raw_data<-read.csv("raw_data.csv")
blood<-raw_data%>%select(BirdID,Plate,BloodAge,DNAAge,CaptureYear)
GeneticParentAge2<-GeneticParentAge2%>%left_join(blood)
SocialParentAge2<-SocialParentAge2%>%left_join(blood)
GeneticParentAge2<-GeneticParentAge2%>%mutate(offspring_age=CaptureYear-Cohort)
SocialParentAge2<-SocialParentAge2%>%mutate(offspring_age=CaptureYear-Cohort)

##calculating quadratic of social and genetic parents age=======##
SocialParentAge2<-SocialParentAge2%>%mutate(age_dad2=age_dad^2,
                                            age_mum2=age_mum^2)

GeneticParentAge2<-GeneticParentAge2%>%mutate(genetic_dad_age2=genetic_dad_age^2,
                                              genetic_mum_age2=genetic_mum_age^2)

###=======select offspring which are less than one year old=====##
SocialParentAge3<-SocialParentAge2%>%subset(offspring_age<=1)
GeneticParentAge3<-GeneticParentAge2%>%subset(offspring_age<=1)
write.csv(SocialParentAge3,"C:/Users/彼岸花开/Documents/Silwood/Project/Rproject/SocialParentAge3.csv")
write.csv(GeneticParentAge3,"C:/Users/彼岸花开/Documents/Silwood/Project/Rproject/GeneticParentAge3.csv")

##=====describe statistics=======##
summary(GeneticParentAge3$offspring_age)
sd(GeneticParentAge3$offspring_age)
summary(GeneticParentAge3$genetic_dad_age)
sd(GeneticParentAge3$genetic_dad_age)
summary(GeneticParentAge3$genetic_mum_age)
sd(GeneticParentAge3$genetic_mum_age)

summary(SocialParentAge3$offspring_age)
sd(SocialParentAge3$offspring_age)
summary(SocialParentAge3$age_dad)
sd(SocialParentAge3$age_dad)
summary(SocialParentAge3$age_mum)
sd(SocialParentAge3$age_mum)

summary(GeneticParentAge3$TSRatio)
sd(GeneticParentAge3$TSRatio)
summary(SocialParentAge3$TSRatio)
sd(SocialParentAge3$TSRatio)
##========linear mixed model of genetic and social parents=====##
summary(lmer(TSRatio~mean_genetic_dad+centered_geneticdad_age+BloodAge+DNAAge+offspring_age+(1|Genetic_dad)+(1|Cohort)+(1|Plate),data=GeneticParentAge3))
summary(lmer(TSRatio~mean_genetic_mum+centered_geneticmom_age+BloodAge+DNAAge+offspring_age+(1|Genetic_mum)+(1|Cohort)+(1|Plate),data=GeneticParentAge3))
summary(lmer(TSRatio~mean_social_dad+deltaSocialDad+BloodAge+DNAAge+offspring_age+(1|SocialDadID)+(1|Cohort)+(1|Plate),data=SocialParentAge3))
summary(lmer(TSRatio~mean_social_mum+deltaSocialMum+BloodAge+DNAAge+offspring_age+(1|SocialMumID)+(1|Cohort)+(1|Plate),data=SocialParentAge3))

##========calculating quadratic relationship of genetic and social parents=====##
geneticdad2<-lmer(TSRatio~genetic_dad_age2+genetic_dad_age+DNAAge+BloodAge+offspring_age+(1|Genetic_dad)+(1|Cohort)+(1|Plate),data=GeneticParentAge2)
summary(geneticdad2)
geneticmom2<-lmer(TSRatio~genetic_mum_age2+genetic_mum_age+DNAAge+BloodAge+offspring_age+(1|Genetic_mum)+(1|Cohort)+(1|Plate),data=GeneticParentAge2)
summary(geneticmom2)
socialdad3<-lmer(TSRatio~age_dad2+age_dad+DNAAge+BloodAge+offspring_age+(1|SocialDadID)+(1|Cohort)+(1|Plate),data=SocialParentAge3)
summary(socialdad3)
socialmom3<-lmer(TSRatio~age_mum2+age_mum+DNAAge+BloodAge+offspring_age+(1|SocialMumID)+(1|Cohort)+(1|Plate),data=SocialParentAge3)
summary(socialmom3)

##==========making linear plots============##
linear_meangenetic_dad<-ggplot(GeneticParentAge3,aes(mean_genetic_dad,TSRatio))+geom_point()+geom_smooth(method = "lm",formula = y~x,se=T)+theme_classic()+labs(x="Mean age genetic father (years)",y="Offspring telomere lenghth")
linear_meangenetic_mum<-ggplot(GeneticParentAge3,aes(mean_genetic_mum,TSRatio))+geom_point()+geom_smooth(method = "lm",formula = y~x,se=T)+theme_classic()+labs(x="Mean age genetic mother (years)",y="Offspring telomere lenghth")
linear_within_genetic_dad<-ggplot(GeneticParentAge3,aes(centered_geneticmom_age,TSRatio))+geom_point()+geom_smooth(method = "lm",formula = y~x,se=T)+theme_classic()+labs(x="Centered age genetic mother (years)",y="Offspring telomere lenghth")

linear_meansocial_dad<-ggplot(SocialParentAge3,aes(mean_social_dad,TSRatio))+geom_point()+geom_smooth(method = "lm",formula = y~x,se=T)+theme_classic()+labs(x="Mean age social father (years)",y="Offspring telomere lenghth")
linear_meansocial_mom<-ggplot(SocialParentAge3,aes(mean_social_mum,TSRatio))+geom_point()+geom_smooth(method = "lm",formula = y~x,se=T)+theme_classic()+labs(x="Mean age social mother (years)",y="Offspring telomere lenghth")
linear_within_social_dad<-ggplot(SocialParentAge3,aes(deltaSocialDad,TSRatio))+geom_point()+geom_smooth(method = "lm",formula = y~x,se=T)+theme_classic()+labs(x="Centered age social father (years)",y="Offspring telomere lenghth")
linear_within_social_mom<-ggplot(SocialParentAge3,aes(deltaSocialMum,TSRatio))+geom_point()+geom_smooth(method = "lm",formula = y~x,se=T)+theme_classic()+labs(x="Centered age social mother (years)",y="Offspring telomere lenghth")
##=============end========##






















