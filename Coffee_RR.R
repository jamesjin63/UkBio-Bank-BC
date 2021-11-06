library(tidyverse)
library(doParallel)
rm(list=ls())
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_drink.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/df_coffee_tea.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_enroll.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_followup.Rdata")
# x=bd_register %>% 
#   select(f.eid,f.34.0.0,f.21022.0.0,
#          f.40006.0.0,f.40006.1.0,f.40006.2.0,f.40006.3.0,
#          f.40006.4.0,f.40006.5.0,f.40006.7.0,f.40006.8.0,
#          f.40006.9.0,f.40006.10.0,f.40006.11.0,f.40006.12.0,
#          f.40006.13.0,f.40006.14.0,f.40006.15.0,f.40006.16.0) %>% 
#   drop_na(7)
# 
# x=bd_register %>% 
#   select(f.eid,f.34.0.0,f.21022.0.0,
#          f.40008.0.0,f.40008.1.0,f.40008.2.0,f.40008.3.0,
#          f.40008.4.0,f.40008.5.0,f.40008.6.0,f.40008.7.0,
#          f.40008.8.0,f.40008.9.0,f.40008.10.0,f.40008.11.0,
#          f.40008.12.0,f.40008.13.0,f.40008.14.0,f.40008.15.0,f.40008.16.0,
#          ) %>% 
#   drop_na(5)

####################################
### 筛选病人
####################################
death=bd_death %>% select(f.eid,f.40000.0.0) %>% 
  set_names("f.eid","death_time")
Coffee=df %>% select(f.eid,Coffee)


x=bd_register %>%  ## 这里的诊断时间，按照第一次算
  select(f.eid,f.34.0.0,f.21022.0.0,f.40005.0.0,f.40006.0.0) %>% 
  as_tibble() %>% left_join(bd_enroll) %>% left_join(bd_followup) %>% 
  left_join(.,death) %>% left_join(.,Coffee) %>% 
  select(f.eid,f.34.0.0,f.40005.0.0,f.40006.0.0,f.53.0.0,f.191.0.0,death_time,Coffee) %>% 
  set_names("f.eid","born_year","dignosde_time","ICD10","enter_time",
            "followup_time","death_time","Coffee")
         

### 1.排除入组前有癌症的病人： 诊断时间< 入组时间
xa= x %>% filter(dignosde_time<enter_time)

### 2.排除 失访的病人（失访时间<诊断时间）
xb= x %>% filter(followup_time<dignosde_time)

### 3.排除 死亡的病人 （死亡时间<诊断时间）
xc= x %>% filter(death_time<dignosde_time)

### 4.排除 没有喝咖啡记录的人
xd1= x %>% drop_na(Coffee)
xd= x %>% filter(!f.eid %in% xd1$f.eid)

### 5.随访时间不足一年 （入组不到一年，就发生癌症，死亡，或失访，其中任意一个）
xe= x %>% filter(followup_time<(enter_time+365) |
                 (dignosde_time<(enter_time+365) & dignosde_time>(enter_time) )|
                 death_time<(enter_time+365) )


####################################
### 符合病人
####################################

##### 随访出现癌症的
df_cancer= x %>% filter(!f.eid %in% c(xa$f.eid,xb$f.eid,xc$f.eid,xd$f.eid,xe$f.eid)) %>% 
  drop_na(dignosde_time) %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No"),
         Coffee=ifelse(Coffee=="No","No","Yes"))

##### 随访未出现癌症的
df_cancer_non= x %>% filter(!f.eid %in% c(xa$f.eid,xb$f.eid,xc$f.eid,xd$f.eid,xe$f.eid)) %>% 
  filter(!f.eid %in% c(df_cancer$f.eid)) %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No"),
         Coffee=ifelse(Coffee=="No","No","Yes"))


### 乳腺癌比较
dfall=rbind(df_cancer_non,df_cancer) #%>% filter(enter_time>as.Date("2010-01-01"))

write.csv(dfall,"BC_cancer_coffee_tea.csv")

##### 随访出现癌症的
df_Coffee= dfall %>% 
  filter(Coffee=="Yes")

table(df_Coffee$BC)

##### 随访未出现癌症的
df_Coffee_non= dfall %>% 
  filter(Coffee=="No")

table(df_Coffee_non$BC)


library(epitools) 
RRtable<-matrix(c(345,22970,152,11169),nrow = 2, ncol = 2)
RRtable
riskratio.wald(RRtable)   



#write.csv(dfall,"Coffe_BC.csv")


################################1################################1
######### covariate:
################################1################################1
# For multivariable analysis:
# 1. Demographic variables: age, race/ethnicity, education
# 2. Lifestyle variables: smoking history pack-years of smoking, alcohol intake, total energy intake,
# physical activity, BMI, sleep duration
# 3. Reproductive variables: family history of BC, oral contraceptive use ever history of menopausal
# HT use, age at menarche, age at menopause, age at first full-term birth, number of live birth

################################1
dfbase1=bd_register %>%  
  select(f.eid,f.21022.0.0,f.21001.0.0,f.20116.0.0,
         f.20414.0.0,f.1160.0.0,f.20110.0.0
  ) %>% 
  set_names("f.eid","Age","BMI","smoking_status",
            "alcohol_intake","sleep_duration","BC_history")

################################2
dfbase2=bd_death %>%  
  select(f.eid,f.845.0.0,f.2724.0.0,f.2814.0.0
  ) %>% 
  set_names("f.eid","Age_education","Had_menopause","HRT")

################################3
dfbase3=bd_followup%>%  
  select(f.eid,f.2714.0.0,f.2734.0.0,f.2754.0.0,f.21000.0.0) %>% 
  set_names("f.eid","age_menarch",
            "Number_of_live_births",
            "Age_first_live_birth",
            "Ethnics")
################################ all
dfbase=left_join(dfbase1,dfbase2) %>% left_join(.,dfbase3)

dfbase=left_join(dfbase1,dfbase2) %>% left_join(.,dfbase3) %>% 
  as_tibble() %>% 
  filter(!Age_first_live_birth %in% c(-4,-3)) %>% 
  filter(!Number_of_live_births %in% c(-4,-3)) %>% 
  filter(!age_menarch %in% c(-4,-3,-1)) %>% 
  filter(!Age_education%in% c(-4,-3)) %>% 
  filter(!sleep_duration%in% c(-4,-3)) %>% 
  mutate(sleep_duration=cut(sleep_duration,breaks=c(-Inf,5,7,9,Inf),
                            labels=c("<=5","6-7","8-9",">=10"))) %>% 
  mutate(sleep_duration=as.factor(sleep_duration)) %>% 
  mutate(BC_history=as.factor(ifelse(BC_history=="Breast cancer","Yes","No"))) %>% 
  mutate(age_menarch=as.factor(cut(age_menarch,breaks=c(-Inf,11,14,Inf),
                                   labels=c("<=11","12-13",">=14")))) %>% 
  mutate(smoking_status=as.factor(smoking_status),
         alcohol_intake=as.factor(alcohol_intake),
         Had_menopause=as.factor(Had_menopause),
         HRT=as.factor(HRT),
         Ethnics=as.factor(Ethnics))








