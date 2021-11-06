library(tidyverse)
library(doParallel)
rm(list=ls())
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_drink.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/df_coffee_tea.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_enroll.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_followup.Rdata")

####################################
### 筛选病人
####################################
death=bd_death %>% select(f.eid,f.40000.0.0) %>% 
  set_names("f.eid","death_time")

deathBC=bd_death %>% select(f.eid,f.40000.0.0,f.40001.0.0,f.40002.0.1,f.40010.0.0) %>% 
  set_names("f.eid","death_time","ICD10","ICD210","reason")



x=bd_register %>%  ## 这里的诊断时间，按照第一次算
  select(f.eid,f.34.0.0,f.21022.0.0,f.40005.0.0,f.40006.0.0,f.40013.0.0) %>% 
  as_tibble() %>% left_join(bd_enroll) %>% left_join(bd_followup) %>% 
  left_join(.,death) %>% 
  select(f.eid,f.34.0.0,f.40005.0.0,f.40006.0.0,f.40013.0.0,f.53.0.0,f.191.0.0,death_time) %>% 
  set_names("f.eid","born_year","dignosde_time","ICD10","ICD9","enter_time",
            "followup_time","death_time")


### 1.排除入组前有癌症的病人： 诊断时间< 入组时间
xa= x %>% filter(dignosde_time<enter_time)

### 2.排除 失访的病人（失访时间<诊断时间）
xb= x %>% filter(followup_time<dignosde_time)

### 3.排除 死亡的病人 （死亡时间<诊断时间）
xc= x %>% filter(death_time<dignosde_time)

### 4.排除 没有喝咖啡记录的人
### 5.随访时间不足一年 （入组不到一年，就发生癌症，死亡，或失访，其中任意一个）
xe= x %>% filter(followup_time<(enter_time+365) |
                   (dignosde_time<(enter_time+365) & dignosde_time>(enter_time) )|
                   death_time<(enter_time+365) )


####################################
### 符合病人
####################################

##### 随访出现癌症的
## 1. ICD10
df_cancer= x %>% filter(!f.eid %in% c(xa$f.eid,xb$f.eid,xc$f.eid,xe$f.eid)) %>% 
  drop_na(dignosde_time) %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No"))

## 2. ICD9
df_cancer2= x %>% filter(ICD9==174) %>% 
  drop_na(ICD9) %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No"))
  
##### 随访未出现癌症的
df_cancer_non= x %>% filter(!f.eid %in% c(xa$f.eid,xb$f.eid,xc$f.eid,xe$f.eid)) %>% 
  filter(!f.eid %in% c(df_cancer$f.eid)) %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No"))


### 乳腺癌比较
dfall=rbind(df_cancer_non,df_cancer) #%>% filter(enter_time>as.Date("2010-01-01"))


####################################
### 死亡病人--BC
####################################

## 3. death reason
df_cancer3= deathBC %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No")) %>% 
  drop_na(BC)


####################################
### 属于---BC
####################################
df_cancer3_BC=df_cancer3 %>% filter(BC=="Yes")

x1=dfall %>% filter(f.eid %in% df_cancer3_BC$f.eid) %>% 
  mutate(cancer=NA,
         BC="Yes")

x2=dfall %>% filter(!f.eid %in% df_cancer3_BC$f.eid) 


### 乳腺癌
dfallend=rbind(x2,x1)
write.csv(dfallend,"dfallBC.csv",row.names = F)





