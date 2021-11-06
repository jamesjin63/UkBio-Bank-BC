library(tidyverse)
library(doParallel)
rm(list=ls())
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_drink.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_employe.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/df_coffee_tea.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_enroll.Rdata")
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_followup.Rdata")

### 所有乳腺癌的病人
dfall=read.csv("dfallBC.csv",header = T)

### 生存的信息


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
                            labels=c("<=5","6_7","8_9",">=10"))) %>% 
  mutate(sleep_duration=as.factor(sleep_duration)) %>% 
  mutate(BC_history=as.factor(ifelse(BC_history=="Breast cancer","Yes","No"))) %>% 
  mutate(age_menarch=as.factor(cut(age_menarch,breaks=c(-Inf,11,14,Inf),
                                   labels=c("<=11","12_13",">=14")))) %>% 
  mutate(smoking_status=as.factor(smoking_status),
         alcohol_intake=as.factor(alcohol_intake),
         Had_menopause=as.factor(Had_menopause),
         HRT=as.factor(HRT),
         Ethnics=as.factor(Ethnics)) 



####################################################################################
All_BC=left_join(dfall,dfbase)

####################################################################################


## follow time
## 有诊断疾病的
xa=dfall %>% 
  mutate(dignosde_time = replace_na(dignosde_time, "2021-01-01")) %>%  ## 替换NA
  # select(dignosde_time,enter_time) %>% na.omit()
  filter(BC=="Yes")%>% 
  mutate(time=as.numeric(as.Date(dignosde_time)-as.Date(enter_time))) %>% # survival time
  select(f.eid,time,BC) %>% mutate(status=2) #status 1=censored, 2=BC

## 无诊断疾病的
xb=dfall %>% 
  mutate(dignosde_time = replace_na(dignosde_time, "2021-01-01")) %>%  ## 替换NA
  mutate(BC = replace_na(BC, "Other")) %>% 
  filter(!BC=="Yes") %>% 
  mutate(time=as.Date(dignosde_time)-as.Date(enter_time)) %>% # survival time
  mutate(deathTm=as.Date(death_time)-as.Date(enter_time)) %>% # deathe time
  mutate(deathTm = replace_na(deathTm, 99999)) %>% 
  mutate(followTm=as.Date(followup_time)-as.Date(enter_time)) %>%  # deathe time
  mutate(followTm = replace_na(followTm, 99999)) %>% 
  mutate(time1=ifelse(time>deathTm,deathTm,time)) %>% 
  mutate(time2=ifelse(time>followTm,followTm,time)) %>% 
  mutate(time=ifelse(time1<time2,time1,time2)) %>% 
  select(f.eid,time,BC) %>% mutate(status=1)# status 1=censored, 2=BC

## 生存时间
surdf=rbind(xa,xb)

## 合并影响因素
dfsur=left_join(surdf,dfbase)





x=bd_employe %>%  ## 这里的诊断时间，按照第一次算
  select(f.eid,f.3426.0.0) %>% 
  as_tibble() %>% 
  rename("Job_night_shift"=2) 

yy=dfall %>% select(f.eid,BC) %>% 
  left_join(.,x) %>% 
  mutate(BC=replace_na(BC,"No")) %>%   as_tibble() 


library(compareGroups)
df=yy %>% filter(Job_night_shift %in% c("Never/rarely","Sometimes","Usually","Always")) %>% 
  mutate(BC=as.factor(BC) ,
                 Job_night_shift=as.factor(Job_night_shift)) 

resOR <- compareGroups(BC ~ Job_night_shift, df)
restabOR <- createTable(resOR, show.ratio = TRUE, show.p.overall = FALSE, 
                        hide.no = "no",  type=1)
print(restabOR, header.labels = c(p.ratio = "p-value"))

dfyall=dfsur %>% 
  left_join(.,x) %>% 
  mutate(BC=replace_na(BC,"No")) %>%   as_tibble() 

write.csv(dfyall,"All_BC_employe.csv",row.names = F)
#

##############################
### 计算单因素RR
##############################


##############################
### 计算多因素RR
##############################

