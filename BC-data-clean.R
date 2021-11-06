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

x=bd_register %>%  ## 这里的诊断时间，按照第一次算
  select(f.eid,f.34.0.0,f.21022.0.0,f.40005.0.0,f.40006.0.0) %>% 
  as_tibble() %>% left_join(bd_enroll) %>% left_join(bd_followup) %>% 
  left_join(.,death) %>% 
  select(f.eid,f.34.0.0,f.40005.0.0,f.40006.0.0,f.53.0.0,f.191.0.0,death_time) %>% 
  set_names("f.eid","born_year","dignosde_time","ICD10","enter_time",
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
df_cancer= x %>% filter(!f.eid %in% c(xa$f.eid,xb$f.eid,xc$f.eid,xe$f.eid)) %>% 
  drop_na(dignosde_time) %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No"))

##### 随访未出现癌症的
df_cancer_non= x %>% filter(!f.eid %in% c(xa$f.eid,xb$f.eid,xc$f.eid,xe$f.eid)) %>% 
  filter(!f.eid %in% c(df_cancer$f.eid)) %>% 
  mutate(cancer=substr(ICD10,1,3)) %>% 
  mutate(BC=ifelse(cancer=="C50","Yes","No"))


### 乳腺癌比较
dfall=rbind(df_cancer_non,df_cancer) #%>% filter(enter_time>as.Date("2010-01-01"))




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
write.csv(All_BC,"All_BC.csv",row.names = F)
####################################################################################


## follow time
## 有诊断疾病的
xa=dfall %>% 
  mutate(dignosde_time = replace_na(dignosde_time, "2021-01-01")) %>%  ## 替换NA
  # select(dignosde_time,enter_time) %>% na.omit()
  filter(BC=="Yes")%>% 
  mutate(time=as.numeric(dignosde_time-enter_time)) %>% # survival time
  select(f.eid,time,BC) %>% mutate(status=2) #status 1=censored, 2=BC


## 无诊断疾病的
xb=dfall %>% 
  mutate(dignosde_time = replace_na(dignosde_time, "2021-01-01")) %>%  ## 替换NA
  mutate(BC = replace_na(BC, "Other")) %>% 
  filter(!BC=="Yes") %>% 
  mutate(time=dignosde_time-enter_time) %>% # survival time
  mutate(deathTm=death_time-enter_time) %>% # deathe time
  mutate(deathTm = replace_na(deathTm, 99999)) %>% 
  mutate(followTm=followup_time-enter_time) %>%  # deathe time
  mutate(followTm = replace_na(followTm, 99999)) %>% 
  mutate(time1=ifelse(time>deathTm,deathTm,time)) %>% 
  mutate(time2=ifelse(time>followTm,followTm,time)) %>% 
  mutate(time=ifelse(time1<time2,time1,time2)) %>% 
  select(f.eid,time,BC) %>% mutate(status=1)# status 1=censored, 2=BC

## 生存时间
surdf=rbind(xa,xb)

## 合并影响因素
dfsur=left_join(surdf,dfbase)


### load package
library(tidyverse)
library("survival")
library("survminer")
library(tableone)


###########################################################################
## 生存分析
###########################################################################

df=dfsur %>% select(-f.eid,-Age_education,-alcohol_intake,
                    -Age_first_live_birth) %>% 
  #filter(HRT %in% c("Yes","No")) %>% 
  #filter(!smoking_status=="Pre") %>% 
  mutate(Ethnics=as.factor(ifelse(Ethnics=="British","British","Non-British"))) %>% 
  na.omit() %>% 
  select(-BC)
table(df$status)

write.csv(df,"dfxb.csv",row.names = F)




df=read.csv("dfxb.csv",header = T) %>%
  mutate(Ethnics=as.factor(ifelse(Ethnics=="British","British","Non-British"))) 
########  KM -plot
fit_KM = survfit(Surv(time, status) ~ sleep_duration, data = df)
print(fit_KM)
ggsurvplot(fit_KM,data=df,pval = TRUE)



cox <- coxph(Surv(time, status) ~ . , data = df)
summary(cox)
cox_fit <- survfit(cox)
summary(cox_fit)

# ranger model
library(ranger)
r_fit <- ranger(Surv(time, status) ~ . , data = df,
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(df)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)

###########################################################################
## PSM
###########################################################################
library("MatchIt")
dfMA=df %>% mutate(status=ifelse(status==1,0,1))

# 1:1 NN PS matching w/o replacement
m.out1 <- matchit(status ~ Age + BMI + smoking_status+ 
                    sleep_duration+BC_history+
                    Had_menopause+HRT+Number_of_live_births+
                    Ethnics+age_menarch,
                  data = dfMA,
                  method = "nearest", distance = "glm")

m.out1 <- matchit(status ~ Age +
                    Ethnics,
                  data = dfMA,
                  method = "nearest", distance = "glm")
# Checking balance after NN matching
summary(m.out1, un = FALSE)

plot(summary(m.out1))

## PSA 数据
m.data1 <- match.data(m.out1)
dfaa=m.data1 %>% select(-13,-14,-15)
write.csv(dfaa,"dfxb.csv",row.names = F)



