library(tidyverse)
library(doParallel)
rm(list=ls())
load("~/Desktop/PhDwork/Bricks/Lina/UKB-drink/bd_drink.Rdata")


x1=tibble(id=1:dim(bd)[2],
          name=colnames(bd))
x2=tibble(id=1:dim(bd_death)[2],
          name=colnames(bd_death))
x3=tibble(id=1:dim(bd_beverage)[2],
          name=colnames(bd_beverage))
x4=tibble(id=1:dim(bd_register)[2],
          name=colnames(bd_register))

library(readr)
registername <- read_table("BC/register.txt")
registername =as.data.frame(registername )
register=bd_register %>% 
  select(f.eid,f.1160.0.0,f.1170.0.0,f.1180.0.0,f.1190.0.0,
         f.1200.0.0,f.1200.1.0,f.1200.0.0,f.20117.0.0,
         f.20414.0.0,f.31.0.0,f.34.0.0,f.52.0.0,f.189.0.0,
         f.21022.0.0,f.40005.0.0,f.40006.0.0,f.40008.0.0,
         f.40009.0.0,f.40011.0.0,f.40012.0.0,f.40013.0.0,
         f.40019.0.0,f.40021.0.0,f.20110.0.0,f.2674.0.0,
         f.2784.0.0,f.2794.0.0,f.1239.0.0,f.20116.0.0,f.21001.0.0)


deathname <- read_table("BC/death.txt")
deathname =as.data.frame(deathname )
death=bd_death %>% 
  select(f.eid,f.845.0.0,f.6138.0.0,f.2724.0.0,f.2814.0.0,
         f.2734.0.0,f.20089.0.0,f.103000.0.0,f.103010.0.0,
         f.103020.0.0,f.103040.0.0,f.103050.0.0,f.103060.0.0,
         f.103070.0.0,f.103080.0.0,f.103090.0.0,
         f.103100.0.0,f.103120.0.0,f.103130.0.0,f.103140.0.0,
         f.103150.0.0,f.103160.0.0,f.103170.0.0,f.103180.0.0,
         f.103190.0.0,f.103200.0.0,f.103210.0.0,f.103220.0.0,
         f.103230.0.0,f.826.0.0,f.3426.0.0,f.40000.0.0,f.40001.0.0,
         f.40002.0.1,f.40007.0.0,f.40010.0.0)


beveragename <- read_table("BC/beverage.txt")
beveragename =as.data.frame(beveragename )
beverage=bd_beverage %>% 
  select(f.eid,f.100150.0.0,f.100160.0.0,f.100170.0.0,f.100180.0.0,
         f.100190.0.0,f.100200.0.0,f.100210.0.0,f.100220.0.0,
         f.100230.0.0,f.100240.0.0,f.100250.0.0,f.100260.0.0,f.100270.0.0,f.100280.0.0,
         f.100290.0.0,f.100300.0.0,f.100310.0.0,f.100320.0.0,f.100330.0.0,
         f.100350.0.0,f.100360.0.0,f.100370.0.0,f.100380.0.0,f.100390.0.0,f.100400.0.0,
         f.100410.0.0,f.100420.0.0,f.100430.0.0,f.100440.0.0,f.100460.0.0,
         f.100470.0.0,f.100480.0.0,f.100490.0.0,f.100500.0.0,f.100510.0.0,f.100520.0.0,
         f.100530.0.0,f.100540.0.0,f.100550.0.0,f.100560.0.0
         )

Tea=bd_beverage %>% 
  select(f.eid,f.100390.0.0,f.100400.0.0,
         f.100410.0.0,f.100420.0.0,f.100430.0.0,f.100440.0.0,f.100460.0.0,
         f.100470.0.0,f.100480.0.0,f.100490.0.0)

Coffee=bd_beverage %>% 
  select(f.eid,f.100240.0.0,f.100250.0.0,f.100260.0.0,f.100270.0.0,f.100280.0.0,
         f.100290.0.0,f.100300.0.0,f.100310.0.0,f.100320.0.0,f.100330.0.0,
         f.100350.0.0,f.100360.0.0,f.100370.0.0)



x=Coffee %>% na.omit()


############################################
### Detect the coffee drink 
############################################
registerDoParallel(8) 
x=Coffee
y=c()
for (i in 1:dim(x)[1]) {
  xa=x %>% select(-1)%>% slice(i)
  xb=na.omit(t(xa)) %>% as.data.frame()
  xc=paste(xb$V1, collapse="_")
  xc=ifelse(xc=="",NA,xc)
  #print(xb)
  y=c(y,xc)
}

end=x %>% mutate(Coffee=y) %>% 
  select(f.eid,Coffee) 

stopImplicitCluster()

############################################
### Detect the Tea drink 
############################################
registerDoParallel(8) 
x=Tea
y=c()
for (i in 1:dim(x)[1]) {
  xa=x %>% select(-1)%>% slice(i)
  xb=na.omit(t(xa)) %>% as.data.frame()
  xc=paste(xb$V1, collapse="_")
  xc=ifelse(xc=="",NA,xc)
  #print(xb)
  y=c(y,xc)
}

end2=x %>% mutate(Tea=y) %>% 
  select(f.eid,Tea) 

stopImplicitCluster()




############################################
### Detect the Tea drink 
############################################
df=left_join(register,death) %>% left_join(.,end) %>% left_join(.,end2) 



save(df,file = "df_coffee_tea.Rdata")














