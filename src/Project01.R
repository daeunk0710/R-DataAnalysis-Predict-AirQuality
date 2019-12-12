CEEproject1_1 = read.csv("Pro1_Data_all.csv")
CEEproject1_2 <- CEEproject1_1[-c(1:3)]
attach(CEEproject1_2)
contrasts(metropolitan)

lm.fit = lm(car~., data=CEEproject1_2)
summary(lm.fit)

lm.fit1 = lm(car~metropolitan+pop+age_mean+age_median+pop_c_m+pop_c_w
             +distance_1+distance_2+distance_3)
summary(lm.fit1)

lm.fit2 = lm(car~metropolitan+pop+age_mean+age_median+pop_c_m+pop_c_w
             +distance_1+time_15+time_60.90+time_90.120)
summary(lm.fit2)

lm.fit3 = lm(car~metropolitan+pop+age_mean+age_median+pop_c_m+pop_c_w
             +distance_1+time_15+time_60.90+time_90.120+nc_car_dis
             +pt_n+pt_c+trans_n+trans_tt+trans_wt)
summary(lm.fit3)

lm.fit31 = update(lm.fit3, ~.-metropolitan-pop_c_m-distance_1-time_15
                  -pt_c-trans_n-trans_wt)
summary(lm.fit31)

install.packages('cellranger')
install.packages('car')
library(car)
vif(lm.fit31)

lm.fit32 = update(lm.fit31, ~.-pop -age_mean -age_median
                  -pop_c_w -time_60.90 -time_90.120)
summary(lm.fit32)

plot(CEEproject1_2, ph=20)
cor(CEEproject1_2)
library(ISLR)
