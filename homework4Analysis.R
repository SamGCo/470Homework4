if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

#1 Remove all SNPs, 800-series plans, and prescription drug only plans (i.e., plans that do not offer Part C benefits). Provide a box and whisker plot showing the distribution of plan counts by county over time. Do you think that the number of plans is sufficient, too few, or too many?
finalData<-read_rds("data/output/final_ma_data.rds")

final1Data<-finalData%>%
  group_by(fips,year) %>%
  select(fips,year)%>% summarize(planCount=n())%>%
  ungroup() 


boxplot(planCount ~ year, data = final1Data, xlab = "Year", ylab = "Mean Value", main = "Boxplot of Mean Values by Year",ylim=c(0,100))
#2
finalData%>%
  filter((year==2012 | year==2015 | year==2009) & !is.na(Star_Rating))%>%
  ggplot( aes(x = factor(Star_Rating), fill = as.factor(year),na.rm=T)) + 
  geom_bar(position = "dodge") + 
  labs(x = "Year", y = "Count", fill = "Star_Rating") + 
  ggtitle("Count of Each Star Rating for Each Year")+theme_bw()
# As time goes on, the count of 3.5 plans and higher increase. Overall, this data seems to indicate the average star rating has increased over this time.
#3
finalData%>%
  filter(year>2008 & year<2016)%>%
  ggplot( aes(x = year, y = ma_rate)) + 
  stat_summary(fun = "mean", geom = "bar") +
  labs(x = "Year", y = "Average Benchmark Payment") + 
  ggtitle("Average Benchmark Payment Each Year")
# From my graph, it seems the MA rate for benchmark payments has not changed very much over the years, but there was a slight increase in the trend of the payments.

#4

number4<-finalData%>%
  filter(year>2008 & year<2016)%>%
  mutate(nonMA=(parta_enroll+partb_enroll))%>%
  mutate(advShare=(avg_enrolled/(avg_enrolled+nonMA)))
number4%>%
  ggplot( aes(x = year, y = advShare)) + 
  stat_summary(fun = "mean", geom = "bar") +
  labs(x = "Year", y = "Share of Medicare Advantage") + 
  ggtitle("Average Medicare Advantage Share from 2009-2015")
#This data shows that the Medicare Advantage has increased in popularity over the years, directly correlating with the change in payments.

#5
ateFinalData<-finalData%>%
  filter(year==2009)


ateTest<-ateFinalData%>%
mutate(raw_rating=rowMeans(
  cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
        glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
        mental_health,osteo_test,physical_monitor,primaryaccess,
        hospital_followup,depression_followup,nodelays,carequickly,
        overallrating_care,overallrating_plan,calltime,
        doctor_communicate,customer_service,osteo_manage,
        diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
        diabetes_chol,antidepressant,bloodpressure,ra_manage,
        copd_test,betablocker,bladder,falling,appeals_timely,
        appeals_review),
  na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, first_enrolled,
         last_enrolled, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate,partd,plan_type)

# ateTest$roundedRatings<-round(ateTest$raw_rating * 2) / 2
# 
# # create a table of the number of plans rounded to each rating
# table(rounded_ratings)
# 
# # create a table of the breakdown of ratings
# breakdown <- data.frame(
#   Rating = seq(1, 5, by = 0.5),
#   Rounded_Up = sapply(seq(1, 5, by = 0.5), function(x) sum(ateTest$roundedRatings >= x)),
#   Rounded_Down = sapply(seq(1.5, 5, by = 0.5), function(x) sum(ateTest$roundedRatings == x))
# )
# breakdown
# rounded_values2 <- cut(ateTest$raw_rating, breaks = seq(1, 5.5, by = 0.5), labels = seq(1, 5, by = 0.5), include.lowest = TRUE)
number5<-table(ateTest$Star_Rating)
print(number5)

#6
ateTest<- ateTest %>%
  mutate(score = raw_rating - 2.25,
         treat = (score>=0),
         window1 = (score>=-.175 & score<=.175),
         window2 = (score>=-.125 & score<=.125),
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share),
         score_treat=score*treat)
star25.1 <- lm(avg_enrollment ~ score + treat, data=ateTest)
star25.2 <- lm(avg_enrollment ~ score + treat, data= (ateTest %>% filter(window1==TRUE)))
star25.3 <- lm(avg_enrollment ~ score + treat + score_treat, data= (ateTest %>% filter(window1==TRUE)))
star25.4 <- lm(avg_enrollment ~ score + treat + score_treat, data= (ateTest %>% filter(window2==TRUE)))
est1 <- as.numeric(star25.1$coef[3])
est2 <- as.numeric(star25.2$coef[3])
est3 <- as.numeric(star25.3$coef[3])
est4 <- as.numeric(star25.4$coef[3])
est1 <- rdrobust::rdrobust(y=ateTest$avg_enrollment, x=ateTest$score, c=0,
                 h=0.125, p=1, kernel="uniform", vce="hc0",
                 masspoints="off")
est2 <- rdrobust::rdrobust(y=ateTest$avg_enrollment, x=ateTest$score, c=.5,
                           h=0.125, p=1, kernel="uniform", vce="hc0",
                           masspoints="off")
est3 <- rdrobust::rdrobust(y=ateTest$avg_enrollment, x=ateTest$score, c=1,
                           h=0.125, p=1, kernel="uniform", vce="hc0",
                           masspoints="off")
est4 <- rdrobust::rdrobust(y=ateTest$avg_enrollment, x=ateTest$score, c=1.5,
                           h=0.125, p=1, kernel="uniform", vce="hc0",
                           masspoints="off")
modelList<-list(est1,est2,est3,est4)

#7
h_values <- c(0.1, 0.12, 0.13, 0.14, 0.15)
c_values<-c(0,.5,1,1.5)
results_list <- list()
for(c in c_values){
   for (h in h_values) {
rdrobust::rdplot(y=ateTest$avg_enrollment,h=h,binselect="es",x=(ateTest$score+c),title = "Average Enrollment RD Plot",x.label = "Score",y.label="Average Enrollment",masspoints="off")
   }}
save.image("workspace.RData")
# # Loop over values of h and run rdrobust()
# results_list <- list()
# 
#   est <- rdrobust::rdrobust(
#     y = ateTest$avg_enrollment,
#     x = ateTest$score,
#     c = c,
#     h = h,
#     p = 1,
#     kernel = "uniform",
#     vce = "hc0",
#     masspoints = "off"
#     
#   )
#   var_name<-paste0("h", h, "_c", c)
#   assign(var_name,est)
#   #results_list<-add(var_name)
# }
# }
# counth<-1
# countc<-0
# number7<-data.frame(matrix(nrow=0,ncol=4))
# colnames(number7)<-c("2.25","2.75","3.25","3.75")
# for(c in c_values){
#   countc<-countc+1
#   for (h in h_values) {
#     var_name <- paste0("h", h, "_c", c)
#     number7[counth,countc]<-get(var_name)$Estimate
#     counth<-counth+1
#   }
# }
# summary(h0.1_c0)
# coef_list <- list()
# for (i in seq_along(results_list)) {
#   coef_list[[i]] <- as.data.frame(results_list[[i]]$coefficients)
# }
# 
# # Combine the coefficients into a single data frame
# coef_df <- do.call(rbind, coef_list)
# 
# # Add columns for the values of c and h
# coef_df$c <- rep(c_values, each = length(h_values))
# coef_df$h <- rep(h_values, length(c_values))
# 
# # Plot the estimated coefficients
# ggplot(coef_df, aes(x = h, y = Estimate, group = c, color = factor(c))) +
#   geom_line() +
#   labs(x = "h", y = "Estimated coefficient", color = "c") +
#   theme_bw()

