library(data.table)
ins_data <- fread("Data/SyntheticInsurerData.csv",data.table=FALSE)
CensorTime <- (781-ins_data$Month_of_Sale)/12
ins_data$EventTime <- rep(0,160781)
ins_data$EventTime[is.na(ins_data$Time_of_death)] <- CensorTime[is.na(ins_data$Time_of_death)]
ins_data$EventTime[!is.na(ins_data$Time_of_death)] <- ins_data$Time_of_death[!is.na(ins_data$Time_of_death)]
ins_data$Claim <- 1*(ins_data$Claim == "YES")

library(survival)
Cox_ph <- coxph(Surv(EventTime, Claim) ~ Month_of_Sale + Age + Sex  + BloodPressure +
                  Smoking + (BMI>30) , data=ins_data)
summary(Cox_ph)