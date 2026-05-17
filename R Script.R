#Sample Clinical Data

library(dplyr),
set.seed(123)
dm <- data.frame(USUBJID = paste0("SUBJ", 1:100), age = sample(25:75, 100, replace = TRUE),
                 sex = sample(c("M", "F"), 100, replace = TRUE), ARM = sample(c("placebo", "Drug"), 100, replace = TRUE))
dm
head(dm)

#Survival Dataset
ADTTE <- data.frame(USUBJID = dm$USUBJID, TRTGRP = dm$ARM, 
                    TIME = sample(30:50, 100, replace = TRUE), event = sample(c(0, 1),100, replace = TRUE)
)
ADTTE
head(ADTTE)

#Kaplan-Meier Survival Analysis

library(survival)
fit <- survfit(Surv(TIME, event) ~ TRTGRP, data = ADTTE)
fit

plot(fit, col = c("blue", "red"), lwd = 2, xlab = "days", ylab = "survival probability", main = "Kaplan-Meier Survival Curve")
legend("bottomleft", legend = levels(as.factor(ADTTE$TRTGRP)), col = c("blue", "red"), lwd = 2)

#Safety Analysis (ADAE)

ADAE <- data.frame(USUBJID = sample(dm$USUBJID, 200, replace = TRUE), AEDECOD = sample(c("Headache", "Nausea", "Fatigue"), 200, replace = TRUE), severity = sample(c("Mild", "Moderate", "Severe"), 200, replace = TRUE))
ADAE
head(ADAE)

#Adverse Event Summary Table Listing Figure
table(ADAE$AEDECOD)
table(ADAE$severity)

#Visualization
library(ggplot2)
ggplot(ADAE, aes(x = AEDECOD, fill = severity)) + geom_bar(position = "dodge") + labs(title = "Adverse Events by Severity", x = "Adverse Event", y = "Count")

#Create ADSL Dataset
ADSL <- dm %>% mutate(AGEGRP = ifelse(age < 50, "<50", "50+"))
ADSL

#Export Outputs
write.csv(ADSL, "ADSL.csv", row.names = FALSE)
write.csv(ADAE, "ADAE.csv", row.names = FALSE)

# Cox Proportional Hazard Model
cox_model <- coxph(Surv(TIME, event) ~ TRTGRP + age, data = merge(ADTTE, dm))
  cox_model
  summary(cox_model)
  
  fit <- survfit(cox_model)
  fit
  plot(fit)

  
