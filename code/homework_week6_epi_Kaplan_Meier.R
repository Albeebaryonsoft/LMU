


df <- read.csv2("data/test.csv")

#draw stratified Kaplan-Meier curves for men and women.

  
surv_obj <- Surv(time = df$time, event = df$status)
km_fit <- survfit(surv_obj ~ sex, data = df)


library("survminer")
ggsurvplot(km_fit,
           data = df,
           pval = TRUE,             # 显示 log-rank 检验 p 值
           conf.int = TRUE,         # 加置信区间
           risk.table = TRUE,       # 图下显示在险人数
           legend.title = "Group",
           legend.labs = c("Male", "Female"),
           xlab = "Time (days)",
           ylab = "Overall Survival probability",
           ggtheme = theme_minimal())


#What is the median survival time for men and women?
#According to the plot, median survival time for man is 10 days, for women is 7 days. 


#What is the survival probability at day 5?
#The survival probability for man is at day 5 is 0.8, for women is 1. 