library(outliers)
outliers_prueba<-ajuste_RPM_Resistencia(df)
RPM_outliers<-sapply(outliers_prueba, "[",,1)
sapply(RPM_outliers, length)
