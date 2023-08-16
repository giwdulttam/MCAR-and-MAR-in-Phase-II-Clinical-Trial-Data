install.packages(dplyr)
library(dplyr)
install.packages(ggplot2)
library(ggplot2)
install.packages(mvtnorm)
library(mvtnorm)
library("cowplot")
install.packages("tidyr")
library("tidyr")



ADPA <- read.csv('/Users/matthewludwig/Desktop/ADPA.csv')
ADAE <- read.csv('/Users/matthewludwig/Desktop/ADAE.csv')
ADSL <- read.csv('/Users/matthewludwig/Desktop/ADSL.csv')


#1.)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#Make a copy of ADPA dataset and randomly select 10% patients and set their PASI scores at Visit 6 to be missing.
ADPA1 <- data.frame(ADPA)


#select all the rows for Visit 6
VISIT6 <- ADPA1[ADPA1$AVISITN == 6 & ADPA1$PARAMCD == 'PASI', ]
x = ceiling(length(unique(ADPA1[['SUBJID']])) / 10)
VISIT6_10 = slice_sample(VISIT6, n  =  x , replace=FALSE)
VISIT6_10['AVAL'] <- NA


#Dataframe of 10% of all patients with Visit 6 value replaced with NA
for (i in 1:x){
  ADPA1[ADPA1$AVISITN == 6 & ADPA1$PARAMCD == 'PASI' & ADPA1$USUBJID == VISIT6_10[i,'USUBJID'],'AVAL' ] <- NA
}

#NOTE: ******ADPA1 is dataframe with 10% of patient with Visit 6 value set to NA******



#2 a.)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Drop out any of the patients with missing data --> take out rows with USUBJID in VISIT6_10
#remove all rows with USUBJID in VISIT6_10

ADPA1_removed <- ADPA1

z = VISIT6_10$USUBJID
w = ADPA1$USUBJID

ADPA1_removed <- ADPA1_removed[!(w %in% z), ]
ADPA1_removed <- filter(ADPA1_removed, ADPA1_removed$PARAMCD == 'PASI' )


#NOTE: ****** (ADPA1_removed = dataframe with all patients with missing data removed...) ******
#Analyze by logistic regression using PASI75 with treatment and sex (from ADSL data)


#(Replace NA values with 0 for the first visit in PCHGCA2N of each patient)
ADPA1_removed$PCHGCA2N[is.na(ADPA1_removed$PCHGCA2N)] <- 0


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_removed['USUBJID','PCHGCA2N']
ADPA1_removed_subset <- select(ADPA1_removed, USUBJID, PCHGCA2N, AVISITN)
ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)

#NOTE: ADSL_subset has 198 observations, ADPA1_removed_subset has 890 observations --> only model PASI75 on VISIT = 6

#remove subject IDs with missing data from ADSL_subset  --> remove subjects in VISTI6_10
ADSL_subset_removed <- ADSL_subset[!( ADSL_subset$USUBJID %in% VISIT6_10$USUBJID), ]
ADSL_subset_removed

#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_removed_subset
ADPA1_removed_subset <- subset(ADPA1_removed_subset, ADPA1_removed_subset$AVISITN == 6)  
#***

#Combine ADPA1_removed_subset & ADSL_subset_removed
data1 <- cbind(ADPA1_removed_subset,ADSL_subset_removed) %>% select(-c(3,4))


#Logistic Regression Model
model1 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data1)

options(scipen=999)
summary(model1)


# Plot the results
p1 <- ggplot(data = data1, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = '10% percent missing, exclude missing data' ) + xlab("Treatment Arm (1-5)")


ggsave(file = "MLR - MCAR 10% - Drop Missing.png")



#2 b.) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ADPA1_imputed <- ADPA1

#Make any of the patients with missing data have PCGHN2N to be 0 --> that is rows with USUBJID in VISIT6_10, set PCHGCA2N to be 0

z = VISIT6_10$USUBJID
w = ADPA1$USUBJID

ADPA1_imputed$PCHGCA2N[(w %in% z)] <- 0
ADPA1_imputed <- filter(ADPA1_imputed, ADPA1_imputed$PARAMCD == 'PASI' )

#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_imputed['USUBJID','PCHGCA2N']
ADPA1_imputed_subset <- select(ADPA1_imputed, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_imputed_subset <- subset(ADPA1_imputed_subset, ADPA1_imputed_subset$AVISITN == 6)  


#Combine ADPA1_removed_subset & ADSL_subset_removed
data2 <- cbind(ADPA1_imputed_subset,ADSL_subset) %>% select(-c(3,4))


#Logistic Regression Model
model2 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data2)

options(scipen=999)
summary(model2)

# Plot the results
p2 <- ggplot(data = data2, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = '10% percent missing, missing data imputed' ) + xlab("Treatment Arm (1-5)") 


ggsave(file = "MLR - 10% MCAR - Imputed.png")



#2 c.) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





ADPA1_complete <- data.frame(ADPA)

ADPA1_complete <- filter(ADPA1_complete, ADPA1_complete$PARAMCD == 'PASI' )


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_complete['USUBJID','PCHGCA2N']
ADPA1_complete_subset <- select(ADPA1_complete, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_complete_subset <- subset(ADPA1_complete_subset, ADPA1_complete_subset$AVISITN == 6)  


#Combine ADPA1_complete_subset & ADSL_subset
data3 <- cbind(ADPA1_complete_subset,ADSL_subset) %>% select(-c(3,4))


#Logistic Regression Model
model3 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data3)

options(scipen=999)
summary(model3)

# Plot the results
p3 <- ggplot(data = data3, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = 'No missing data' ) + xlab("Treatment Arm (1-5)") 

ggsave(file = "MLR - (MCAR 10%) - Complete.png")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  
ggdraw() +
  draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p3, x = 0, y = 0, width = .5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))

ggsave(file = "Plots for 10% MCAR Data.png")


summary(model1)
summary(model2)
summary(model3)


#3.) - Repeat parts 1.) and 2.) for 20, 30% of data missing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#20% of data missing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Make a copy of ADPA dataset and randomly select 10% patients and set their PASI scores at Visit 6 to be missing.
ADPA1<-data.frame(ADPA)


#select all the rows for Visit 6
VISIT6<-ADPA1[ADPA1$AVISITN == 6 & ADPA1$PARAMCD == 'PASI', ]
y = ceiling(length(unique(ADPA1[['SUBJID']])) / 50)
VISIT6_10 = slice_sample(VISIT6, n  =  y , replace=FALSE)
VISIT6_10['AVAL'] <- NA


#Dataframe of 20% of all patients with Visit 6 value replaced with NA
for (i in 1:y){
  ADPA1[ADPA1$AVISITN == 6 & ADPA1$PARAMCD == 'PASI' & ADPA1$USUBJID == VISIT6_10[i,'USUBJID'],'AVAL' ] <- NA
}

#NOTE: ADPA1 is dataframe with 20% of patient with Visit 6 value set to NA


#Drop out any of the patients with missing data --> take out rows with USUBJID in VISIT6_10
#remove all rows with USUBJID in VISIT6_10

ADPA1_removed <- ADPA1

z = VISIT6_10$USUBJID
w = ADPA1$USUBJID

ADPA1_removed <- ADPA1_removed[!(w %in% z), ]
ADPA1_removed <- filter(ADPA1_removed, ADPA1_removed$PARAMCD == 'PASI' )
#(ADPA1_removed = dataframe with all patients with missing data removed...)


#Analyze by logistic regression using PASI75 with treatment and sex (from ADSL data)

#Replace NA values with 0 for the first visit in PCHGCA2N of each patient
ADPA1_removed$PCHGCA2N[is.na(ADPA1_removed$PCHGCA2N)] <- 0


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_removed['USUBJID','PCHGCA2N']
ADPA1_removed_subset <- select(ADPA1_removed, USUBJID, PCHGCA2N, AVISITN)
ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#remove subject IDs with missing data from ADSL_subset  --> remove subjects in VISTI6_10
ADSL_subset_removed <- ADSL_subset[!( ADSL_subset$USUBJID %in% VISIT6_10$USUBJID), ]
ADSL_subset_removed


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_removed_subset
ADPA1_removed_subset <- subset(ADPA1_removed_subset, ADPA1_removed_subset$AVISITN == 6)  


#Combine ADPA1_removed_subset & ADSL_subset_removed
data1 <- cbind(ADPA1_removed_subset, ADSL_subset_removed) %>% select(-c(3,4))


#Logistic Regression Model
model1 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data1)

options(scipen=999)
summary(model1)


# Plot the results
p1 <- ggplot(data = data1, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = '20% percent missing, only patients without missing data' ) + xlab("Treatment Arm (1-5)") 

ggsave(file = "MLR - MCAR 20% - Drop Missing.png")



ADPA1_imputed <- ADPA1

ADPA1_imputed$PCHGCA2N[(w %in% z)] <- 0

ADPA1_imputed <- filter(ADPA1_imputed, ADPA1_imputed$PARAMCD == 'PASI' )


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_imputed['USUBJID','PCHGCA2N']
ADPA1_imputed_subset <- select(ADPA1_imputed, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_imputed_subset <- subset(ADPA1_imputed_subset, ADPA1_imputed_subset$AVISITN == 6)  


#Combine ADPA1_removed_subset & ADSL_subset_removed
data2 <- cbind(ADPA1_imputed_subset,ADSL_subset) %>% select(-c(3,4))


#Logistic Regression Model
model2 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data2)

options(scipen=999)
summary(model2)

# Plot the results
p2 <- ggplot(data = data2, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = '20% percent missing, patients with missing data imputed' ) + xlab("Treatment Arm (1-5)") 


ggsave(file = "MLR - MCAR 20% - Imputed.png")

ADPA1_complete <- data.frame(ADPA)

ADPA1_complete <- filter(ADPA1_complete, ADPA1_complete$PARAMCD == 'PASI' )


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_complete['USUBJID','PCHGCA2N']
ADPA1_complete_subset <- select(ADPA1_complete, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_complete_subset <- subset(ADPA1_complete_subset, ADPA1_complete_subset$AVISITN == 6)  


#Combine ADPA1_complete_subset & ADSL_subset
data3 <- cbind(ADPA1_complete_subset,ADSL_subset) %>% select(-c(3,4))


#Logistic Regression Model
model3 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data3)

options(scipen=999)
summary(model3)

# Plot the results
p3 <- ggplot(data = data3, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = 'No missing data' ) + xlab("Treatment Arm (1-5)") 

ggsave(file = "MLR - MCAR 20% - Complete.png")



ggdraw() +
  draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p3, x = 0, y = 0, width = .5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))

ggsave(file = "Plots for 20% MCAR Data.png")





#30% of data missing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Make a copy of ADPA dataset and randomly select 30% patients and set their PASI scores at Visit 6 to be missing.
ADPA1<-data.frame(ADPA)


#select all the rows for Visit 6
VISIT6<-ADPA1[ADPA1$AVISITN == 6 & ADPA1$PARAMCD == 'PASI', ]
q = ceiling(length(unique(ADPA1[['SUBJID']])) / 80)
VISIT6_10 = slice_sample(VISIT6, n  =  q , replace=FALSE)
VISIT6_10['AVAL'] <- NA


#Dataframe of 30% of all patients with Visit 6 value replaced with NA
for (i in 1:q){
  ADPA1[ADPA1$AVISITN == 6 & ADPA1$PARAMCD == 'PASI' & ADPA1$USUBJID == VISIT6_10[i,'USUBJID'],'AVAL' ] <- NA
}

#NOTE: ADPA1 is dataframe with 10% of patient with Visit 6 value set to NA


#Drop out any of the patients with missing data --> take out rows with USUBJID in VISIT6_10
#remove all rows with USUBJID in VISIT6_10

ADPA1_removed <- ADPA1

z = VISIT6_10$USUBJID
w = ADPA1$USUBJID

ADPA1_removed <- ADPA1_removed[!(w %in% z), ]
ADPA1_removed <- filter(ADPA1_removed, ADPA1_removed$PARAMCD == 'PASI' )
#(ADPA1_removed = dataframe with all patients with missing data removed...)


#Analyze by logistic regression using PASI75 with treatment and sex (from ADSL data)


#Replace NA values with 0 for the first visit in PCHGCA2N of each patient
ADPA1_removed$PCHGCA2N[is.na(ADPA1_removed$PCHGCA2N)] <- 0


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_removed['USUBJID','PCHGCA2N']
ADPA1_removed_subset <- select(ADPA1_removed, USUBJID, PCHGCA2N, AVISITN)
ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#NOTE: ADSL_subset has 198 observations, ADPA1_removed_subset has 890 observations --> only model PASI75 on VISIT = 6


#remove subject IDs with missing data from ADSL_subset  --> remove subjects in VISTI6_10
ADSL_subset_removed <- ADSL_subset[!( ADSL_subset$USUBJID %in% VISIT6_10$USUBJID), ]
ADSL_subset_removed


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_removed_subset
ADPA1_removed_subset <- subset(ADPA1_removed_subset, ADPA1_removed_subset$AVISITN == 6)  


#Combine ADPA1_removed_subset & ADSL_subset_removed
data1 <- cbind(ADPA1_removed_subset,ADSL_subset_removed) %>% select(-c(3,4))


#Logistic Regression Model
model1 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data1)

options(scipen=999)
summary(model1)


# Plot the results
p1 <- ggplot(data = data1, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = '30% percent missing, only patients without missing data' ) + xlab("Treatment Arm (1-5)") 

ggsave(file = "MLR - MCAR 30% - Drop Missing.png")


#Question: Are we only modeling PASI75 patient data for VISIT = 6 ???



ADPA1_imputed <- ADPA1

ADPA1_imputed$PCHGCA2N[(w %in% z)] <- 0

ADPA1_imputed <- filter(ADPA1_imputed, ADPA1_imputed$PARAMCD == 'PASI' )


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_imputed['USUBJID','PCHGCA2N']
ADPA1_imputed_subset <- select(ADPA1_imputed, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_imputed_subset <- subset(ADPA1_imputed_subset, ADPA1_imputed_subset$AVISITN == 6)  


#Combine ADPA1_removed_subset & ADSL_subset_removed
data2 <- cbind(ADPA1_imputed_subset,ADSL_subset) %>% select(-c(3,4))


#Logistic Regression Model
model2 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data2)

options(scipen=999)
summary(model2)

# Plot the results
p2 <- ggplot(data = data2, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = '30% percent missing, patients with missing data imputed' ) + xlab("Treatment Arm (1-5)") 


ggsave(file = "MLR - MCAR 30% - Imputed.png")


ADPA1_complete <- data.frame(ADPA)

ADPA1_complete <- filter(ADPA1_complete, ADPA1_complete$PARAMCD == 'PASI' )


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA1_complete['USUBJID','PCHGCA2N']
ADPA1_complete_subset <- select(ADPA1_complete, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#Remove visits 1-5 in ADPA1_removed_subset
ADPA1_complete_subset <- subset(ADPA1_complete_subset, ADPA1_complete_subset$AVISITN == 6)  


#Combine ADPA1_complete_subset & ADSL_subset
data3 <- cbind(ADPA1_complete_subset, ADSL_subset) %>% select(-c(3,4))


#Logistic Regression Model
model3 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data3)

options(scipen=999)
summary(model3)

# Plot the results
p3 <- ggplot(data = data3, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = 'No missing data' ) + xlab("Treatment Arm (1-5)") 

ggsave(file = "Multiple_Logistic_Regression - Complete.png")


ggdraw() +
  draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p3, x = 0, y = 0, width = .5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))

ggsave(file = "Plots for 30% MCAR Data.png")





#4.) Generate MAR data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ADPA2 <- select(ADPA, USUBJID, SUBJID, AVISITN, TRTP, TRTPN, AVAL, BASE, PCHG, PCHGCA2N, PARAMCD)
ADPA2 <- ADPA2[ADPA2$PARAMCD == 'PASI',]


for (i in 1:198){

  #visit 4 values
  if(ADPA2[ADPA2$AVISITN == 3 & ADPA2$SUBJID == i, "AVAL"] > ADPA2[ADPA2$AVISITN == 3 & ADPA2$SUBJID == i, "BASE"] | ADPA2[ADPA2$AVISITN == 3 & ADPA2$SUBJID == i, "PCHG"] < 10){

    if(runif(1) < .3){
    ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "AVAL"] <- NA
    ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
    }
    
  }else{
    
    if(runif(1) < .05){
      ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "AVAL"] <- NA
      ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
      }
  }
  
  #visit 5 values
  if(is.na(ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "AVAL"])){
    ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "AVAL"] <- NA
    ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
  }else{
    
    if(ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "AVAL"] > ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "BASE"] | ADPA2[ADPA2$AVISITN == 4 & ADPA2$SUBJID == i, "PCHG"] < 10){
      
      if(runif(1) < .3){
        ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "AVAL"] <- NA
        ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
        
      }
      
    }else{
      
      if(runif(1) < .05){
        ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "AVAL"] <- NA
        ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
      }
    }
    
  }
  
  #visit 6 values
  if(is.na(ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "AVAL"])){
    ADPA2[ADPA2$AVISITN == 6 & ADPA2$SUBJID == i, "AVAL"] <- NA
    ADPA2[ADPA2$AVISITN == 6 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
  }else{
    
    if(ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "AVAL"] > ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "BASE"] | ADPA2[ADPA2$AVISITN == 5 & ADPA2$SUBJID == i, "PCHG"] < 10){
      
      if(runif(1) < .3){
        ADPA2[ADPA2$AVISITN == 6 & ADPA2$SUBJID == i, "AVAL"] <- NA
        ADPA2[ADPA2$AVISITN == 6 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
      }
      
    }else{
      
      if(runif(1) < .05){
        ADPA2[ADPA2$AVISITN == 6 & ADPA2$SUBJID == i, "AVAL"] <- NA
        ADPA2[ADPA2$AVISITN == 6 & ADPA2$SUBJID == i, "PCHGCA2N"] <- NA
      }
    }
  }
  }

  



#5.) (Repeat step 2 using ADPA2) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Same code as step 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Drop out any of the patients with missing data --> take out all patients that have NA values

ADPA2_removed = ADPA2

#remove all patients from ADPA2 that have NA values in AVAL and PCHGCA2N
ADPA2_missing_rows = ADPA2_removed[(is.na(ADPA2_removed$"PCHGCA2N")), ]
ADPA2_removed = ADPA2_removed[!(is.na(ADPA2_removed$"PCHGCA2N")), ]


#Remove visits 1-5 in ADPA2_removed
ADPA2_removed_subset <- subset(ADPA2_removed, ADPA2_removed$AVISITN == 6)  
ADPA2_missing_rows_subset <- subset(ADPA2_missing_rows, ADPA2_missing_rows$AVISITN == 6)  



#Analyze by logistic regression using PASI75 with treatment and sex (from ADSL data) --> ADPA2_removed_subset

#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA2_removed['USUBJID','PCHGCA2N']
ADPA2_removed_subset <- select(ADPA2_removed_subset, USUBJID, PCHGCA2N, AVISITN)
ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)




#NOTE: ADSL_subset and ADPA2_removed_subset have different number of rows...

#remove subject IDs with missing data from ADSL_subset  --> remove subjects in ADPA2_missing_rows
ADSL_subset_removed <- ADSL_subset[!( ADSL_subset$USUBJID %in% ADPA2_missing_rows_subset$USUBJID), ]



#Combine ADPA2_removed_subset & ADSL_subset_removed
data4 <- cbind(ADPA2_removed_subset,ADSL_subset_removed) %>% select(-c(3,4))


#Logistic Regression Model
model4 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data4)

options(scipen=999)
summary(model1)


# Plot the results
p4 <- ggplot(data = data4, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = 'MAR, exclude missing data' ) + xlab("Treatment Arm (1-5)") 

ggsave(file = "MLR - MAR - Drop Missing.png")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#b.)


ADPA2_imputed = ADPA2

#for all patients from ADPA2 that have NA values in AVAL and PCHGCA2N, set NA to be 0

ADPA2_imputed$PCHGCA2N[is.na(ADPA2_imputed$PCHGCA2N)] <- 0
ADPA2_imputed$PCHGCA2N[is.na(ADPA2_imputed$AVAL)] <- 0

ADPA2_imputed <- filter(ADPA2_imputed, ADPA2_imputed$PARAMCD == 'PASI' )


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA2_imputed['USUBJID','PCHGCA2N']
ADPA2_imputed_subset <- select(ADPA2_imputed, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)

#Remove visits 1-5 in ADPA1_removed_subset
ADPA2_imputed_subset <- subset(ADPA2_imputed_subset, ADPA2_imputed_subset$AVISITN == 6)  

#Combine ADPA2_removed_subset & ADSL_subset_removed
data5 <- cbind(ADPA2_imputed_subset,ADSL_subset) %>% select(-c(3,4))






#Logistic Regression Model
model5 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data5)

options(scipen=999)
summary(model5)

# Plot the results
p5 <- ggplot(data = data5, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = 'MAR, missing data imputed' ) + xlab("Treatment Arm (1-5)") 


ggsave(file = "MLR - MAR - Imputed.png")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#c.)


ADPA2_complete = data.frame(ADPA)

ADPA2_complete <- filter(ADPA2_complete, ADPA2_complete$PARAMCD == 'PASI' )


#Make a combined dataframe with PASI75, treatment, sex -- based on USUBJID
ADPA2_complete['USUBJID','PCHGCA2N']
ADPA2_complete_subset <- select(ADPA2_complete, USUBJID, PCHGCA2N, AVISITN)

ADSL_subset <- select(ADSL,USUBJID, SEXN, ARMN)


#Remove visits 1-5 in ADPA1_removed_subset
ADPA2_complete_subset <- subset(ADPA2_complete_subset, ADPA2_complete_subset$AVISITN == 6)  


#Combine ADPA1_complete_subset & ADSL_subset
data6 <- cbind(ADPA1_complete_subset,ADSL_subset) %>% select(-c(3,4))


#Logistic Regression Model
model6 <- glm(PCHGCA2N ~ SEXN + ARMN, family="binomial", data=data6)

options(scipen=999)
summary(model6)

# Plot the results
p6 <- ggplot(data = data6, aes(x = ARMN, y = PCHGCA2N)) +
  geom_point(aes(colour = factor(SEXN))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + ggtitle("PASI75 vs. Treatment and Sex", subtitle = 'No missing data' ) + xlab("Treatment Arm (1-5)") 

ggsave(file = "MLR - MAR - Complete.png")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



ggdraw() +
  draw_plot(p1, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(p2, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(p3, x = 0, y = 0, width = .5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0.5, 0), y = c(1, 1, 0.5))

ggsave(file = "Plots for MAR.png")





#6.) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create MNAR mechanisms

# 
# 
# ADPA3<-data.frame(ADPA)
# 
# 
# 
# 
# #use AESTDY values of date of event from start of study to build correlation model
# 
# 
# AESEV <- ADAE["AESEV"]
# 
# 
# ADPA3 <- ADPA3 %>% mutate(Toxicity_Index=NA) %>% mutate(AESEV=NA)
# 
# 
# for(i in 1:193){
#   
#   if( ){
#   ADPA3[ADPA3$SUBID == i, "AVAL"] <- 
#       
#     
#   }

  














