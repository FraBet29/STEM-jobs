# ------------------------------------------------------------------------------
# PREPROCESSING DATI
# ------------------------------------------------------------------------------

graphics.off()

data = read.csv("Levels_Fyi_Salary_Data.csv")

data = data[, c(2,4,5,6,7,13,29)]

data = subset(data, gender == "Male" | gender == "Female")
data = subset(data, !is.na(Education))
data = subset(data, Education != "Some College" & Education != "Highschool")

companies = prop.table(table(data$company))
top_companies = sort(companies, decreasing = TRUE)[1:5]
data = subset(data, (company %in% names(top_companies)))
# Amazon, Apple, Microsoft, Google, Facebook

titles = prop.table(table(data$title))
sort(titles, decreasing = TRUE)
data = subset(data, title == "Software Engineer" | title == "Software Engineering Manager" | title == "Technical Program Manager" | 
                title == "Data Scientist" | title == "Hardware Engineer")

USA = function(x){
  return(substr(x, nchar(x)-3, nchar(x)-3) == ",")
}

data = subset(data, USA(location))

data$company = as.factor(data$company)
data$title = as.factor(data$title)
data$location = as.factor(data$location)
data$gender = as.factor(data$gender)
data$Education = as.factor(data$Education)

titles = prop.table(table(data$title))

summary(data)

# ------------------------------------------------------------------------------
# VISUALIZZAZIONE DATI
# ------------------------------------------------------------------------------

library(ggplot2)
library(GGally)
library(MASS)

ggplot(data, aes(x=totalyearlycompensation, y=title)) + 
  geom_boxplot() +
  theme(axis.title.y = element_blank(), axis.title=element_text(size=10, face="plain")) + 
  labs(x = "Total Yearly Compensation")

ggplot(data, aes(x=totalyearlycompensation, y=company)) + 
  geom_boxplot() +
  theme(axis.title.y = element_blank(), axis.title=element_text(size=10, face="plain")) + 
  labs(x = "Total Yearly Compensation")

ggplot(data, aes(x=totalyearlycompensation, y=title, fill=factor(gender))) + 
  geom_boxplot() +
  theme(axis.title.y = element_blank(), axis.title=element_text(size=10, face="plain"), legend.position = "none") + 
  labs(x = "Total Yearly Compensation")

ggplot(data, aes(x=totalyearlycompensation, y=company, fill=factor(gender))) + 
  geom_boxplot() +
  theme(axis.title.y = element_blank(), axis.title=element_text(size=10, face="plain"), legend.position = "none") + 
  labs(x = "Total Yearly Compensation")

mean(data$totalyearlycompensation[which(data$gender == "Male")])
mean(data$totalyearlycompensation[which(data$gender == "Female")])

ggplot(data, aes(x=totalyearlycompensation, y=Education)) + 
  geom_boxplot() +
  theme(axis.title.y = element_blank(), axis.title=element_text(size=10, face="plain")) + 
  labs(x = "Total Yearly Compensation")

mean(data$totalyearlycompensation[which(data$Education == "Bachelor's Degree")])
mean(data$totalyearlycompensation[which(data$Education == "Master's Degree")])
mean(data$totalyearlycompensation[which(data$Education == "PhD")])

ggpairs(data[, c(3,5)])

# ------------------------------------------------------------------------------
# ANOVA
# ------------------------------------------------------------------------------

# Confronto tra titoli

aov_titles = aov(data$totalyearlycompensation ~ data$title)
# ANOVA prende Data Scientist come gruppo di riferimento
summary(aov_titles)
aov_titles$coefficients # scostamento dei vari gruppi dal gruppo di riferimento
# C'è almeno una categoria con la media significativamente diversa... quale?
pairwise.wilcox.test(data$totalyearlycompensation, data$title)
mean_title = tapply(data$totalyearlycompensation, data$title, mean)

# Controllo delle ipotesi

qqnorm(aov_titles$residuals)
qqline(aov_titles$residuals, col = "red") # code molto pesanti

box = boxcox(data$totalyearlycompensation ~ data$title)
lambda = box$x[which.max(box$y)] # -2/3
lambda = -1

aov_title_box = aov((totalyearlycompensation^lambda - 1)/lambda ~ title, data = data)
qqnorm(aov_title_box$residuals)
qqline(aov_title_box$residuals, col = "red") # code molto pesanti

# Non usiamo Bartlett test perché la normalità non è confermata
library(car)
leveneTest((data$totalyearlycompensation^lambda - 1)/lambda, data$title)
# Le varianze tra i gruppi non sono uguali

# Proviamo con il test Kruskal-Wallis che non richiede l'ipotesi di normalità
kruskal.test(data$totalyearlycompensation, data$title)
# Stesso risultato dell'ANOVA

# Confronto tra aziende

aov_companies = aov(data$totalyearlycompensation ~ data$company)
# ANOVA prende Amazon come gruppo di riferimento
summary(aov_companies)
aov_companies$coefficients
# Cè almeno una categoria con la media significativamente diversa... quale?
pairwise.wilcox.test(data$totalyearlycompensation, data$company)
mean_companies = tapply(data$totalyearlycompensation, data$company, mean)

# Controllo delle ipotesi

qqnorm(aov_companies$residuals)
qqline(aov_companies$residuals, col = "red") # code molto pesanti

box = boxcox(data$totalyearlycompensation ~ data$company)
lambda = box$x[which.max(box$y)] # -2/3
lambda = -1

aov_companies_box = aov((totalyearlycompensation^lambda - 1)/lambda ~ company, data = data)
qqnorm(aov_companies_box$residuals)
qqline(aov_companies_box$residuals, col = "red") # code molto pesanti

# Non usiamo Bartlett test perché la normalità non è confermata
leveneTest((data$totalyearlycompensation^lambda - 1)/lambda, data$company)
# Le varianze tra i gruppi non sono uguali

# Proviamo con il test Kruskal-Wallis che non richiede l'ipotesi di normalità
kruskal.test(data$totalyearlycompensation, data$company)
# Stesso risultato dell'ANOVA

# Confronto tra livelli di educazione

aov_ed = aov(data$totalyearlycompensation ~ data$Education)
# ANOVA prende bachelor come gruppo di riferimento
summary(aov_ed)
aov_ed$coefficients
# C'è almeno una categoria con la media significativamente diversa... quale?
pairwise.wilcox.test(data$totalyearlycompensation, data$Education)
mean_ed = tapply(data$totalyearlycompensation, data$Education, mean)

# Controllo delle ipotesi

qqnorm(aov_ed$residuals)
qqline(aov_ed$residuals, col = "red") # code molto pesanti

box = boxcox(data$totalyearlycompensation ~ data$Education)
lambda = box$x[which.max(box$y)] # -2/3
lambda = -1

aov_ed_box = aov((totalyearlycompensation^lambda - 1)/lambda ~ Education, data = data)
qqnorm(aov_ed_box$residuals)
qqline(aov_ed_box$residuals, col = "red") # code molto pesanti

# Non usiamo Bartlett test perché la normalità non è confermata
leveneTest((data$totalyearlycompensation^lambda - 1)/lambda, data$Education)
# Le varianze tra i gruppi non sono uguali

# Proviamo con il test Kruskal-Wallis che non richiede l'ipotesi di normalità
kruskal.test(data$totalyearlycompensation, data$Education)

# Identificazione dei gruppi significativamente diversi
pairwise.wilcox.test(data$totalyearlycompensation, data$title)
pairwise.wilcox.test(data$totalyearlycompensation, data$company)
pairwise.wilcox.test(data$totalyearlycompensation, data$Education)
# Titoli lavorativi: il gruppo con la media significativamente diversa è Software Engineering Manager
# Aziende: tutti i gruppi hanno la media significativamente diversa
# Educazione: tutti i gruppi hanno la media significativamente diversa

#-------------------------------------------------------------------------------
# REGRESSIONE LINEARE - MODELLO PER TITOLO E AZIENDA (AGGREGATO)
#-------------------------------------------------------------------------------

t = "Software Engineer"

data_t = data[which(data$title == t), ]
mod_tc = lm(totalyearlycompensation ~ yearsofexperience + company, data = data_t) # abbiamo provato con logaritmo (bad) radice (un po' meglio)
summary(mod_tc)

plot(mod_tc$fit, mod_tc$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16, cex = 0.5)
abline(h = 0, lwd = 2, lty = 2, col = 'red')

qqnorm(mod_tc$residuals)
qqline(mod_tc$residuals, col = "red")
shapiro.test(mod_tc$residuals)

box = boxcox(data_t$totalyearlycompensation ~ data_t$yearsofexperience + data_t$company)
lambda = box$x[which.max(box$y)]
lambda = -0.5

mod_tcbox = lm((totalyearlycompensation^lambda - 1)/lambda ~ yearsofexperience + company, data = data_t)
summary(mod_tcbox)

plot(mod_tcbox$fit, mod_tcbox$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16, cex = 0.5)
abline(h = 0, lwd = 2, lty = 2, col = 'red')

plot(mod_tcbox, which = 1) # ci sono sempre outlier e punti leva

qqnorm(mod_tcbox$residuals)
qqline(mod_tcbox$residuals, col = "red")
shapiro.test(mod_tcbox$residuals) # p-value = 6e-13
# Però lo Shapiro test non è attendibile per campioni numerosi

# Dal modello si vede che le aziende sono covariate significative
# Ma non è molto interpretabile

#-------------------------------------------------------------------------------
# REGRESSIONE LINEARE - MODELLO PER TITOLO E AZIENDA (SEPARATO)
# ------------------------------------------------------------------------------

t = "Software Engineer"
c = "Apple"

data_tc = data_t[which(data_t$company == c), ]

h = hist(data_tc$totalyearlycompensation, breaks = 25, probability = TRUE)
brks=h$breaks
ggplot(data_tc, aes(x=totalyearlycompensation)) +
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue", stat="bin",breaks = brks, right = TRUE) +
  geom_density(alpha=.2, fill="#FF6666") +
  theme(axis.title.y = element_text(size=10, face="plain"), axis.title=element_text(size=10, face="plain")) + 
  labs(x = "Total Yearly Compensation", y = "Density") + 
  ggtitle(paste0("Azienda: ", c, "\nTitolo: ", t))

plot(data_tc$yearsofexperience, data_tc$totalyearlycompensation, cex = 0.5, pch = 16, 
     xlab = "Years of Experience", ylab = "Total Yearly Compensation")

mod_tc = lm(data_tc$totalyearlycompensation ~ data_tc$yearsofexperience)
summary(mod_tc)
plot(mod_tc, which=1)

qqnorm(mod_tc$residuals)
qqline(mod_tc$residuals, col = "red")
shapiro.test(mod_tc$residuals)

box = boxcox(data_tc$totalyearlycompensation ~ data_tc$yearsofexperience)
lambda = box$x[which.max(box$y)]
lambda
lambdascelto=readline(prompt="inserisci lambda: ")
# Amazon: lambda = -1
# Apple: lambda = 0
# Microsoft: lambda = -1
# Facebook: lambda = 0
# Google: lambda = 0

if (lambdascelto==0){
  mod_tcbox = lm(log(data_tc$totalyearlycompensation) ~ data_tc$yearsofexperience)
}else {
  mod_tcbox = lm( (data_tc$totalyearlycompensation^lambda - 1)/lambda ~ data_tc$yearsofexperience)
}
summary(mod_tcbox)

qqnorm(mod_tcbox$residuals)
qqline(mod_tcbox$residuals, col = "red")
shapiro.test(mod_tcbox$residuals)

h0=hist(mod_tcbox$residuals, breaks = 15, probability = TRUE)
brks0=h0$breaks

ggplot(mod_tcbox, aes(x=mod_tcbox$residuals)) +
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue", stat="bin", breaks = brks0, right = TRUE) +
  geom_density(alpha=.2, fill="#FF6666") +
  theme(axis.title.y = element_text(size=10, face="plain"), axis.title=element_text(size=10, face="plain")) + 
  labs(x = "Residuals", y = "Density")

# ------------------------------------------------------------------------------
# Rimozione punti leva
# ------------------------------------------------------------------------------

X = model.matrix(mod_tc)
lev = hat(X)
p = mod_tc$rank # rango della matrice X
n = dim(data_tc)[1] # dimensione del dataset

# Plot vari
# plot(mod_tc$fitted.values, lev, pch = 16)
# abline(h = 2*p/n, lty = 2, col = 'red')
# influencePlot(mod_tc, id.method = "identify")
# plot(mod_tc, which = 5)
# library(olsrr)
# ols_plot_resid_lev(mod_tc)

watchout_data_tc = which(lev > 2*p/n)
plot(data_tc$yearsofexperience, data_tc$totalyearlycompensation, cex = 0.5, pch = 16,
     xlab = "Years of Experience", ylab = "Total Yearly Compensation")
points(data_tc$yearsofexperience[watchout_data_tc], data_tc$totalyearlycompensation[watchout_data_tc], col = 'red', 
       cex = 0.5, pch = 16, xlab = "Years of Experience", ylab = "Total Yearly Compensation")

data_tcl = subset(data_tc, lev < 2*p/n)

mod_tcl = lm(data_tcl$totalyearlycompensation ~ data_tcl$yearsofexperience)
summary(mod_tcl)

ols_plot_resid_lev(mod_tcl)
plot(mod_tcl, which=1)

qqnorm(mod_tcl$residuals)
qqline(mod_tcl$residuals, col = "red")
shapiro.test(mod_tcl$residuals)

box = boxcox(data_tcl$totalyearlycompensation ~ data_tcl$yearsofexperience)
lambda = box$x[which.max(box$y)]
lambda
lambdascelto=readline(prompt="inserisci lambda: ")
# Amazon: lambda = -1
# Apple: lambda = 0
# Microsoft: lambda = -1
# Facebook: lambda = 0
# Google: lambda = 0

if (lambdascelto==0){
  mod_tclbox = lm(log(data_tcl$totalyearlycompensation) ~ data_tcl$yearsofexperience)
}else {
  mod_tclbox = lm( (data_tcl$totalyearlycompensation^lambda - 1)/lambda ~ data_tcl$yearsofexperience)
}
summary(mod_tclbox)

qqnorm(mod_tclbox$residuals)
qqline(mod_tclbox$residuals, col = "red")
shapiro.test(mod_tclbox$residuals)

h1 = hist(mod_tclbox$residuals, prob = T, breaks = 15)
brks1=h1$breaks

ggplot(mod_tclbox, aes(x=mod_tclbox$residuals)) +
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue", stat="bin", breaks = brks1, right = TRUE) +
  geom_density(alpha=.2, fill="#FF6666") +
  theme(axis.title.y = element_text(size=10, face="plain"), axis.title=element_text(size=10, face="plain")) + 
  labs(x = "Residuals", y = "Density")

#-------------------------------------------------------------------------------
# Distanza di Cook
# ------------------------------------------------------------------------------

cd = cooks.distance(mod_tc)

watchout_data_tc_cook = which(cd > 4/(n-p))
plot(data_tc$yearsofexperience, data_tc$totalyearlycompensation, cex = 0.5, pch = 16,
     xlab = "Years Of Experience", ylab = "Total Yearly Compensation")
points(data_tc$yearsofexperience[watchout_data_tc_cook], data_tc$totalyearlycompensation[watchout_data_tc_cook], col = 'green', 
       cex = 0.5, pch = 16, xlab = "Years of Experience", ylab = "Total Yearly Compensation")

# Rimozione di punti con distanza di Cook elevata
data_tcld = subset(data_tc, cd<=4/(n-p))
mod_tcld = lm(data_tcld$totalyearlycompensation ~ data_tcld$yearsofexperience)
summary(mod_tcld)

ols_plot_resid_lev(mod_tcld)
plot(mod_tcld, which=1)

qqnorm(mod_tcld$residuals)
qqline(mod_tcld$residuals, col = "red")
shapiro.test(mod_tcld$residuals)

box = boxcox(data_tcld$totalyearlycompensation ~ data_tcld$yearsofexperience)
lambda = box$x[which.max(box$y)]
lambda
lambdascelto=readline(prompt="inserisci lambda: ")
# Amazon: lambda = -0.5
# Apple: lambda = 0
# Microsoft: lambda = -1
# Facebook: lambda = 0
# Google: lambda = 0

if (lambdascelto==0){
  mod_tcldbox = lm(log(data_tcld$totalyearlycompensation) ~ data_tcld$yearsofexperience)
}else {
  mod_tcldbox = lm( (data_tcld$totalyearlycompensation^lambda - 1)/lambda ~ data_tcld$yearsofexperience)
}
summary(mod_tcldbox)

qqnorm(mod_tcldbox$residuals)
qqline(mod_tcldbox$residuals, col = "red")
shapiro.test(mod_tcldbox$residuals)

h2 = hist(mod_tcldbox$residuals, prob = T, breaks = 15)
brks2=h2$breaks

ggplot(mod_tcldbox, aes(x=mod_tcldbox$residuals)) +
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue", stat="bin", breaks = brks2, right = TRUE) +
  geom_density(alpha=.2, fill="#FF6666") +
  theme(axis.title.y = element_text(size=10, face="plain"), axis.title=element_text(size=10, face="plain")) + 
  labs(x = "Residuals", y = "Density")

plot(data_tcld$yearsofexperience, data_tcld$totalyearlycompensation, 
     xlab = "Years Of Experience", ylab = "Total Yearly Compensation")
abline(a = mod_tcld$coefficients[1], b = mod_tcld$coefficients[2])

mod_tcld$coefficients[1]
mod_tcld$coefficients[2]

# ------------------------------------------------------------------------------
# REGRESSIONE LINEARE - CONFRONTO TRA AZIENDE
# ------------------------------------------------------------------------------

# Coefficienti del modello senza punti Cook, senza Box-Cox
companies = c("Apple", "Microsoft", "Facebook", "Google", "Amazon") 
intercept = c(198908.4, 164391.5, 221061, 202939, 166171.6) # SOLO NO COOK
slope = c(12587.1, 5401.6, 20339, 13800, 11164.2) # SOLO NO COOK
color = c("red", "blue", "green", "orange", "purple")

for(i in 1:5) {
  plot(data$yearsofexperience, data$totalyearlycompensation, cex = 0.5, pch = 16,
       xlab = "Years of Experience", ylab = "Total Yearly Compensation",main=companies[i])
  points(data$yearsofexperience[which(data$company == companies[i])], data$totalyearlycompensation[which(data$company == companies[i])], col = color[i], 
         cex = 0.5, pch = 16, xlab = "Years of Experience", ylab = "Total Yearly Compensation")
  abline(a = intercept[i], b = slope[i], col = color[i])
}

plot(data$yearsofexperience, data$totalyearlycompensation, cex = 0.5, pch = 16,
     xlab = "Years of Experience", ylab = "Total Yearly Compensation",main="confronto aziende")
for(i in 1:5) {
  abline(a = intercept[i], b = slope[i], col = color[i])
}
legend(x = "topright", legend = companies, fill = color)

# ------------------------------------------------------------------------------
# REGRESSIONE LINEARE - GENERE
# ------------------------------------------------------------------------------

t = "Software Engineer"
c = "Apple"

data_tc = data_t[which(data_t$company == c), ]

mod_tc = lm(data_tc$totalyearlycompensation ~ data_tc$yearsofexperience * data_tc$gender)
summary(mod_tc)
plot(mod_tc, which=1)

contrasts(data_tc$gender)

qqnorm(mod_tc$residuals)
qqline(mod_tc$residuals, col = "red")
shapiro.test(mod_tc$residuals)

box = boxcox(data_tc$totalyearlycompensation ~ data_tc$yearsofexperience * data_tc$gender)
lambda = box$x[which.max(box$y)]
lambda
lambdascelto=readline(prompt="inserisci lambda: ")
# Amazon: lambda = -1
# Apple: lambda = -1
# Microsoft: lambda = -1
# Facebook: lambda = 0
# Google: lambda = 0

if (lambdascelto==0){
  mod_tcbox = lm(log(data_tc$totalyearlycompensation) ~ data_tc$yearsofexperience * data_tc$gender)
}else {
  mod_tcbox = lm( (data_tc$totalyearlycompensation^lambda - 1)/lambda ~ data_tc$yearsofexperience * data_tc$gender)
}
summary(mod_tcbox)

qqnorm(mod_tcbox$residuals)
qqline(mod_tcbox$residuals, col = "red")
shapiro.test(mod_tcbox$residuals) 

plot(data_tc$yearsofexperience, data_tc$totalyearlycompensation,pch=16,cex=0.5, 
     xlab = "Years Of Experience", ylab = "Total Yearly Compensation")
points(data_tc$yearsofexperience[which (data_tc$gender=='Male')], data_tc$totalyearlycompensation[which (data_tc$gender=='Male')], col = 'Royal Blue',cex=0.5, pch = 16)
points(data_tc$yearsofexperience[which (data_tc$gender=='Female')], data_tc$totalyearlycompensation[which (data_tc$gender=='Female')], col = 'Hot Pink',cex=0.5, pch = 16)

abline(a = mod_tc$coefficients[1], b = mod_tc$coefficients[2], col='Hot Pink')
abline(a = mod_tc$coefficients[1]+mod_tc$coefficients[3], b = mod_tc$coefficients[4]+mod_tc$coefficients[2], col='Royal Blue')
