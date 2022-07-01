library(readr)
britishSeatBeltStudy <- read_csv("britishSeatBeltStudy.csv", 
                                 col_types = cols(law = col_factor(levels = c("0", 
                                                                              "1")), Date = col_date(format = "%Y-%m-%d")))
View(britishSeatBeltStudy)

britishSeatBeltStudy$FractionOfDriversKilled = britishSeatBeltStudy$DriversKilled /britishSeatBeltStudy$drivers

summary(britishSeatBeltStudy$FractionOfDriversKilled)

boxplot(FractionOfDriversKilled ~ law, data = britishSeatBeltStudy)
boxplot(DriversKilled ~ law, data = britishSeatBeltStudy)

t.test(britishSeatBeltStudy$DriversKilled [which(britishSeatBeltStudy$law %in% c(1))], 
       britishSeatBeltStudy$DriversKilled [which(britishSeatBeltStudy$law %in% c(0))], 
       var.equal = F, paired = F
)

t.test(britishSeatBeltStudy$FractionOfDriversKilled [which(britishSeatBeltStudy$law %in% c(1))], 
       britishSeatBeltStudy$FractionOfDriversKilled [which(britishSeatBeltStudy$law %in% c(0))], 
       var.equal = F, paired = F
)


# Linear Analysis:  Logistic Regression to predict number of drivers that occur with
#law being in effect

fit2 <- lm (formula = DriversKilled ~ law + kms + PetrolPrice ,
           data = britishSeatBeltStudy, family = 'binomial')
summary(fit2)