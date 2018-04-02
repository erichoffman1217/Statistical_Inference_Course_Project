## Part 2 of the Statistical Inference Course Project

library(ggplot2)
data(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)
table(ToothGrowth$supp,ToothGrowth$dose)
levels(ToothGrowth$supp) <- c("Orange Juice", "Ascorbic Acid")
ggplot(ToothGrowth, aes(x=factor(dose), y=len)) + 
        facet_grid(.~supp) +
        geom_boxplot(aes(fill = supp), show_guide = FALSE) +
        labs(title="Guinea pig tooth length by dosage for each type of supplement", 
             x="Dose (miligrams / day)",
             y="Tooth Length")

hypothall<-t.test(len ~ supp, data = ToothGrowth)
hypothall$conf.int
hypothall$p.value

hypoth1<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 0.5))
hypoth1$conf.int
hypoth1$p.value

hypoth2<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 1))
hypoth2$conf.int
hypoth2$p.value

hypoth3<-t.test(len ~ supp, data = subset(ToothGrowth, dose == 2))
hypoth3$conf.int
hypoth3$p.value
x <- c('all', '0.5', '1.0', '2.0')
y <- c(hypothall$p.value,hypoth1$p.value,hypoth2$p.value,hypoth3$p.value)
pvalue_table<- as.data.frame(cbind(x,y))
colnames(pvalue_table) <- c('Dose', 'p-value')

