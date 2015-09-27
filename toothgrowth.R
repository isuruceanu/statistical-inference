library(datasets)
library(ggplot2)

data("ToothGrowth")

str(ToothGrowth)

table(ToothGrowth$dose, ToothGrowth$supp)

ggplot(data = ToothGrowth, aes(x = as.factor(dose), y = len, fill = supp)) +
    geom_bar(stat = "identity") +
    facet_grid(. ~ supp) +
    xlab("Dose in miligrames") +
    ylab("Tooth length") +
    guides(fill = guide_legend(title = "Suppliment type"))

r <- ToothGrowth %>% mutate(id = rep(1:10, times = 6))
r <- dcast(r, id ~ dose + supp, value.var = "len")

rbind(t.test(r$`0.5_OJ`, r$`1_OJ`)$conf.int,
      t.test(r$`1_OJ`, r$`2_OJ`)$conf.int,
      t.test(r$`0.5_OJ`, r$`2_OJ`)$conf.int
) 


rbind(t.test(r$`0.5_VC`, r$`1_VC`)$conf.int,
      t.test(r$`1_VC`, r$`2_VC`)$conf.int,
      t.test(r$`0.5_VC`, r$`2_VC`)$conf.int
) 

rbind(t.test(r$`0.5_OJ`, r$`0.5_VC`)$conf.int,
      t.test(r$`1_OJ`, r$`1_VC`)$conf.int,
      t.test(r$`2_OJ`, r$`2_VC`)$conf.int
) 