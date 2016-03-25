library(RSNNS)
library(ROCR)

# Читаем данные из csv
my_df <- read.csv("normal_NYK_and_Healthy_new_data for viewing.csv", sep = ";")

# Создаем объект нейросети радиально-базисных функций
# в качестве входов используем значения предикторов
# в качестве выхода - предсказуемое значение
nnet <- rbf(as.matrix(my_df[3:12]), as.matrix(my_df$class))

# Надо понимать, что на выходе сети в данном примере
# вероятность того, что объект принадлежит классу единицы,
# поэтому с этим значением работаем точно так же, как и в случае регрессии
prob <- predict(nnet, as.matrix(my_df[3:12]))
pred_fit <- prediction(prob, my_df$class)
perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
abline(v = 0.53, lwd = 2)
pred_resp  <- factor(ifelse(prob > 0.53, 1, 0))
correct  <- ifelse(pred_resp == my_df$class, 1, 0)

# Среднее число правильных ответов сети: 64%
mean(correct)
