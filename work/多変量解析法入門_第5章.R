
# 例題1 ---------------------------------------------------------------------

x1 = c(51, 38, 57, 51, 53, 77, 63, 69, 72, 73)
x2 = c(16, 4, 16, 11, 4, 22, 5, 5, 2, 1)
y = c(3.0, 3.2, 3.3, 3.9, 4.4, 4.5, 4.5, 5.4, 5.4, 6.0)
df = data.frame(x1, x2, y)

graphics::pairs(df)
?pairs

library(psych)
psych::pairs.panels(df)
psych::pairs.panels(df, hist.col="white", rug=FALSE, ellipses=FALSE, lm=TRUE)
?pairs.panels

library(corrplot)
corrplot::corrplot(cor(df[,-5]))
corrplot::corrplot(round(cor(df), 2), method="color", addCoef.col=TRUE) 

library(tidyverse)
library(GGally)
ggpairs(df)


# 偏回帰係数の推定
result = lm(y~x1+x2)
summary(result)

S11 = sum((x1-mean(x1))^2)
S12 = sum((x1-mean(x1))*(x2-mean(x2)))
S22 = sum((x2-mean(x2))^2)
S1y = sum((x1-mean(x1))*(y-mean(y)))
S2y = sum((x2-mean(x2))*(y-mean(y)))

S = matrix(c(S11, S12, S12, S22), nrow=2, ncol=2)
det(S)
S.inv = solve(S)

beta12 = S.inv %*% c(S1y, S2y)
beta0 = mean(y) - beta12[1,]*mean(x1) - beta12[2,]*mean(x2)

# よって回帰式は　y = 1.0201 + 0.0668*x1 - 0.0808*x2 


# 例題2 ---------------------------------------------------------------------

# 重相関係数、決定係数の算出
y.p = beta0 + beta12[1,]*x1 + beta12[2,]*x2
plot(y, y.p)

R = sum((y-mean(y))*(y.p-mean(y.p)))/sqrt(sum((y-mean(y))^2)*sum((y.p-mean(y.p))^2))
R2 = R^2

Se = sum((y-y.p)^2)
Syy = sum((y-mean(y))^2)
R2.adj = 1 - (Se/(length(y)-3)) / (Syy/(length(y)-1))
  

# 例題3 ---------------------------------------------------------------------

Se_M1_1 = Syy - S1y^2/S11  
F0_1 = ((Syy - Se_M1_1)/((length(y)-1)-(length(y)-2)))/(Se_M1_1/(length(y)-2))

Se_M1_2 = Syy - S2y^2/S22  
F0_2 = ((Syy - Se_M1_2)/((length(y)-1)-(length(y)-2)))/(Se_M1_2/(length(y)-2))

# F0_1 > F0_2 より、x1を採用

Se_M2 = Se
F0 = ((Se_M1 - Se_M2)/((length(y)-2)-(length(y)-3)))/(Se_M2/(length(y)-3))

# F0>2より、x2も採用


# 例題4 ---------------------------------------------------------------------

# 予測値
y.p

# 標準化残差
e = y - y.p
e.s = e / sqrt(Se/(length(y)-3))

# マハラノビス距離
D2 = (length(y)-1)*(((x1-mean(x1))^2)*S.inv[1, 1] + 2*(x1-mean(x1))*(x2-mean(x2))*S.inv[1, 2] + ((x2-mean(x2))^2)*S.inv[2, 2])

# テコ比
h = (1/length(y)) + (D2/(length(y)-1))

# テコ比の描画
graphics::par(new=TRUE)
graphics::plot(e.s, h, xlim=c(-2, 2), ylim=c(0, 0.8))
graphics::abline(h=2.5*mean(h), col="blue")

# 標準化残差の散布図
graphics::par(mfrow=c(1,2))
graphics::plot(x1, e.s)
graphics::abline(h=0, col="blue")
graphics::plot(x2, e.s)
graphics::abline(h=0, col="blue")

dev.off()


# 例題5 ---------------------------------------------------------------------

# [x01, x02] = [70, 10]のときの母回帰の信頼率95%の信頼区間と予測区間を算出

# 予測値
y0.p = beta0 + beta12[1,]*70 + beta12[2,]*10

# マハラノビス距離
D02 = (length(y)-1)*(((70-mean(x1))^2)*S.inv[1, 1] + 2*(70-mean(x1))*(10-mean(x2))*S.inv[1, 2] + ((10-mean(x2))^2)*S.inv[2, 2])

# 母回帰β0＋β1*xの95%信頼区間
y.p1_lower = y0.p - stats::qt(0.025, df=length(y)-3, lower.tail=FALSE) * sqrt(((1/length(y))+(D02/(length(y)-1)))*(Se/(length(y)-3)))
y.p1_upper = y0.p + stats::qt(0.025, df=length(y)-3, lower.tail=FALSE) * sqrt(((1/length(y))+(D02/(length(y)-1)))*(Se/(length(y)-3)))

# 母回帰β0＋β1*x+εの95%予測区間
y.p2_lower = y0.p - stats::qt(0.025, df=length(y)-3, lower.tail=FALSE) * sqrt((1+(1/length(y))+(D02/(length(y)-1)))*(Se/(length(y)-3)))
y.p2_upper = y0.p + stats::qt(0.025, df=length(y)-3, lower.tail=FALSE) * sqrt((1+(1/length(y))+(D02/(length(y)-1)))*(Se/(length(y)-3)))



# 問題5.1 -------------------------------------------------------------------

# 問題5.1(1)
x1 = c(12, 12, 11, 7, 8, 9, 14, 11)
x2 = c(4, 3, 3, 1, 3, 2, 5, 4)
y = c(22, 24, 21, 19, 19, 22, 24, 23)
df = data.frame(x1, x2, y)

library(psych)
psych::pairs.panels(df, hist.col="white", rug=FALSE, ellipses=FALSE, lm=TRUE)

library(corrplot)
corrplot::corrplot(cor(df[,-5]))
corrplot::corrplot(round(cor(df), 2), method="color", addCoef.col=TRUE) 

# 相関係数
cor(df)

# 問題5.1(2)
# 回帰式の推定
result = lm(y~x1+x2)
summary(result)

S11 = sum((x1-mean(x1))^2)
S12 = sum((x1-mean(x1))*(x2-mean(x2)))
S22 = sum((x2-mean(x2))^2)
S1y = sum((x1-mean(x1))*(y-mean(y)))
S2y = sum((x2-mean(x2))*(y-mean(y)))

S = matrix(c(S11, S12, S12, S22), nrow=2, ncol=2)
S
det(S)
S.inv = solve(S)
S.inv

beta = S.inv %*% c(S1y, S2y)
beta

beta0 = mean(y) - beta[1] * mean(x1) - beta[2] * mean(x2)
beta0

# よって回帰式は　y = 13.01 + 1.006 * x1 - 0.5841 * x2 と推定される

# 寄与率と自由度調整済寄与率
Syy = sum((y - mean(y))^2)
Syy
Se = sum((y - (beta0 + beta[1] * x1 + beta[2] * x2)) ^ 2)
Se

R2 = 1 - Se/Syy
R2
R2.adj = 1 - (Se/(length(y)-2-1))/(Syy/(length(y)-1))
R2.adj


# 問題5.1(3)
# x1のみを取り込んだ場合
result1 = lm(y ~ x1)
summary(result1)

result1$coefficients[1]
result1$coefficients[2]
# 回帰式は y = 14.01 + 0.7368 * x1

Se.M1.1 = sum((y - (result1$coefficients[1] + result1$coefficients[2] * x1)) ^ 2)
F0.M1.1 = ((Syy-Se.M1.1)/((length(y)-1)-(length(y)-1-1)))/(Se.M1.1/(length(y)-1-1))
# F0（分散比）は 18.02

# 寄与率と自由度調整済寄与率
R2.M1.1 = 1 - Se.M1.1/Syy
R2.M1.1.adj = 1 - (Se.M1.1/(length(y)-1-1))/(Syy/(length(y)-1))


# 問題5.1(4)
# x2のみを取り込んだ場合
result2 = lm(y ~ x2)
summary(result2)

result2$coefficients[1]
result2$coefficients[2]
# 回帰式は y = 18.51 + 1.034 * x2

Se.M1.2 = sum((y - (result2$coefficients[1] + result2$coefficients[2] * x2)) ^ 2)
F0.M1.2 = ((Syy-Se.M1.2)/((length(y)-1)-(length(y)-1-1)))/(Se.M1.2/(length(y)-1-1))
# F0（分散比）は 4.402

# 寄与率と自由度調整済寄与率
R2.M1.2 = 1 - Se.M1.2/Syy
R2.M1.2.adj = 1 - (Se.M1.2/(length(y)-1-1))/(Syy/(length(y)-1))


# 問題5.1(5)
# x1のみを取り込んだモデルにx2を追加する場合

Se.M2 = Se
F0.M2 = ((Se.M1.1-Se.M2)/((length(y)-1-1)-(length(y)-2-1)))/(Se.M2/(length(y)-2-1))
# F0は 0.813

F0 = ((Syy-Se.M2)/((length(y)-1)-(length(y)-2-1)))/(Se.M2/(length(y)-2-1))

# 描画
graphics::par(new=TRUE)
graphics::curve(df(x, df1=(length(y)-1)-(length(y)-2-1), df2=length(y)-2-1), from=0, to=10)
graphics::abline(v=F0, col="blue")
graphics::abline(v=stats::qf(0.05, df1=(length(y)-1)-(length(y)-2-1), df2=length(y)-2-1, lower.tail=FALSE), col="red")


