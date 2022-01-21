
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










