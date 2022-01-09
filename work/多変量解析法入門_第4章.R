
# 例題1 ---------------------------------------------------------------------

x = c(2.2, 4.1, 5.5, 1.9, 3.4, 2.6, 4.2, 3.7, 4.9, 3.2)
y = c(71, 81, 86, 72, 77, 73, 80, 81, 85, 74)

plot(y~x)                         # 散布図を描く
result = lm(y~x)                  # 回帰分析を行う
abline(result)                    # 推定回帰直線を描く
summary(result)

x_mean = mean(x)
y_mean = mean(y)
Sxy = sum((x - x_mean) * (y - y_mean))
Sxx = sum((x - x_mean)^2)
Syy = sum((y - y_mean)^2)

# 回帰係数の予測
beta1.p = Sxy / Sxx
beta0.p = y_mean - beta1.p * x_mean

# よって、回帰式は y=61.9+4.52x

# 寄与率の算出
SR = beta1.p * Sxy
R2 = SR / Syy

Se = Syy - SR
R2 = 1 - Se / Syy
R2.adj = 1 - (Se/(length(x)-2)) / (Syy/(length(x)-1))


# 例題2 ---------------------------------------------------------------------

# β1≠0かどうかの検定
# H0:β1=0とする

# beta1.pはN(β1, σ^2/Sxx)に従う
# 母分散σは未知なので、誤差の母分散σの代わりに不偏分散Se/φe=n-2を用いると、検定統計量t0はt(φe=n-2)に従う

t0 = beta1.p / sqrt((Se/(length(x)-2))/Sxx)

# 描画
graphics::par(new=TRUE)
graphics::curve(dt(x, df=length(x)-2), from=-11, to=11)
graphics::abline(v=t0, col="blue")
graphics::abline(v=stats::qt(0.025, df=length(x)-2, lower.tail=FALSE), col="red")
graphics::abline(v=stats::qt(0.025, df=length(x)-2), col="red")

# よって、t0=10.6は5％水準で有意　→　H0は棄却される

beta1_lower = beta1.p - stats::qt(0.025, df=length(x)-2, lower.tail=FALSE) * sqrt((Se/(length(x)-2))/Sxx)
beta1_upper = beta1.p + stats::qt(0.025, df=length(x)-2, lower.tail=FALSE) * sqrt((Se/(length(x)-2))/Sxx)

# よってβ1の95％信頼区間は　3.54 <= β1 <= 5.50


# 例題3 ---------------------------------------------------------------------

# 予測値
y.p = beta0.p + beta1.p * x

# 標準化残差
e.s = (y - y.p) / sqrt(Se/(length(x)-2))

# テコ比
h = (1/length(x)) + ((x - x_mean)^2/Sxx)

# テコ比の描画
graphics::par(new=TRUE)
graphics::plot(e.s, h, xlim=c(-2, 2), ylim=c(0, 0.6))
graphics::abline(h=2.5*mean(h), col="blue")

# 標準化残差の散布図
graphics::par(new=TRUE)
graphics::plot(x, e.s)


# 例題4 ---------------------------------------------------------------------

# x0=5のとき、母回帰β0＋β1*xの信頼区間と予測区間を算出する
y0.p = beta0.p + beta1.p * 5

# 母回帰β0＋β1*xの95%信頼区間
y_lower = y0.p - stats::qt(0.025, df=length(x)-2, lower.tail=FALSE) * sqrt(((1/length(x))+((5-x_mean)^2/Sxx))*(Se/(length(x)-2)))
y_upper = y0.p + stats::qt(0.025, df=length(x)-2, lower.tail=FALSE) * sqrt(((1/length(x))+((5-x_mean)^2/Sxx))*(Se/(length(x)-2)))

# 誤差を含めた母回帰β0＋β1*x+εの95%予測区間
y.p_lower = y0.p - stats::qt(0.025, df=length(x)-2, lower.tail=FALSE) * sqrt((1+(1/length(x))+((5-x_mean)^2/Sxx))*(Se/(length(x)-2)))
y.p_upper = y0.p + stats::qt(0.025, df=length(x)-2, lower.tail=FALSE) * sqrt((1+(1/length(x))+((5-x_mean)^2/Sxx))*(Se/(length(x)-2)))

