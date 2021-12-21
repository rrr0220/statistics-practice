
# 問題2.2(1) → 母分散未知の為、誤答 ---------------------------------------------------------------------

data = c(3, 4, 2, 9, 6, 7, 5, 6, 5, 4)

# 平均の算出
data_ave = mean(data)

# 分散の算出
data_var = sum((data-data_ave)^2)/length(data)

# 不偏分散の算出
data_var.s = stats::var(data)

# H0：μ=3 のとき、z値の算出
z = (data_ave-3)/sqrt(data_var/length(data))

# 指定したｚ値の上側確率P(z>z値)
stats::pnorm(z, lower.tail=F)

# 指定した上側確率に対応するｚ値（境界値）
stats::qnorm(0.025, lower.tail=F)

# 描画
graphics::par(new=TRUE)
graphics::curve(dnorm(x, mean=0, sd=1), from=-5, to=5)
graphics::abline(v=z, col="blue")
graphics::abline(v=stats::qnorm(0.025, lower.tail=FALSE), col="red")
graphics::abline(v=stats::qnorm(0.025), col="red")

# よって、μ=3.0は5％水準で有意　→　H0は棄却される

x_ave_lower = data_ave - stats::qnorm(0.025, lower.tail=FALSE) * sqrt(data_var/length(data))
x_ave_upper = data_ave + stats::qnorm(0.025, lower.tail=FALSE) * sqrt(data_var/length(data))

# よって母平均μの95％信頼区間は　3.909 <= μ <= 6.291  

# 描画
graphics::par(new=TRUE)
graphics::curve(dnorm(x, mean=data_ave, sd=sqrt(data_var)), from=-5, to=15)
graphics::abline(v=x_ave_lower, col="red")
graphics::abline(v=x_ave_upper, col="red")


# 問題2.2(1) ----------------------------------------------------------------

# H0：μ=3 のとき、t値の算出
t = (data_ave-3)/sqrt(data_var.s/length(data))

# 指定したt値の上側確率P(t>t値)
stats::pt(t, df=length(data)-1, lower.tail=FALSE)

# 指定した上側確率に対応するt値（境界値）
stats::qt(0.025, df=length(data)-1, lower.tail=FALSE)

# 描画
graphics::par(new=TRUE)
graphics::curve(dt(x, df=length(data)-1), from=-5, to=5)
graphics::abline(v=t, col="blue")
graphics::abline(v=stats::qt(0.025, df=length(data)-1, lower.tail=FALSE), col="red")
graphics::abline(v=stats::qt(0.025, df=length(data)-1), col="red")

# よって、μ=3.0は5％水準で有意　→　H0は棄却される

mu_lower = data_ave - stats::qt(0.025, df=length(data)-1, lower.tail=FALSE) * sqrt(data_var.s/length(data))
mu_upper = data_ave + stats::qt(0.025, df=length(data)-1, lower.tail=FALSE) * sqrt(data_var.s/length(data))

# よって母平均μの95％信頼区間は　3.652 <= μ <= 6.548


# 問題2.2(2) ----------------------------------------------------------------

# H0：σ^2=2.0^2 のとき、χ^2値の算出
X2 = sum((data-data_ave)^2)/(2)^2

# 指定したχ^2値の上側確率P(χ^2>χ^2値)
stats::pchisq(X2, df=length(data)-1, lower.tail=FALSE)

# 指定した下側・上側確率に対応するχ^2値（境界値）
stats::qchisq(0.025, df=length(data)-1)
stats::qchisq(0.025, df=length(data)-1, lower.tail=FALSE)

# 描画
graphics::par(new=TRUE)
graphics::curve(dchisq(x, df=length(data)-1), from=0, to=30)
graphics::abline(v=X2, col="blue")
graphics::abline(v=stats::qchisq(0.025, df=length(data)-1), col="red")
graphics::abline(v=stats::qchisq(0.025, df=length(data)-1, lower.tail=FALSE), col="red")

# よって、σ^2=2.0^2は5％水準で有意でない　→　H0は棄却されない

sigmasq_upper = sum((data-data_ave)^2)/stats::qchisq(0.025, df=length(data)-1)
sigmasq_lower = sum((data-data_ave)^2)/stats::qchisq(0.025, df=length(data)-1, lower.tail=FALSE)

# よって母分散σ^2の95％信頼区間は　1.94 <= σ^2 <= 13.66



# 問題2.3 -------------------------------------------------------------------

x1 = c(5, 6, 4, 8, 7, 3, 6, 4, 5)
x2 = c(8, 9, 6, 10, 12, 8, 7, 9)

x1_ave = mean(x1)
x2_ave = mean(x2)

x1_sd = sqrt(sum((x1-x1_ave)^2)/length(x1))
x2_sd = sqrt(sum((x2-x2_ave)^2)/length(x2))

# 平均差の標本分布の平均
x12_ave = x1_ave - x2_ave

# 母分散が等しい二つの標本の平均差の母集団標準偏差σ(1-2)の推定量
sigma12 = sqrt((length(x1)*((x1_sd)^2)+length(x2)*((x2_sd)^2))/(length(x1)+length(x2)-2))

# 平均差の標本分布の標準偏差
s12 = sigma12 * sqrt((1/length(x1))+(1/length(x2)))

# H0:μ1=μ2のとき、t0値の算出
t0 = x12_ave / s12

# 指定したt値の上側確率P(t>t値)
stats::pt(t0, df=length(x1)+length(x2)-2, lower.tail=FALSE)

# 指定した上側確率に対応するt値（境界値）
stats::qt(0.025, df=length(x1)+length(x2)-2)
stats::qt(0.025, df=length(x1)+length(x2)-2, lower.tail=FALSE)

# 描画
graphics::par(new=TRUE)
graphics::curve(dt(x, df=length(x1)+length(x2)-2), from=-5, to=5)
graphics::abline(v=t0, col="blue")
graphics::abline(v=stats::qt(0.025, df=length(x1)+length(x2)-2, lower.tail=FALSE), col="red")
graphics::abline(v=stats::qt(0.025, df=length(x1)+length(x2)-2), col="red")

# よって、|t0|=3.961 > |t(15, 0.025)|=2.131 なので、5％水準で有意である　→　H0は棄却される、二つの母平均は異なる

mu12_lower = x12_ave - stats::qt(0.025, df=length(x1)+length(x2)-2, lower.tail=FALSE) * s12
mu12_upper = x12_ave + stats::qt(0.025, df=length(x1)+length(x2)-2, lower.tail=FALSE) * s12

# よって母平均差μ1-μ2の95％信頼区間は　-5.063 <= μ1-μ2 <= -1.520
