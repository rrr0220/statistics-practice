

# 第8章 ---------------------------------------------------------------------

# １．クロス表の作成
tbl2.1 = matrix(c(395, 2456, 1758, 4609, 749, 66,
                  147, 153, 916, 1216, 235, 135,
                  694, 327, 1347, 2368, 283, 185,
                  1236, 2936, 4021, 8193, 1131, 297,
                  4558, 5129, 10842, 20529, NA, NA
                  ), byrow = TRUE, ncol = 6, nrow = 5)

dimnames(tbl2.1) = list(地域 = c("オスロ", "中部地域", "北部地域", "合計", "全国"),
                          犯罪 = c("強盗", "詐欺", "破壊", "合計", "密", "疎"))

tbl2.1

tbl2.1a = tbl2.1[1:3, 1:3]
tbl2.1a

# 総度数の計算
n = sum(tbl2.1a)
n

# 周辺度数の計算
row.mar = apply(tbl2.1a, 1, sum)  # 行周辺度数
col.mar = apply(tbl2.1a, 2, sum)  # 列周辺度数

tbl2.1a.mar = rbind(cbind(tbl2.1a, row.mar), c(col.mar, n))
rownames(tbl2.1a.mar)[4] <- "col.mar"
tbl2.1a.mar

# 行プロファイルの計算
tbl.r = tbl2.1a / row.mar
平均行prof = col.mar / n  # 平均行プロファイル（行重心＝列質量）
r.mass = row.mar / n # 平均列プロファイル（列重心＝行質量）

tbl2.1a.r = cbind(rbind(tbl.r, 平均行prof), c(r.mass, 1))
colnames(tbl2.1a.r)[4] <- "r.mass"
tbl2.1a.r

# 列プロファイルの計算
tbl.c = t(tbl2.1a) / col.mar
平均列prof = row.mar / n  # 平均列プロファイル（列重心＝行質量）
c.mass = col.mar / n # 平均行プロファイル（行重心＝列質量）

tbl2.1a.c = cbind(rbind(tbl.c, 平均列prof), c(c.mass, 1))
colnames(tbl2.1a.c)[4] <- "c.mass"
tbl2.1a.c

# 三角座標
table.pro = tbl2.1a.r
row.names(table.pro)[4] = "重心"
table.pro
table.x = 1 - table.pro[, 1] - table.pro[, 3]/2
table.y = table.pro[, 3] * sqrt(3)/2

plot.new()
lines(c(0, 1, 0.5, 0), c(0, 0, sqrt(3)/2, 0), col = "gray")
text(c(0, 1, 0.5), c(0, 0, sqrt(3)/2), labels=colnames(table.pro[, 1:3]))
text(table.x, table.y*1.1, labels=rownames(table.pro[1:4,]))
points(table.x, table.y, pch=20)

lines(table.x[c(1, 4)], table.y[c(1, 4)], lty=2)
lines(table.x[c(2, 4)], table.y[c(2, 4)], lty=2)
lines(table.x[c(3, 4)], table.y[c(3, 4)], lty=2)
lines(table.x[c(1, 2)], table.y[c(1, 2)], lty=3)
lines(table.x[c(2, 3)], table.y[c(2, 3)], lty=3)
lines(table.x[c(3, 1)], table.y[c(3, 1)], lty=3)

# 行プロファイルデータからχ2距離を計算
tbl2.1a.r
.tbl = tbl2.1a.r[, 1:3]

# 行重心との距離
dist.r = sqrt(apply((t(.tbl[1:3, ])-.tbl[4, ])^2/.tbl[4, ], 2, sum))
dist.r

# 各行プロファイル間の距離
sqrt(sum((.tbl[1, ] - .tbl[2,])^2 / .tbl[4, ]))  # オスロ‐中部
sqrt(sum((.tbl[2, ] - .tbl[3,])^2 / .tbl[4, ]))  # 中部-北部
sqrt(sum((.tbl[3, ] - .tbl[1,])^2 / .tbl[4, ]))  # 北部-オスロ

# 列プロファイルデータからχ2距離を計算
tbl2.1a.c
.tbl = tbl2.1a.c[, 1:3]

# 列重心との距離
dist.c = sqrt(apply((t(.tbl[1:3, ])-.tbl[4, ])^2/.tbl[4, ], 2, sum))
dist.c

# 各列プロファイル間の距離
sqrt(sum((.tbl[1, ] - .tbl[2,])^2 / .tbl[4, ]))  # 強盗‐詐欺
sqrt(sum((.tbl[2, ] - .tbl[3,])^2 / .tbl[4, ]))  # 詐欺-破壊
sqrt(sum((.tbl[3, ] - .tbl[1,])^2 / .tbl[4, ]))  # 破壊-強盗

# 対応分析の実行
library(FactoMineR)         
res.ca = CA(tbl2.1a)
summary(res.ca)
str(res.ca)
res.ca
res.ca$eig

# スクリープロット
library(qcc)
pareto.chart(res.ca$eig[, 1], name=c("Dim1", "Dim2"),
             main="Screeplot", las=1, xlab="座標軸",
             ylab="固有値", ylab2="累積%", col="lightblue")

# χ2距離の検算
sqrt(res.ca$row$coord[, 1]^2 + res.ca$row$coord[, 2]^2)

# 座標軸への行プロファイル・ポイントの寄与（絶対的寄与）
res.ca$row$contrib

library(lattice)
library(reshape2)

data = res.ca$row$contrib[nrow(res.ca$row$contrib):1, 1:2]  # 行を逆転
dd.t = melt(data)
barchart(Var1~value|Var2, data=dd.t,
         main="各軸に対する行ポイントの寄与",
         xlab="寄与率")

# 座標軸への列プロファイル・ポイントの寄与（絶対的寄与）
res.ca$col$contrib

data = res.ca$col$contrib[nrow(res.ca$col$contrib):1, 1:2]  # 行を逆転
dd.t = melt(data)
barchart(Var1~value|Var2, data=dd.t,
         main="各軸に対する列ポイントの寄与",
         xlab="寄与率")

rm(data, dd.t)

# 行プロファイル・ポイントへの各軸の寄与（平方相関）
res.ca$row$cos2

data = res.ca$row$cos2[nrow(res.ca$row$cos2):1, 2:1]
dd.t = melt(data)
barchart(Var2~value|Var1, data=dd.t,
         main="行ポイントへの各軸の寄与",
         xlab="寄与率")

# 列プロファイル・ポイントへの各軸の寄与（平方相関）
res.ca$col$cos2

data = res.ca$col$cos2[nrow(res.ca$col$cos2):1, 2:1]
dd.t = melt(data)
barchart(Var2~value|Var1, data=dd.t,
         main="列ポイントへの各軸の寄与",
         xlab="寄与率")

rm(data, dd.t)

# 累積寄与率（質）の確認
data = apply(res.ca$row$cos2, 1, cumsum)
dd.t = melt(data)
xyplot(value~Var1|Var2, data=dd.t, type="b",
       main="各軸の行ポイントへの累積寄与",
       xlab="質")

data = apply(res.ca$col$cos2, 1, cumsum)
dd.t = melt(data)
xyplot(value~Var1|Var2, data=dd.t, type="b",
       main="各軸の列ポイントへの累積寄与",
       xlab="質")

rm(data, dd.t)


# 遷移方程式
summary(res.ca)
F = res.ca$row$coord  # 行スコア
G = res.ca$col$coord  # 列スコア
R = tbl2.1a.r[1:3, 1:3]  # 行プロファイル
C = tbl2.1a.c[1:3, 1:3]  # 行プロファイル
Dm12 = diag(1/sqrt(res.ca$eig[, 1]))

(R %*% G %*% Dm12)  # 列スコア→行スコアへ変換　RGD1/2
(C %*% F %*% Dm12)  # 行スコア→列スコアへ変換　CFD1/2

# サプリメンタリー・ポイント
tbl2.1
tbl2.1.s = tbl2.1[c(1:3,5), c(1:3,5,6)]
tbl2.1.s

res.ca.sup = CA(tbl2.1.s, row.sup=4, col.sup=c(4,5), graph=FALSE)

plot.new()
plot(res.ca.sup, title="表2.1を対応分析。サプリメンタリー・ポイント付き")
lines(res.ca.sup$col.sup$coord, lty=2)

res.ca.sup$row.sup


# 遷移方程式を用いたサプリメンタリー・ポイント(全国)の計算

R = tbl2.1.s[,1:3]/apply(tbl2.1.s[,1:3], 1, sum)  #全国を含めた行プロファイル
G  # 全国を含めない列スコア
R %*% G %*% Dm12　# 全国を含む行スコア

sup = (R %*% G %*% Dm12)[4, ]
sup[1]^2/(sup[1]^2 + sup[2]^2)  # 平方相関 cos2 for Dim1
sup[2]^2/(sup[1]^2 + sup[2]^2)  # 平方相関 cos2 for Dim2

res.ca.sup$row.sup  #スコアの確認

# 遷移方程式を用いたサプリメンタリー・ポイント(密・疎)の計算

C = t(tbl2.1.s[1:3,])/apply(t(tbl2.1.s[1:3,]), 1, sum)  #密・疎を含めた列プロファイル
F  # 密・疎を含めない行スコア
C %*% F %*% Dm12　# 密・疎を含む列スコア

sup = (C %*% F %*% Dm12)[4, ]
sup[1]^2/(sup[1]^2 + sup[2]^2)  # 平方相関 cos2 for Dim1
sup[2]^2/(sup[1]^2 + sup[2]^2)  # 平方相関 cos2 for Dim2

sup = (C %*% F %*% Dm12)[5, ]
sup[1]^2/(sup[1]^2 + sup[2]^2)  # 平方相関 cos2 for Dim1
sup[2]^2/(sup[1]^2 + sup[2]^2)  # 平方相関 cos2 for Dim2

res.ca.sup$col.sup  #スコアの確認

