

# 問題3.1 -------------------------------------------------------------------

A = matrix(c(3, 1, 2, 2), nrow=2, ncol=2, byrow=TRUE)
B = matrix(c(4, 2, 8, 5), nrow=2, ncol=2, byrow=TRUE)
C = matrix(c(1, 2, 2, 4), nrow=2, ncol=2, byrow=TRUE)

# 問題3.1(1)

AB = A %*% B
AB.t = t(AB)
A.t = t(A)
B.t = t(B)
B.t_A.t = B.t %*% A.t

identical(AB.t, B.t_A.t)

# 問題3.1(2)

AB.inv = solve(AB)
A.inv = solve(A)
B.inv = solve(B)
B.inv_A.inv = B.inv %*% A.inv

identical(AB.inv, B.inv_A.inv)
identical(round(AB.inv, 3), round(B.inv_A.inv, 3))
identical(round(AB.inv, 2), round(B.inv_A.inv, 2))
all.equal(AB.inv, B.inv_A.inv)

# 問題3.1(3)

A.t.inv = solve(A.t)
A.inv.t = t(A.inv)

identical(A.t.inv, A.inv.t)

# 問題3.1(4)

det(AB)
det(A)*det(B)
all.equal(det(AB), det(A)*det(B))

det(t(A))
all.equal(det(t(A)), det(A))

# 問題3.1(5)

qr(A)$rank
qr(B)$rank
qr(C)$rank

# 問題3.1(6)

sum(diag(A+B))
sum(diag(A)) + sum(diag(B))
all.equal(sum(diag(A+B)), sum(diag(A))+sum(diag(B)))

sum(diag(AB))
BA = B %*% A
sum(diag(BA))
all.equal(sum(diag(AB)), sum(diag(BA)))

# 問題3.1(7)

eigen(A)
# |A-λI|=0の確認
det(A - eigen(A)$values[1] * diag(2))
det(A - eigen(A)$values[2] * diag(2))
# 固有値ベクトルの長さの確認
norm(eigen(A)$vector[,1], type="2")
norm(eigen(A)$vector[,2], type="2")

eigen(C)
# |C-λI|=0の確認
det(C - eigen(C)$values[1] * diag(2))
# 固有値ベクトルの長さの確認
norm(eigen(C)$vector[,1], type="2")
norm(eigen(C)$vector[,2], type="2")

# 問題3.1(8)

# T'CT=Λ
T = eigen(C)$vectors
T.t = t(T)
T.t %*% C %*% T
round(T.t %*% C %*% T, 3)


