

############Simumlation de S~################"
p=5
n=100;
T=rexp(1,8)
X=rexp(1,2)
S=X-p*T
k=1
while(k<n){
  T=c(T,T[length(T)]+rexp(1,8))
  X=c(X,X[length(X)]+rexp(1,2))
  S=c(S,X[length(X)]-p*T[length(T)])
  k=k+1
}
plot(S,type = 'l')
#########Simulation de Probabilite#################
p=4.5
n=1000;
cpt=0
c=4
for (i in 1:1000) {
  T=rexp(1,8)
  X=rexp(1,2)
  S=X-p*T
  k=1
  while(k<n){
    T=c(T,T[length(T)]+rexp(1,8))
    X=c(X,X[length(X)]+rexp(1,2))
    S=c(S,X[length(X)]-p*T[length(T)])
    k=k+1
  }
  R=c-S
  for (i in 1:length(R)) {
    if (R[i]<0){
      cpt=cpt+1;
    break
      }
  }
}
Probaproche = cpt/1000

Probaproche
Pexact
Pexact=4/4.5*exp(-4/4.5)
#########L'evolution de Probaliblite en fonction de C ##########
C=0:0.1:18
p=4.5
n=100
P=0
for (l in 1:length(C)) {
  c=C[l]
  cpt=0
  for (i in 1:100) {
    T=rexp(1,8)
    X=rexp(1,2)
    S=X-p*T
    k=1
    while(k<n){
      T=c(T,T[length(T)]+rexp(1,8))
      X=c(X,X[length(X)]+rexp(1,2))
      S=c(S,X[length(X)]-p*T[length(T)])
      k=k+1
    }
    R=c-S
    for (i in 1:length(R)) {
      if (R[i]<0){
        cpt=cpt+1;
        break
      }
    }
  }
  P[l]= cpt/100
}

plot(C,P,type = 'l')


###################################
tmp=(1:100)*10
M=0
V=0
for (i in 1:length(tmp)) {
t=tmp[i];
T=rexp(1,8)
X=rexp(1,2)
Ct=X
while(T[length(T)]<t){
  T=c(T,T[length(T)]+rexp(1,8))
  X=c(X,X[length(X)]+rexp(1,2))
  Ct=c(Ct,X[length(X)])
}
M[i]=mean(Ct)
V[i]=var(Ct)
}


plot(tmp,M,type = 'l')
lines(tmp, 4*tmp, col = "blue")
## on a un probleme a resoudre remplacer 4 par 2 pourqoi??








































