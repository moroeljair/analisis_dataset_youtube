library(readr)
videos <- read_csv("D:/Jair/6tosemestre/Estadisticas para las ciencias de la computacion/Proyecto1/CAvideos.csv")
View(videos)

#plantar semilla
set.seed(2)
#numero de observaciones a escoger
nObservaciones=1000
#sacar posiciones para filas aleatorias
posiciones = sample(40881,nObservaciones)

#sacar la matriz con mil observaciones
matriz1=videos[posiciones,]

#sacar datos para training y test
#70% training, 30% test
nfilas = length(matriz1$views)
fin = round(nfilas*70/100)
mTraining = matriz1[1:fin,]
mTest = matriz1[(fin+1):nfilas,]
iteracion=1
banderaOUTLIER = 0
posSINoutliers=posiciones[1:fin]

bondades=c(0)


'while'
while(banderaOUTLIER==0){
cat("------------------------------")
cat("\n","PARA ITERACION ",iteracion,"\n")
#vectores deseados, escogidos de mTraining
vistas = mTraining$views
likes = mTraining$likes
dislikes = mTraining$dislikes
nComentarios = mTraining$comment_count
'rating disabled es booleano, se tiene que cambiar a 0 o 1'
rating = mTraining$ratings_disabled
rating = rating*1
'comments disabled es booleano, se tiene que cambiar a 0 o 1'
comments = mTraining$comments_disabled
comments = comments*1

'primero se hara un modelo de regresion lineal multiple para predecir los LIKES'
y=likes
x1=vistas
x2=dislikes
x3=nComentarios
x4=rating
x5=comments

#matriz
m = cbind(y,x1,x2,x3,x4,x5)
#ver la correlacion entre variables
cat("CORRELACION ENTRE VARIABLES","\n")
cor(m)

#predecir likes con vistas, dislikes, nComentarios,rating y comments_disabled
reg8 = lm(y~x1+x2+x3+x4+x5)
cat("RESUMEN DEL MODELO","\n")
summary(reg8)

#TEST
#para REG8
'prueba de significancia con matriz test para reg8 porque tiene mejor bondad 
de ajuste'
#coeficientes
b0 = reg8$coefficients[1]
b1 = reg8$coefficients[2]
b2 = reg8$coefficients[3]
b3 = reg8$coefficients[4]
b4 = reg8$coefficients[5]
b5 = reg8$coefficients[6]
b_vector=c(b1,b2,b3,b4,b5)

#vectores deseados, escogidos de mTest
vistasTest = mTest$views
likesTest = mTest$likes
dislikesTest = mTest$dislikes
nComentariosTest = mTest$comment_count
ratingT = mTest$ratings_disabled
ratingT = ratingT*1
'comments disabled es booleano, se tiene que cambiar a 0 o 1'
commentsT = mTest$comments_disabled
commentsT = commentsT*1

#vectores y,x1,x2,x3 de TEST
y_TEST = likesTest
x1_TEST = vistasTest
x2_TEST = dislikesTest
x3_TEST = nComentariosTest
x4_TEST = ratingT
x5_TEST = commentsT
x_matriz = cbind(x1_TEST,x2_TEST,x3_TEST,x4_TEST,x5_TEST)


#calculamos y estimada
yest = b0 + b1*x1_TEST +b2*x2_TEST +b3*x3_TEST +b4*x4_TEST +b5*x5_TEST

#CALCULOS PARA CALCULAR COEFICIENTE DE DETERMINACION

ymed = mean(y_TEST) #media, valor esperado de y
y_ymed = y_TEST-ymed #y - ymedia
error = y_TEST-yest #y - yestimada
n =length(y_TEST)
p = 5 #numero de variable independientes
#suma de cuadrados debida al error sumatoria((y-yest)^2)
SCE = sum(error^2)
#suma total de cuadrados sumatoria((y-ymed)^2)
STC = sum(y_ymed^2)
#suma de cuadrados debida a la regrecion sum((yest-ymed)^2)
SCR = sum((yest-ymed)^2)
#coeficiente de determinacion multiple
R2 = SCR/STC
#coeficiente de determinacion multiple ajustado
R2a = 1-(1-R2)*((n-1)/(n-p-1))

cat("PRUEBAS DE SIGNIFICANCIA CON DATOS TEST","\n")
cat("Bondad de ajuste ajustada:",R2a,"\n")

bondades[iteracion]=R2a

#PRUEBA F
'cuadrado medio de la regresion'
CMR = SCR/p
CME = SCE/(n-p-1)

alfa=0.05

F=CMR/CME
f_estadistico = qf(0.05,p,(n-p-1),lower.tail = FALSE)

cat("F: ",F," f_estadistico",f_estadistico,"\n")

if(F>=f_estadistico){
  print("Se rechaza H0, se acepta Ha. Prueba F")
}else{
  print("Se acepta H0, Prueba F, b1=...=bn=0")
}

'prueba T, para cada vector X'
s=round(sqrt(CME))
t_estadistico = qt(alfa/2,(n-p-1),lower.tail = FALSE)
cat("t estadistico =",t_estadistico,'\n','\n')


for(i in 1:p){
  vecX = x_matriz[,i]
  xmed = mean(vecX)
  x_xmed = vecX - xmed
  sbi = s/sqrt(sum((x_xmed)^2))
  #cat ("bi=",b_vector[i],"sbi",sbi,'\n')
  t=b_vector[i]/sbi
  cat("t value de X",i,"="," ",t)
  if(t>=t_estadistico){
    cat("Se rechaza H0, se acepta Ha, para X",i,'\n')
  }else{
    cat("Se acepta H0, se rechaza Ha, para X",i,'\n')
  }
}


'calculo de valor de OUTLIERS y VALORES INFLUYENTES'
#outliers
'tiene que estar fuera del intervalo [-2;2]'
rs = rstandard(reg8)
#influyentes
'h1 tiene que '
h1 = influence(reg8)$hat
cook = cooks.distance(reg8)

'en el algoritmo siguiente se sacara los outliers, si no son 
valores influyentes'
#posSINoutliers=posiciones[1:fin]
ndatos=length(posSINoutliers)
p=5
umbralValInfluyentes=3*(p+1)/ndatos
if(umbralValInfluyentes>=0.99){
  umbralValInfluyentes=0.99
}

noutliers = 0

for(i in 1:ndatos){
  valorRs = rs[i]
  valorh1 = h1[i]
  valorCook = cook[i]
  bandera = 0
  if(valorRs<(-2) | valorRs>(2)){
    #cat("posicion:",i,";valor outlier:",valorRs,'\n')
    bandera=1
  }
  if(valorh1>umbralValInfluyentes){
    #cat("posicion:",i," h1:",valorh1,'\n')
    bandera=0
  }
  if(valorCook>1){
    #cat("posicion:",i,";cook:",valorCook,'\n')
    bandera=0
  }
  if(bandera==1){
    posSINoutliers=posSINoutliers[-c(i)]
    noutliers=noutliers+1
  }
}

if(noutliers==0){
  banderaOUTLIER=1
}

mTraining = videos[posSINoutliers,]


iteracion=iteracion+1

cat("nOUTLIERS=",noutliers)

}

