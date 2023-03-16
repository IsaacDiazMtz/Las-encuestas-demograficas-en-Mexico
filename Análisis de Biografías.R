####################################################################################
####################################################################################
###                                                                              ###
###         El impacto de la no corresidencia de los padres con los hijos        ###
###         durante la infancia y/o adolescencia sobre la transicion a la        ###
###                     vida adula: Un análisis multiestado                      ###
###                             (Descriptivos)                                   ###
###                                                                              ###
####################################################################################
####################################################################################

# Librerias

library("Biograph", lib.loc="~/R/win-library/3.5")
library("TraMineR", lib.loc="~/R/win-library/3.5")
library("msm", lib.loc="~/R/win-library/3.5")
library("Matrix", lib.loc="~/R/win-library/3.5")
library("lattice", lib.loc="~/R/win-library/3.5")
library("mvna", lib.loc="~/R/win-library/3.5")
library("mstate", lib.loc="~/R/win-library/3.5")
library("survival", lib.loc="C:/Program Files/R/R-3.5.1/library")
library("eha", lib.loc="~/R/win-library/3.5")
library("KMsurv", lib.loc="~/R/win-library/3.5")
library("survMisc", lib.loc="~/R/win-library/3.5")
library("survminer", lib.loc="~/R/win-library/3.5")
library("ggfortify", lib.loc="~/R/win-library/3.5")
library("flexsurv", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
library("actuar", lib.loc="~/R/win-library/3.5")

# Elementos iniciales

setwd("~/Maestria en Demografía/Metodología de la Investigación II/Metodologia/Intento 2")
base1= read.csv("Base.R.csv",header = T)

base1=base1[base1$Año.nacimiento >= 1970 & base1$Año.nacimiento < 1990,]  # Dos Cohortes
base1=base1[base1$IOS.3. > 0,]                                            # Missings IOS 
base1=base1[base1$tam_loc > 0,]                                           # Missings tam_loc

n=length(base1$ID)
options(scipen=999)
citation("Biograph")

################################  Construcción de la base  ################################

# Nacimientos

id=seq(1,n,by=1)
dia.n=round(runif(n,1,28))
nac=as.vector(base1$Mes.nacimiento)
B=matrix(c(base1$Año.nacimiento,nac,dia.n),n,3)
born=paste(B[,1],B[,2],B[,3],sep="-")

# Salidas

dia.s=round(runif(n,1,28))
mes=round(runif(n,7,12))
año=rep(2017,n)
entrevista=matrix(c(año,mes,dia.s),n,3)
interview=paste(entrevista[,1],entrevista[,2],entrevista[,3],sep="-")

##### Variables explicativas

# Sexo

base1$Sexo[base1$Sexo == 1] = "Mujer"
base1$Sexo[base1$Sexo == 0] = "Hombre"
sex=factor(base1$Sexo)

# Cohorte

Cohort=c(rep(0,n))
base1=cbind(base1,Cohort)
#base1$Cohort[base1$Año.nacimiento >1960 & base1$Año.nacimiento < 1970] = "60´s"
base1$Cohort[base1$Año.nacimiento >= 1970 & base1$Año.nacimiento < 1980] = "70´s"
base1$Cohort[base1$Año.nacimiento >= 1980 & base1$Año.nacimiento < 1990] = "80´s"
#base1$Cohort[base1$Año.nacimiento >= 1990 & base1$Año.nacimiento < 2000] = "90´s"
cohort=factor(base1$Cohort)

# IOS

IOS.3=factor(base1$IOS.3.)
IOS.4=factor(base1$IOS.4.)
IOS.5=factor(base1$IOS.5.)

# Tamaño de localidad

base1$tam_loc[base1$tam_loc == 1] = "Metropolitana"
base1$tam_loc[base1$tam_loc == 2] = "Urbana"
base1$tam_loc[base1$tam_loc == 3] = "Urbana"
base1$tam_loc[base1$tam_loc == 4] = "Rural"
tam_loca=factor(base1$tam_loc)

##### Mecanismos

# Correside vs No Correside

NCP=c(rep(0,n))
NCM=c(rep(0,n))
NC=c(rep(0,n))
base1=cbind(base1,NCP,NCM,NC)

base1$NCP[base1$Infancia.y.adolescencia_pad > 1] = 1
base1$NCM[base1$Infancia.y.adolescencia_mad > 1] = 1
base1$NC[base1$NCP > 0 | base1$NCM > 0 ] = 1

base1$NCP[base1$NCP == 0] = "Corresidio"
base1$NCP[base1$NCP == 1] = "No corresidio"

base1$NCM[base1$NCM == 0] = "Corresidio"
base1$NCM[base1$NCM == 1] = "No corresidio"

base1$NC[base1$NC == 0] = "Corresidio"
base1$NC[base1$NC == 1] = "No corresidio"

corres=factor(base1$NC)

# Paterna

inf.1ra.p=factor(base1$prim_inf_pad)
inf.2da.p=factor(base1$seg_inf_pad)
inf.3ra.p=factor(base1$terc_inf_pad)
ado.tem.p=factor(base1$adoles_temp_pad)
ado.tar.p=factor(base1$adoles_tardia_pad)
inf.7.pad=factor(base1$infancia.7._pad)
inf.3.pad=factor(base1$infancia.3._pad)
ado.3.pad=factor(base1$Adolescencia.3._pad)
ado.2.pad=factor(base1$Adolescencia.2._pad)
inf.ado.p=factor(base1$Infancia.y.adolescencia_pad)

# Materna

inf.1ra.m=factor(base1$prim_inf_mad)
inf.2da.m=factor(base1$seg_inf_mad)
inf.3ra.m=factor(base1$terc_inf_mad)
ado.tem.m=factor(base1$adoles_temp_mad)
ado.tar.m=factor(base1$adoles_tardia_mad)
inf.7.mad=factor(base1$infancia.7._mad)
inf.3.mad=factor(base1$infancia.3._mad)
ado.3.mad=factor(base1$Adolescencia.3._mad)
ado.2.mad=factor(base1$Adolescencia.2._mad)
inf.ado.m=factor(base1$Infancia.y.adolescencia_mad)

##### Transiciones

namstates = c("F","E","D","I","U","H")

base1$Edad.del.primer.empleo[base1$Edad.del.primer.empleo == 0] = 999999
base1$Edad.al.primer.hijo[base1$Edad.al.primer.hijo == 0] = 999999
base1$Edad.de.independencia[base1$Edad.de.independencia == 0] = 999999
base1$Edad.a.la.primera.union[base1$Edad.a.la.primera.union == 0] = 999999

E1=base1$Edad.del.primer.empleo + base1$Año.nacimiento
dia.e=round(runif(n,1,28))
mes.e=round(runif(n,1,12))
E2=data.frame(E1,mes.e,dia.e)
E2$mes.e[E2$E1 > 3000] = 0
E2$dia.e[E2$E1 > 3000] = 0
E2$E1[E2$E1 > 3000] = 0
E=paste(E2$E1,E2$mes.e,E2$dia.e,sep="-")
E[E=="0-0-0"]=NA

D1=base1$Edad.de.desercion.2. + base1$Año.nacimiento
dia.d=round(runif(n,1,28))
mes.d=round(runif(n,1,12))
D2=data.frame(D1,mes.d,dia.d)
D=paste(D2$D1,D2$mes.d,D2$dia.d,sep="-")

I1=base1$Edad.de.independencia + base1$Año.nacimiento
dia.i=round(runif(n,1,28))
mes.i=round(runif(n,1,12))
I2=data.frame(I1,mes.i,dia.i)
I2$mes.i[I2$I1 > 3000] = 0
I2$dia.i[I2$I1 > 3000] = 0
I2$I1[I2$I1 > 3000] = 0
I=paste(I2$I1,I2$mes.i,I2$dia.i,sep="-")
I[I=="0-0-0"]=NA

U1=base1$Edad.a.la.primera.union + base1$Año.nacimiento
dia.u=round(runif(n,1,28))
mes.u=round(runif(n,1,12))
U2=data.frame(U1,mes.u,dia.u)
U2$mes.u[U2$U1 > 3000] = 0
U2$dia.u[U2$U1 > 3000] = 0
U2$U1[U2$U1 > 3000] = 0
U=paste(U2$U1,U2$mes.u,U2$dia.u,sep="-")
U[U=="0-0-0"]=NA

H1=base1$Edad.al.primer.hijo + base1$Año.nacimiento
dia.h=round(runif(n,1,28))
mes.h=round(runif(n,1,12))
H2=data.frame(H1,mes.h,dia.h)
H2$mes.h[H2$H1 > 3000] = 0
H2$dia.h[H2$H1 > 3000] = 0
H2$H1[H2$H1 > 3000] = 0
H=paste(H2$H1,H2$mes.h,H2$dia.h,sep="-")
H[H=="0-0-0"]=NA

d <- data.frame(E=E,D=D,I=I,U=U,H=H,stringsAsFactors =FALSE)         # Une las fechas de las transiciones
nsample <- nrow(d)

dd<- apply(d,1,function(x) y=as.Date(x))
dd <- data.frame(t(dd))                                              #  Cambia el fortato de la fecha
dimnames(dd) <- dimnames(d)

f <- Sequences.ind.0(dd,namstates,absorb=NULL)                       # Genera una lista con 3 elementos: nombre de los estados, tiempos de las transiciones y las trayectorias
dates <- data.frame (f$d)

for (i in 1:5)                                                       # Cambia el formato a fecha y ordena las fechas
{
  dates[,i] <- as.Date(dates[,i],origin="1970-01-01") 
}

path <- as.character(f$path)

# Union de la base

bio  <- data.frame (ID=id,born=born,start=born,end=interview,sex=sex,cohort=cohort,IOS.3=IOS.3,IOS.4=IOS.4,IOS.5=IOS.5,
                    tam_loca=tam_loca,Condición.de.socialización=corres,inf.1ra.p=inf.1ra.p,inf.2da.p=inf.2da.p,inf.3ra.p=inf.3ra.p,
                    ado.tem.p=ado.tem.p,ado.tar.p=ado.tar.p,inf.7.pad=inf.7.pad,inf.3.pad=inf.3.pad,ado.3.pad=ado.3.pad,
                    ado.2.pad=ado.2.pad,inf.ado.p=inf.ado.p,inf.1ra.m=inf.1ra.m,inf.2da.m=inf.2da.m,inf.3ra.m=inf.3ra.m,
                    ado.tem.m=ado.tem.m,ado.tar.m=ado.tar.m,inf.7.mad=inf.7.mad,inf.3.mad=inf.3.mad,ado.3.mad=ado.3.mad,
                    ado.2.mad=ado.2.mad,inf.ado.m=inf.ado.m,path=as.character(path),dates[,1:(max(nchar(path))-1)],
                    stringsAsFactors=FALSE)

namtrans <- paste("Tr",1:ncol(f$d),sep="")
colnames(bio)[33:37] <- namtrans[1:5]

attr(bio,"format.date") <- "%Y-%m-%d"
attr(bio,"format.born") <- "%Y-%m-%d"
attr(bio,"param") <- Parameters (bio)

bio.cmc <- date_b (Bdata=bio, selectday=15, format.out="cmc",covs=NULL)   # Cambia el formato de fecha a CMC


################################  Tabulados  ################################

# Primer nivel de analisis

table(corres)


# Segundo nivel de analisis

tabulados=data.frame(bio.cmc$corres,bio.cmc$sex,bio.cmc$cohort,bio.cmc$IOS.5,bio.cmc$tam_loca)
tabulados=tabulados[tabulados$bio.cmc.corres == "No corresidio",]

table(tabulados$bio.cmc.sex)
table(tabulados$bio.cmc.cohort)
table(tabulados$bio.cmc.IOS.5)
table(tabulados$bio.cmc.tam_loca)

table(tabulados$bio.cmc.sex,tabulados$bio.cmc.cohort)
table(tabulados$bio.cmc.sex,tabulados$bio.cmc.IOS.5)
table(tabulados$bio.cmc.sex,tabulados$bio.cmc.tam_loca)

table(tabulados$bio.cmc.cohort,tabulados$bio.cmc.IOS.5)
table(tabulados$bio.cmc.cohort,tabulados$bio.cmc.tam_loca)

table(tabulados$bio.cmc.IOS,tabulados$bio.cmc.tam_loca)


################################  Descriptivos  ################################

param=Parameters(bio.cmc)
param$numstates    #Numero de estados
param$namstates    #Nombre de los estados
param$ntrans       #Numero de transiciones
param$tmat         
param$transitions   #Transiciones de diferentes formas
                    #TRANS: Numero de transiciones
                    #OR /ORN: Origen
                    #DES / DESN: Destinos
                    #ODN: Origen y destino
param$nntrans       #De 500 individuos ocurrieron 975 transiciones (Muestra el origen y el destino de las 500 transiciones)

seq.ind=Sequences.ind(bio.cmc$path,attr(bio.cmc,"param")$namstates)
overviewE=OverviewEpisodes(bio.cmc,seq.ind)       #Diferentes tipos de episodio
                                                  #LROpen: episodios que comienzan antes o al 
                                                  #inicio de la observación y terminan cuando la observación se suspende 
                                                  #en la fecha de la encuesta. 
                                                  #Un total de 44 episodios pertenecen a esta clase. 
                                                  #Se refieren a los encuestados que ocupan un solo
                                                  #estado a lo largo de la ventana de observación y
                                                  #aún viven en el hogar de los padres en la fecha de la encuesta.
                                                  
                                                  #LOpen:  Episodios que comienzan antes o al inicio
                                                  #de la observación y terminan durante la
                                                  #observación. Los 456 episodios se refieren
                                                  #a los encuestados que experimentan al menos
                                                  #una transición durante el período de observación.
                                                  
                                                  #ROpen: episodios que comienzan durante el período
                                                  #de observación y continúan al final de la
                                                  #observación. El total es de 456. De ellas,
                                                  #42 mujeres cohabitan y no tienen hijos en 
                                                  #la fecha de la encuesta y 321 tienen al menos un hijo.
                                                  
                                                  #Closed: episodios que comienzan y terminan durante el período de observación.
                                                  #El número de episodios de este tipo es 691. Tenga en cuenta que, dado
                                                  #que K representa un estado absorbente, un individuo que ingresa a ese
                                                  #estado permanece en ese estado y lo ocupa en la fecha de la encuesta.
                                                  
                                                  #Type: Duraciones

agetrans=AgeTrans(Bdata=bio.cmc)                   #Edades de cada observación, edad de la censura, estado de entrada, estado de censura

overviewT=OverviewTransitions(Bdata=bio.cmc,seq.ind=seq.ind,agetrans=agetrans)   #Numero de transiciones y promedio de edad a la transicion

# Calculo de tasas de transición

d=solve(diag(overviewE$sojourn[1:6,5]/12))
M=d%*%overviewT$Ttrans[1:6,1:6]                   #Numero de transiciones entre tiempo de exposicion en años

diag(M)=0
diag(M)=-apply(M,1,sum)
dimnames(M)=dimnames(overviewT$Ttrans[1:6,1:6])   #Los elementos diagonales son menos la tasa total de abandono de un estado (tasa de salida)

#####   Secuencias de estado

sequences=Sequences(bio.cmc,mean_median="mean")         #Indica las diferentes secuencias o trayectorias registradas durante el periodo de observación
                                                        #1er elemento: la medida usada para arrojar la edad central de la transicion (Media o mediana) 
                                                        #2do elemento: las secuencias                                                  
                                                        #ncase: individuos con la misma trayectoria
                                                        #ns: Numero de estados
                                                        #tr(#): Edad promedio(o mediana) de transicion  
z.c11=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Hombre" ,],"*E")   #Perfil de edad a la independencia por cohorte
z.c12=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Mujer" ,],"*E")   #Perfil de edad a la independencia por cohorte
z.c13=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Hombre",],"*E")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio
z.c14=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Mujer" ,],"*E")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio

z.c1$case                                                     # Los estados de transicion de origen y destino 
z.c1$n                                                        # Numero de transiciones (Marriages)
z.c1$id                                                       # Id
z.c1$pos                                                      # Posicion del primer matrimonio en la secuencia
z.c1$date                                                     # Fecha en la que ocurrio el matrimonio
z.c1$age                                                      # Edad en la que ocurrio el matrimonio
z.c1$year                                                     # Año en la que ocurrio el matrimonio


z.c21=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Hombre",],"*D")   #Perfil de edad a la independencia por cohorte
z.c22=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Mujer",],"*D")   #Perfil de edad a la independencia por cohorte
z.c23=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Hombre",],"*D")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio
z.c24=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Mujer",],"*D")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio

z.c31=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Hombre",],"*I")   #Perfil de edad a la independencia por cohorte
z.c32=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Mujer",],"*I")   #Perfil de edad a la independencia por cohorte
z.c33=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Hombre",],"*I")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio
z.c34=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Mujer",],"*I")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio

z.c41=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Hombre",],"*U")   #Perfil de edad a la independencia por cohorte
z.c42=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Mujer",],"*U")   #Perfil de edad a la independencia por cohorte
z.c43=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Hombre",],"*U")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio
z.c44=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Mujer",],"*U")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio

z.c51=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Hombre",],"*H")   #Perfil de edad a la independencia por cohorte
z.c52=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="Corresidio" & bio.cmc$sex=="Mujer",],"*H")   #Perfil de edad a la independencia por cohorte
z.c53=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Hombre",],"*H")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio
z.c54=TransitionAB(bio.cmc[bio.cmc$Condición.de.socialización=="No corresidio" & bio.cmc$sex=="Mujer",],"*H")      #Pueden ser H,C,A,K para este caso se utiliza M de matrimonio

xbar=c(mean(z.c11$age), mean(z.c12$age), mean(z.c13$age), mean(z.c14$age),
       mean(z.c21$age), mean(z.c22$age), mean(z.c23$age), mean(z.c24$age), 
       mean(z.c31$age), mean(z.c32$age), mean(z.c33$age), mean(z.c34$age),
       mean(z.c41$age), mean(z.c42$age), mean(z.c43$age), mean(z.c44$age),
       mean(z.c51$age), mean(z.c52$age), mean(z.c53$age), mean(z.c54$age))

xgor=c(median(z.c11$age), median(z.c12$age), median(z.c13$age), median(z.c14$age),
       median(z.c21$age), median(z.c22$age), median(z.c23$age), median(z.c24$age), 
       median(z.c31$age), median(z.c32$age), median(z.c33$age), median(z.c34$age),
       median(z.c41$age), median(z.c42$age), median(z.c43$age), median(z.c44$age),
       median(z.c51$age), median(z.c52$age), median(z.c53$age), median(z.c54$age))


#sigma=c(sd(z.c1$age), sd(z.c2$age), sd(z.c3$age), sd(z.c4$age), sd(z.c5$age), sd(z.c6$age), sd(z.c7$age), sd(z.c8$age), sd(z.c9$age), sd(z.c10$age))

matrix(nrow=4,ncol=5,round(xbar,2),dimnames=list(c("Corresidio M","Corresidio H","No corresidio M","No corresidio H"),c("E","D","I","U","H")))
matrix(nrow=4,ncol=5,round(xgor,2),dimnames=list(c("Corresidio M","Corresidio H","No corresidio M","No corresidio H"),c("E","D","I","U","H")))

#matrix(nrow=2,ncol=5,sigma,dimnames=list(c("No corresidio","Corresidio"),c("I","D","E","U","H")))

SamplePath(bio.cmc,50)                                        #Trayectoria y transciones de un individuo (50)
subjectsID=c(1,2,3)                                            #Trayectorias y transiciones de muchos individuos

# Diagrama de Lexis

Dlong=Biograph.long(bio.cmc)
title1="Vida Adulta"
z1=Lexislines.episodes(bio.cmc,Dlong$Depisode,subjectsID,title=title1)

# Lexis de puntos: tomando la transicion de FD por edad y año calendario, identificando las categorias de iglesia 


bio.corres=bio.cmc[bio.cmc$Condición.de.socialización == "Corresidio",]
bio.no.corres=bio.cmc[bio.cmc$Condición.de.socialización == "No corresidio",]


z2=Lexispoints(Bdata=bio.cmc,transition="*U",title="Hogar parental a Primera unión",cov="Condición.de.socialización",legend="topleft")
z21=Lexispoints(Bdata=bio.corres,transition="*U",title="Hogar parental a primera union (Corresidió)",legend=NULL)
z22=Lexispoints(Bdata=bio.no.corres,transition="*U",title="Hogar parental a primera union (No corresidió)",legend=NULL)

z3=Lexispoints(Bdata=bio.cmc,transition="*I",title="Hogar parental a Independencia",cov="Condición.de.socialización",legend="topleft")
z31=Lexispoints(Bdata=bio.corres,transition="*I",title="Hogar parental a Independencia (Corresidió)",legend=NULL)
z32=Lexispoints(Bdata=bio.no.corres,transition="*I",title="Hogar parental a Independencia (No corresidió)",legend=NULL)

z4=Lexispoints(Bdata=bio.cmc,transition="*E",title="Hogar parental a Primer empleo",cov="Condición.de.socialización",legend="topleft")
z41=Lexispoints(Bdata=bio.corres,transition="*E",title="Hogar parental a Primer empleo (Corresidió)",legend=NULL)
z42=Lexispoints(Bdata=bio.no.corres,transition="*E",title="Hogar parental a Primer empleo (No corresidió)",legend=NULL)

z5=Lexispoints(Bdata=bio.cmc,transition="*D",title="Hogar parental a Primera desercion escolar",cov="Condición.de.socialización",legend="topleft")
z51=Lexispoints(Bdata=bio.corres,transition="*D",title="Hogar parental a Primera desercion escolar (Corresidió)",legend=NULL)
z52=Lexispoints(Bdata=bio.no.corres,transition="*D",title="Hogar parental a Primera desercion escolar (No corresidió)",legend=NULL)

z6=Lexispoints(Bdata=bio.cmc,transition="*H",title="Hogar parental a Primer hijo",cov="Condición.de.socialización",legend="topleft")
z61=Lexispoints(Bdata=bio.corres,transition="*H",title="Hogar parental a Primer hijo (Corresidió)",legend=NULL)
z62=Lexispoints(Bdata=bio.no.corres,transition="*H",title="Hogar parental a Primer hijo (No corresidió)",legend=NULL)


# State Occupancies

bio.M=bio.cmc[bio.cmc$sex == "Mujer",]
occup=Occup(bio.M)                    # El cohorte de la biografia, por ejemplo el promedio del curso de
                                        # vida de los miembros de la cohorte es representado por
                                        # los State Occupancies

occup$state_occup                       # State Occupancies por edades (Como van pasando el total de los individuos a cada estado)
z3=plot(x=occup$state_occup,namstates.desired = c("F","E","D","I","U","H","Censored"),colours=c("yellow","green","blue","red","purple","lightgrey","black"),title="States occupancies by age",area=TRUE,xmin=0,xmax=40)

occup$st_age_1[,]                           #Los estados se identifican por números. El estado de una persona que ya no está bajo observación se denota con +.
DTraMiner=seqconc(occup$st_age_1,sep="-")   #Por cada mes de cada individuo coloca el estado en el que se encontraba

namst=c(param$namstates,"-")
og.seq=seqdef(occup$st_age_1,1:ncol(occup$st_age_1),informant="STS",alphabet = c(param$namstates,"+"))  # Se necesita para el grafico de secuencias de estado 

# Grafico de secuencias de estado

namstatest=c("F","E","D","I","U","H","Censored")
ids=which(bio.cmc$ID%in%subjectsID)
seqplot(og.seq,type="i",tlim=ids,ltext=namstatest,xtlab=c(0:40),withlegend="right")

seqplot(og.seq,type="f")                    # Grafica las 10 mas frecuentes secuencias

sum(attr(seqtab(og.seq),"freq")$Percent)    # El porcentaje las 10 mas frecuentes secuencias
                                            # Es diferente de las secuencias producidas por el codigo sequences
                                            # ya que este ignora la edad de las transiciones en la determinación de las frecuencias

seqplot(og.seq,type="d",group=bio.M$Condición.de.socialización,title="Distribución de los estados por condición de socialización (Mujeres)",ylab="Proporción",xtlab=0:50)
seqplot(og.seq,type="i",group=bio.M$Condición.de.socialización,title="Trayectorias con mayor frecuencia por condición de socialización (Mujeres)",ylab="Trayectorias",xtlab=0:50)
seqplot(og.seq,type="I",group=bio.M$Condición.de.socialización,title="Trayectorias de vida por condición de socialización (Mujeres)",ylab="Trayectorias",xtlab=0:50)
seqplot(og.seq,type="ms",group=bio.M$Condición.de.socialización,title="Estado modal etario por condición de socialización (Mujeres)",ylab="Proporción",xtlab=0:50)
seqplot(og.seq,type="pc",group=bio.M$Condición.de.socialización,title="Red de trayectorias por condición de socialización (Mujeres)",ylab="Estados",xtlab="Transiciones")

                                            # d Distribuciones de estados por porcentaje
                                            # i Las 10 trayectorias mas frecuentes
                                            # I Todas las trayectorias de todos los individuos
                                            # ms el estado con mayor frecuencia por edad
                                            # pc red de estados y numero de transicion

# Probabilidades de transición

tr=round(seqtrate(og.seq),4)

P <- MatrixExp(M,t=12)
P <- round(P,6)
dimnames(P)= dimnames(M)

#####  Perfiles de Edad

z1<- TransitionAB(bio.cmc,"UH")                   # Numero de eventos que transicionaron del estado U al H
z2<- TransitionAB(bio.cmc,"*I")                   # Numero de eventos que estuvieron en el estado I antes de la fecha de la encuesta, indistintamente de haber estado en los otros estados
z3<- TransitionAB(bio.cmc,"F*")                   # Numero de eventos que dejaron el estado F antes de la fecha de la encuesta sin importar el estado destino

z1<- TransitionAB(bio.cmc,"*D")            
meanage <- mean(z1$age,na.rm=TRUE)                 # Edad promedio de transicion del estado U al F 

mean(z1$age[bio.cmc$cohort=="80´s"],na.r=TRUE)     # Edad promedio de transicion del estado U al F para una cohorte
mean(z1$age[bio.cmc$cohort=="70´s"],na.r=TRUE)

z2<- TransitionAB(bio.cmc,"*E")            
z3<- TransitionAB(bio.cmc,"*U")            
z4<- TransitionAB(bio.cmc,"*H")            
z5<- TransitionAB(bio.cmc,"*I")            

meanages1 <- aggregate(z1$age,                                    
                      list(cohorte=bio.cmc[bio.cmc$ID%in%z1$id,]$cohort,
                           sexo=bio.cmc[bio.cmc$ID%in%z1$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z1$id,]$Condición.de.socialización),
                      mean,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

medianages1 <- aggregate(z1$age,                                    
                       list(cohorte=bio.cmc[bio.cmc$ID%in%z1$id,]$cohort,
                            sexo=bio.cmc[bio.cmc$ID%in%z1$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z1$id,]$Condición.de.socialización),
                       median,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

meanages2 <- aggregate(z2$age,                                    
                      list(cohorte=bio.cmc[bio.cmc$ID%in%z2$id,]$cohort,
                           sexo=bio.cmc[bio.cmc$ID%in%z2$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z2$id,]$Condición.de.socialización),
                      mean,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

medianages2 <- aggregate(z2$age,                                    
                       list(cohorte=bio.cmc[bio.cmc$ID%in%z2$id,]$cohort,
                            sexo=bio.cmc[bio.cmc$ID%in%z2$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z2$id,]$Condición.de.socialización),
                       median,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

meanages3 <- aggregate(z3$age,                                    
                      list(cohorte=bio.cmc[bio.cmc$ID%in%z3$id,]$cohort,
                           sexo=bio.cmc[bio.cmc$ID%in%z3$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z3$id,]$Condición.de.socialización),
                      mean,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

medianages3 <- aggregate(z3$age,                                    
                       list(cohorte=bio.cmc[bio.cmc$ID%in%z3$id,]$cohort,
                            sexo=bio.cmc[bio.cmc$ID%in%z3$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z3$id,]$Condición.de.socialización),
                       median,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

meanages4 <- aggregate(z4$age,                                    
                      list(cohorte=bio.cmc[bio.cmc$ID%in%z4$id,]$cohort,
                           sexo=bio.cmc[bio.cmc$ID%in%z4$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z4$id,]$Condición.de.socialización),
                      mean,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

medianages4 <- aggregate(z4$age,                                    
                       list(cohorte=bio.cmc[bio.cmc$ID%in%z4$id,]$cohort,
                            sexo=bio.cmc[bio.cmc$ID%in%z4$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z4$id,]$Condición.de.socialización),
                       median,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

meanages5 <- aggregate(z5$age,                                    
                      list(cohorte=bio.cmc[bio.cmc$ID%in%z5$id,]$cohort,
                           sexo=bio.cmc[bio.cmc$ID%in%z5$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z5$id,]$Condición.de.socialización),
                      mean,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones

medianages5 <- aggregate(z5$age,                                    
                       list(cohorte=bio.cmc[bio.cmc$ID%in%z5$id,]$cohort,
                            sexo=bio.cmc[bio.cmc$ID%in%z5$id,]$sex, corresidencia=bio.cmc[bio.cmc$ID%in%z5$id,]$Condición.de.socialización),
                       median,na.rm=TRUE)            # Edad promedio de transicion del estado U al H combinando las cohortes y las religiones


z<- TransitionAB(bio.cmc,"*I")                    # Edad promedio de transición de cualquier estado al I
meanage <- mean(z$age,na.rm=TRUE)

# Grafico de transicion *U por cohorte, sexo y corres

transition <- "*U"
z <- TransitionAB(bio.cmc,transition)
zzz <- data.frame(cbind(ID=bio.cmc[bio.cmc$ID%in%z$id,]$ID,cohort=bio.cmc[bio.cmc$ID%in%z$id,]$cohort,
                            corresidencia=bio.cmc[bio.cmc$ID%in%z$id,]$Condición.de.socialización,
                            sexo=bio.cmc[bio.cmc$ID%in%z$id,]$sex,IOS=bio.cmc[bio.cmc$ID%in%z$id,]$IOS.5,
                            tamloc=bio.cmc[bio.cmc$ID%in%z$id,]$tam_loca,trans=z$age))

zzz$cohort <- factor(zzz$cohort,labels=c("1970-1979","1980-1989"))
zzz$corresidencia <- factor (zzz$corresidencia,labels=c("Corresidio","No Corresidio"))
zzz$sexo <- factor (zzz$sexo,labels=c("Hombre","Mujer"))
zzz$IOS <- factor (zzz$IOS,labels=c("1er quintil","2do quintil","3er quintil","4to quintil","5to quintil"))
zzz$tamloc <- factor (zzz$tamloc,labels=c("Metropolitana","Rural","Urbana"))


densityplot (~trans|cohort,data=zzz,plot.points="rug",
             main="Hogar parental a Primera unión",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|sexo,data=zzz,plot.points="rug",
             main="Hogar parental a Primera unión",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|IOS,data=zzz,plot.points="rug",
             main="Hogar parental a Primera unión",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|tamloc,data=zzz,plot.points="rug",
             main="Hogar parental a Primera unión",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)




# Grafico de transicion *I por cohorte, sexo y corres

transition <- "*I"
z <- TransitionAB(bio.cmc,transition)
zzz <- data.frame(cbind(ID=bio.cmc[bio.cmc$ID%in%z$id,]$ID,cohort=bio.cmc[bio.cmc$ID%in%z$id,]$cohort,
                        corresidencia=bio.cmc[bio.cmc$ID%in%z$id,]$Condición.de.socialización,
                        sexo=bio.cmc[bio.cmc$ID%in%z$id,]$sex,IOS=bio.cmc[bio.cmc$ID%in%z$id,]$IOS.5,
                        tamloc=bio.cmc[bio.cmc$ID%in%z$id,]$tam_loca,trans=z$age))

zzz$cohort <- factor(zzz$cohort,labels=c("1970-1979","1980-1989"))
zzz$corresidencia <- factor (zzz$corresidencia,labels=c("Corresidio","No Corresidio"))
zzz$sexo <- factor (zzz$sexo,labels=c("Hombre","Mujer"))
zzz$IOS <- factor (zzz$IOS,labels=c("1er quintil","2do quintil","3er quintil","4to quintil","5to quintil"))
zzz$tamloc <- factor (zzz$tamloc,labels=c("Metropolitana","Rural","Urbana"))


densityplot (~trans|cohort,data=zzz,plot.points="rug",
             main="Hogar parental a Independencia",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|sexo,data=zzz,plot.points="rug",
             main="Hogar parental a Independencia",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|IOS,data=zzz,plot.points="rug",
             main="Hogar parental a Independencia",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|tamloc,data=zzz,plot.points="rug",
             main="Hogar parental a Independencia",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

# Grafico de transicion *H por cohorte, sexo y corres

transition <- "*H"
z <- TransitionAB(bio.cmc,transition)
zzz <- data.frame(cbind(ID=bio.cmc[bio.cmc$ID%in%z$id,]$ID,cohort=bio.cmc[bio.cmc$ID%in%z$id,]$cohort,
                        corresidencia=bio.cmc[bio.cmc$ID%in%z$id,]$Condición.de.socialización,
                        sexo=bio.cmc[bio.cmc$ID%in%z$id,]$sex,IOS=bio.cmc[bio.cmc$ID%in%z$id,]$IOS.5,
                        tamloc=bio.cmc[bio.cmc$ID%in%z$id,]$tam_loca,trans=z$age))

zzz$cohort <- factor(zzz$cohort,labels=c("1970-1979","1980-1989"))
zzz$corresidencia <- factor (zzz$corresidencia,labels=c("Corresidio","No Corresidio"))
zzz$sexo <- factor (zzz$sexo,labels=c("Hombre","Mujer"))
zzz$IOS <- factor (zzz$IOS,labels=c("1er quintil","2do quintil","3er quintil","4to quintil","5to quintil"))
zzz$tamloc <- factor (zzz$tamloc,labels=c("Metropolitana","Rural","Urbana"))


densityplot (~trans|cohort,data=zzz,plot.points="rug",
             main="Hogar parental a Primer hijo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|sexo,data=zzz,plot.points="rug",
             main="Hogar parental a Primer hijo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|IOS,data=zzz,plot.points="rug",
             main="Hogar parental a Primer hijo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|tamloc,data=zzz,plot.points="rug",
             main="Hogar parental a Primer hijo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

# Grafico de transicion *D por cohorte, sexo y corres

transition <- "*D"
z <- TransitionAB(bio.cmc,transition)
zzz <- data.frame(cbind(ID=bio.cmc[bio.cmc$ID%in%z$id,]$ID,cohort=bio.cmc[bio.cmc$ID%in%z$id,]$cohort,
                        corresidencia=bio.cmc[bio.cmc$ID%in%z$id,]$Condición.de.socialización,
                        sexo=bio.cmc[bio.cmc$ID%in%z$id,]$sex,IOS=bio.cmc[bio.cmc$ID%in%z$id,]$IOS.5,
                        tamloc=bio.cmc[bio.cmc$ID%in%z$id,]$tam_loca,trans=z$age))

zzz$cohort <- factor(zzz$cohort,labels=c("1970-1979","1980-1989"))
zzz$corresidencia <- factor (zzz$corresidencia,labels=c("Corresidio","No Corresidio"))
zzz$sexo <- factor (zzz$sexo,labels=c("Hombre","Mujer"))
zzz$IOS <- factor (zzz$IOS,labels=c("1er quintil","2do quintil","3er quintil","4to quintil","5to quintil"))
zzz$tamloc <- factor (zzz$tamloc,labels=c("Metropolitana","Rural","Urbana"))


densityplot (~trans|cohort,data=zzz,plot.points="rug",
             main="Hogar parental a Primera deserción escolar",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|sexo,data=zzz,plot.points="rug",
             main="Hogar parental a Primera deserción escolar",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|IOS,data=zzz,plot.points="rug",
             main="Hogar parental a Primera deserción escolar",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|tamloc,data=zzz,plot.points="rug",
             main="Hogar parental a Primera deserción escolar",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

# Grafico de transicion *E por cohorte, sexo y corres

transition <- "*E"
z <- TransitionAB(bio.cmc,transition)
zzz <- data.frame(cbind(ID=bio.cmc[bio.cmc$ID%in%z$id,]$ID,cohort=bio.cmc[bio.cmc$ID%in%z$id,]$cohort,
                        corresidencia=bio.cmc[bio.cmc$ID%in%z$id,]$Condición.de.socialización,
                        sexo=bio.cmc[bio.cmc$ID%in%z$id,]$sex,IOS=bio.cmc[bio.cmc$ID%in%z$id,]$IOS.5,
                        tamloc=bio.cmc[bio.cmc$ID%in%z$id,]$tam_loca,trans=z$age))

zzz$cohort <- factor(zzz$cohort,labels=c("1970-1979","1980-1989"))
zzz$corresidencia <- factor (zzz$corresidencia,labels=c("Corresidio","No Corresidio"))
zzz$sexo <- factor (zzz$sexo,labels=c("Hombre","Mujer"))
zzz$IOS <- factor (zzz$IOS,labels=c("1er quintil","2do quintil","3er quintil","4to quintil","5to quintil"))
zzz$tamloc <- factor (zzz$tamloc,labels=c("Metropolitana","Rural","Urbana"))


densityplot (~trans|cohort,data=zzz,plot.points="rug",
             main="Hogar parental a Primer empleo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|sexo,data=zzz,plot.points="rug",
             main="Hogar parental a Primer empleo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|IOS,data=zzz,plot.points="rug",
             main="Hogar parental a Primer empleo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

densityplot (~trans|tamloc,data=zzz,plot.points="rug",
             main="Hogar parental a Primer empleo",
             sub= paste("",length (na.omit(zzz$corresidencia)),sep=""),
             xlab="Edad",scale=list(x=list(alternating=FALSE)),ylab="Densidad",
             groups=corresidencia,ref=TRUE,
             auto.key=TRUE)

