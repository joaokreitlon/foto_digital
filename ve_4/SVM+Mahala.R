library(caret)
library(raster)
library(rgdal)
library(dplyr)
library(e1071) 
library(MASS)
#install.packages('dplyr')
#install.packages('e1071')
#install.packages('MASS')   

setwd('C:/Users/joao.kreitlon/Documents/IME/2023.1/PDI')

img<-brick('WV3_VE3.tif')

img@data@names<-c("Coastal","Blue","Green","Yellow","Red","Red_Edge","NIR1","NIR2")

maxBands<-max(getValues(imgRGBcv)) 
maxBands

colorVector <- c("#008c3a", "#66cc00", "#abff57", "#fd9351", "#000000",
                 "#00fff6")
colorVectorWithWhite <- append(colorVector, "#ffffff", 0)


# === OBSERVACAO ===============================================
#
# Classes:
#
# 1: VEGETACAO ALTA
# 2: AGUA
# 3: VEGETACAO RALA
# 4: TERRA
# 5: EDIFICACOES
# 6: CALCADA

# ==============================================================



# Carregue o shapefile de treinamento
shp_treino<-shapefile('classes.shp')

# Carregue o shapefile de teste
shp_teste<-shapefile('test.shp')

# Rasterize the shapes - * change raster base *
raster_base <- img$Coastal

treino<-rasterize(shp_treino,raster_base,field='id')
teste<-rasterize(shp_teste,raster_base,field='id')


# Consultas:

##################################################
############# Classificador de Mahalanobis ######
#################################################
# Classes:
#
# 1: VEGETACAO ALTA
# 2: AGUA
# 3: VEGETACAO RALA
# 4: TERRA
# 5: EDIFICACOES
# 6: CALCADA


# Tabela de amostras

tab <- stack(img,treino)
tab<- data.frame(getValues(tab))
tab
colnames(tab) <- c("Coastal","Blue","Green","Yellow","Red","Red_Edge","NIR1","NIR2", "CLASSE")
tab<-tab[!is.na(tab$CLASSE),]
tab



# Extraia os pixels de VEGETACAO ALTA e calcule a media e a covariancia

pixVegAlta<-as.matrix(tab[tab$CLASSE==1,1:8])
mVegAlta<-apply(pixVegAlta,2,mean)
covVegAlta<-cov(pixVegAlta)

#covVegAlta # teste

# Extraia os pixels de AGUA e calcule a media e a covariancia

pixAgua<-as.matrix(tab[tab$CLASSE==2,1:8])
mAgua<-apply(pixAgua,2,mean)
covAgua<-cov(pixAgua)

# Extraia os pixels de VEGETACAO RALA e calcule a media e a covariancia

pixVegRala<-as.matrix(tab[tab$CLASSE==3,1:8])
mVegRala<-apply(pixVegRala,2,mean)
covVegRala<-cov(pixVegRala)

# Extraia os pixels de TERRA e calcule a media e a covariancia

pixTerra<-as.matrix(tab[tab$CLASSE==4,1:8])
mTerra<-apply(pixTerra,2,mean)
covTerra<-cov(pixTerra)

# Extraia os pixels de EDIFICACOES e calcule a media e a covariancia

pixEdificacoes<-as.matrix(tab[tab$CLASSE==5,1:8])
mEdificacoes<-apply(pixEdificacoes,2,mean)
covEdificacoes<-cov(pixEdificacoes)

# Extraia os pixels de CALCADA e calcule a media e a covariancia

pixCalcada<-as.matrix(tab[tab$CLASSE==6,1:8])
mCalcada<-apply(pixCalcada,2,mean)
covCalcada<-cov(pixCalcada)

# Transformando a imagem em matriz

pixImg<-data.frame(getValues(img))


# Calcule a distancia de Mahalanobis


DistMahaVegAlta<-apply(pixImg, 1,function(x)mahalanobis(x,mVegAlta,covVegAlta))

DistMahaAgua<-apply(pixImg, 1,function(x)mahalanobis(x,mAgua,covAgua))

DistMahaVegRala<-apply(pixImg, 1,function(x)mahalanobis(x,mVegRala,covVegRala))

DistMahaTerra<-apply(pixImg, 1,function(x)mahalanobis(x,mTerra,covTerra))

DistMahaEdificacoes<-apply(pixImg, 1,function(x)mahalanobis(x,mEdificacoes,covEdificacoes))

DistMahaCalcada<-apply(pixImg, 1,function(x)mahalanobis(x,mCalcada,covCalcada))


DistMaha<-cbind(DistMahaVegAlta
                ,DistMahaAgua
                ,DistMahaVegRala
                ,DistMahaTerra
                ,DistMahaEdificacoes
                ,DistMahaCalcada
              )

# Atribua a classe do pixel de acordo com a MENOR distancia  
pixClasses<-as.matrix(apply(DistMaha,1,which.min))

# Construa a imagem classificada. Utilize a funcao setValues
imgClass<-img$Coastal
imgClassMahalanobis<-setValues(imgClass,pixClasses)
win.graph()
plot(imgClassMahalanobis)

# Rotulo dos pixels
# 1: VEGETACAO ALTA
# 2: AGUA
# 3: VEGETACAO RALA
# 4: TERRA
# 5: EDIFICACOES
# 6: CALCADA

# Visualize a imagem classificada. Neste exemplo, 'imgClass' imagem classificada
win.graph(width = 27, height = 27)
par(mai=c(0.9,0.9,0.5,1.4))


# Insira uma legenda
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", inset=c(0,0), title=NULL,
       c( 'VEGETACAO ALTA',
          'AGUA',
          'VEGETACAO RALA',
          'TERRA',
          'EDIFICACOES',
          'CALCADA'),xpd = TRUE, 
       fill=corleg,bty = "n", horiz=F,col = 2, cex=1.2)

###########################################################
############# Avaliação da exatidão de classificação ######
###########################################################

# Construa uma máscara com os pixels de teste
imgIdx<-!is.na(teste)

# Aplique essa máscara nas imagens classificadas e transforme a intersecao em um vetor


pixTesteMahalanobis<-imgClassMahalanobis*imgIdx
pixTesteMahalanobis<-getValues(pixTesteMahalanobis)
pixTeste<-getValues(teste)

# Remova os pixels com NA
idx<-!is.na(pixTeste)

pixTesteMahalanobis<-pixTesteMahalanobis[idx]
pixTeste<-pixTeste[idx]

# Utilize o pacote 'caret' para avaliar a exatidao de classificacao por meio da matriz de confusao
library(caret)
confusionMatrix(as.factor(pixTesteMahalanobis), as.factor(pixTeste))


###########################################################
############################ SVM ##########################
###########################################################

# Selecionando apenas 10% de cada classe:

n_per_class <- round(table(tab$CLASSE) * 0.1)

# Filtro
filter_table <- data.frame()

# For loop em cada classe:

for (i in unique(tab$CLASSE)) {
  
  filter_lines <- filter(tab, CLASSE == i)
  filter_lines <- slice(filter_lines, 1:n_per_class[i])
  filter_table <- rbind(filter_table, filter_lines)
}

params = list(cost=2^(-5:5), gamma=2^(-5:5))

# Criar um grid de classificação, que serão usados no modelo a seguir:
svm_tune <- tune(svm, train.x = filter_table[ ,1:8], train.y = filter_table$CLASSE,
                 kernel = "radial", ranges = params)


img0 <- stack(img,treino)
img0<- data.frame(getValues(img0))
colnames(img0) <- c("Coastal","Blue","Green","Yellow","Red","Red_Edge","NIR1","NIR2", "CLASSE")
img0

# SVM em si:

svm_mod <- svm(CLASSE ~ ., data = img0 , type = "C-classification",
                       kernel = "radial", cost = svm_tune$best.parameters$cost,
                       gamma = svm_tune$best.parameters$gamma)

# Aplicando na imagem de teste:

svm_apply <- predict(svm_mod, img0[ ,1:8])


dataframe_new <- cbind(img0, classe_predict = svm_apply)


imgClaseSVM <- raster::setValues(img$Blue, dataframe_new$classe_predict)
win.graph()
plot(class_svm)

###########################################################
############# Avaliação da exatidão de classificação ######
###########################################################

# Construa uma máscara com os pixels de teste
imgIdx<-!is.na(teste)

# Aplique essa máscara nas imagens classificadas e transforme a intersecao em um vetor


pixTesteSVM<-imgClaseSVM*imgIdx
pixTesteSVM<-getValues(pixTesteSVM)
pixTeste<-getValues(teste)

# Remova os pixels com NA
idx<-!is.na(pixTeste)

pixTesteSVM<-pixTesteSVM[idx]
pixTeste<-pixTeste[idx]

# Utilize o pacote 'caret' para avaliar a exatidao de classificacao por meio da matriz de confusao
library(caret)

confusionMatrix(as.factor(pixTesteSVM), as.factor(pixTeste))
confusionMatrix(as.factor(pixTesteMahalanobis), as.factor(pixTeste))

