install.packages("skewness")
library(tree)
library(skewness)
library(readr)
library(corrplot)
library(caret)
library(tidyverse)
library(magrittr)
library(olsrr)
library(car)
library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(caret)
library(dplyr)
library(ModelMetrics)
library(lmtest)
library(moments)
library(bestNormalize) # normalization 
library(MASS)
library(psych) 
library(mvnTest) # perform multivariate normality test
library(tree) # perform regression and decision tree
library(randomForest) # perform random forest
library(rpart)  # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(olsrr)
library(kmed)
heart -> heart

#age: Hastanın yaşı
#sex: Hastanın cinsiyeti (1 = erkek, 0 = kadın)
#cp: Chest pain (Kalp ağrısı) (0 = asymptomatic, 1 = atypical angina, 2 = non-anginal pain, 3 = typical angina)
#trestbps: Resting blood pressure (dinlenme kan basıncı)
#chol: Serum cholesterol (kolestrol)
#fbs: Fasting blood sugar (aynaç kan şekeri) (1 = true, 0 = false)
#restecg: Resting electrocardiographic results (dinlenme elektrokardiyografik sonuçları)
#thalach: Maximum heart rate achieved (maksimum atım hızı)
#exang: Exercise induced angina (Egzersiz nedenli anjina) (1 = yes, 0 = no)
#oldpeak: ST depression induced by exercise relative to rest (Egzersiz nedenli ST depresyonu)
#slope: The slope of the peak exercise ST segment (Egzersiz ST segmentin eğimi)
#ca: Number of major vessels (0-3) colored by flourosopy (Büyük damarların sayısı)
#thal: 3 = normal; 6 = fixed defect; 7 = reversable defect (normal, sabit kusur, tersine çevrilebilir kusur)
#target: Diagnosis of heart disease (1 = yes, 0 = no) (Kalp hastalığı tanısı)

head(heart)
str(heart)

#heart datasında class sütununu 0 olanları 0 diğer değerleri 1 olarak tanımlamak için;

for (i in 1:nrow(heart)) {
  if(heart$class[i] == 0) {
    heart$class[i] <- 0
  } else {
    heart$class[i] <- 1
  }
}

heart

#datayı incelediğimde sex,fbs,exang kategorik değişkenleri factore dönüştürüyoruz;
heart$class <- as.factor(heart$class)
heart$sex <- as.factor(heart$sex)
heart$fbs <- as.factor(heart$fbs)
heart$exang <- as.factor(heart$exang)

dim(heart)
str(heart)
################Tanımlayıcı İstatistik################################################################
# Tanımlayıcı İstatistik:

# eksik değer gözlemi
sum(is.na(heart))

#eksik değer bulunmamaktadır.

summary(heart)

#Tüm numeric değerlere baktığımızda mean ve medyan arasında büyük farklılıklar olmadığı gözlemlenmiştr. Buna göre çarpıklık yok gibi gözüküyor. Görsel değerlerle daha net sonuçlara varılabilinir.
#class bağımlı değişkenimiz. Kalp hastalığı tanısı 0 olanlar yok 1 olanlar var.

par(mfrow = c(1,4))

boxplot(heart$age, col = "tomato", main = "age")
boxplot(heart$trestbps, col = "tomato", main = "trestbps")
boxplot(heart$chol, col = "tomato", main = "chol")
boxplot(heart$thalach, col = "tomato", main = "thalach")

#trestbps(kan basıncı), chol(kolestrol) ve thalach(maksimum kalp atış hızı) alanlarında outlier var gibi duruyor. Gerçekten etkilimi diye ilerleyen aşamalarda inceleyeceğim.
# histogram ve normallik hipotez heartleri daha doğru yorum yapılmasına olanak sağlayabilir.

dev.off()

#korelasyona bakıyoruz.

corr <- cor(heart[,c("age","trestbps","chol","oldpeak","thalach","ca","class")])
corr
corrplot(corr)


#test train diye ayırıyorum.

smp_size <- floor(0.70 * nrow(heart)) 
set.seed(2022900075) 
train_ind <- sample(nrow(heart), size = smp_size, replace = FALSE)
train <- heart[train_ind, ]
test <- heart[-train_ind, ]

#######Sınıflandırma ve karar ağaçları#########

treeclass <- tree(class~. , train )
summary(treeclass ) # error rate önemli

# Toplam 18 terminal node ile ağaç oluşturulmuş.
# Residual mean deviance 0.463 olarak dikkat çekiyor.
# Error rate 0.11 gibi yüksek sayılabilecek bir değer almış.

plot(treeclass )
text(treeclass ,pretty =0)

# Kök düğüm(root node) hisofprematurelabor'un 1,2 ve 3 olması olarak saptanmış.

set.seed(3)
cv.heart <- cv.tree(treeclass ,FUN=prune.misclass )
cv.heart


treeclass
set.seed(3)
cv.treeclass <- cv.tree(treeclass ,FUN=prune.misclass )
cv.treeclass
par(mfrow=c(1,2))
plot(cv.treeclass$size ,cv.treeclass$dev ,type="b")

# Grafik de incelendiğinde size'ın 7 olduğu noktada deviance'de belirgin azalmalar dikkat çekmekte. Bu sebeple size 2 olarak seçilerek devam edilecektir.

### Budama #####
prune.treeclass <- prune.misclass (treeclass,best=7)
summary(prune.treeclass)


prune.treeclass2 <- prune.misclass (treeclass,best=12)
summary(prune.treeclass2)

#12 yi seçiyorum hata oranı iyi sonuç verdiği için.
# Budama sonrası yalnızca iki node ile model residual mean deviance'ının 0.81 olarak bir artış göstermiş görünüyor.
# Error rate'de de 0.14 olarak artış görülmekte.

dev.off()
plot(prune.treeclass2 )
text(prune.treeclass2 ,pretty =0)
#yorumla.


### Budama Öncesi Tahminler ###

### Train

classtree.pred <- predict(treeclass ,train ,type="class")

caret::confusionMatrix(classtree.pred, train$class)

# Accuracy Rate : 0.89
# Sensitivity : 0.88
# Specificity : 0.89

### Test

classtree.predtest <- predict(treeclass, test, type = "class")

caret::confusionMatrix(classtree.predtest, test$class)

# Accuracy Rate : 0.73
# Sensitivity : 0.70
# Specificity : 0.77

### Budama Sonrası Tahminler ###

### Train

prunedtree.pred <- predict(prune.treeclass2 ,train ,type="class")

caret::confusionMatrix(prunedtree.pred, train$class)

# Accuracy Rate : 0.8841
# Sensitivity : 0.8364
# Specificity : 0.9381

### Test

prunedtree.predtest <- predict(prune.treeclass2, test, type = "class")

caret::confusionMatrix(prunedtree.predtest, test$class)

# Accuracy Rate : 0.7222
# Sensitivity : 0.6600
# Specificity : 0.8000

# Budama sonrası sınıflandırmanın performansının arttığını accuracy rate, sensivity ve specificity değerleri incelenerek kolaylıkla gözlemlenebilir.
# Hem budanmış, hem de orijinal ağacın confusion matrix'i incelendiğinde ise her ikisinde de test verisinde performansın düştüğünü fark ediyoruz -ki bu hiç istediğimiz bir şey değil.


## BAGGING ####


bag <- randomForest(class~. ,data=train, mtry=13,importance=TRUE)

bag$importance
varImpPlot(bag)

# Değişkenlerin önemlerine bakıldığında;

# Mean decrease accuracy incelendiğinde en çok cp, ardından oldpeak,thai,thalach, ca sıralaması görülmekte.
# Gini değerlerine bakıldığında ise cp, oldpeak,hal sıralaması dikkat çekiyor


### Train

baggintrain <- predict(bag ,train ,type="class")

caret::confusionMatrix(baggintrain, train$class)

# Accuracy Rate : 1
# Sensitivity : 1
# Specificity : 1

### Test

baggintest <- predict(bag, test, type = "class")

caret::confusionMatrix(baggintest, test$class)

# Accuracy Rate : 0.80
# Sensitivity : 0.88
# Specificity : 0.70

# Hem budanmış, hem de orijinal ağacın confusion matrix'i incelendiğinde ise her ikisinde de test verisinde performansın düştüğünü fark ediyoruz -ki bu hiç istediğimiz bir şey değil.
# Train verisinde %100'lük başarı ilgi çekici görünüyor.

#Başarı oranı 0.84 olarak gözlemlenmiştir. 

##RANDOM FORREST####

rf <- randomForest(class~. ,data=train, mtry=4,importance=TRUE)

rf$importance
varImpPlot(rf)

# Değişkenlerin önemlerine bakıldığında;

# Mean decrease accuracy incelendiğinde en çok cp, ardından thal, ca, oldpeak, ca sıralaması görülmekte.
# Gini değerlerine bakıldığında ise cp, thalach, oldpeak sıralaması dikkat çekiyor
# Baggin ile oldukça paralel olduğunu söylemek yanlış olmaz.

# Test verilerinde tahmin yapma
predrf <- predict(rf, train)
caret::confusionMatrix(predrf, train$class)

# Accuracy Rate : 1
# Sensitivity : 1
# Specificity : 1

# Tahmin doğruluğunu hesaplama
predrf_test <- predict(rf, test)
caret::confusionMatrix(predrf_test, test$class)

# Accuracy Rate : 0.8222
# Sensitivity : 0.88
# Specificity : 0.75


##Lojistik Regresyon####
# Lojistik regresyon modeli oluşturma

logmodel1 <- glm(class ~ ., data = train, family = binomial(link = "logit"))
summary(logmodel1)

# katsayı yorumu

# Bağımsız değişkenin değerini bir birim arttırdığımızda tahmin değerindeki değişikliği
# belirlemek için önce log(odds) formulünde her iki tarafa exp fonksiyonu uygulanır.

# Anlamlı değişkenlerin katsayı yorumu:


exp(1.587495) # kalp rahatsızlığı olan kişilerin sexTRUE değişkendeki 1 birimlik artış odds oranını 4.89 kat değiştirir.
exp(2.033127)  # kalp rahatsızlığı olan kişilerin cp2 değişkenindeki bir birimlik artış odds oranını 7.63 kat değiştirir
exp(2.720513)  # kalp rahatsızlığı olan kişilerin cp4 değişkenindeki bir birimlik artış odds oranını 15.18 kat değiştirir
exp(0.031994) # kalp rahatsızlığı olan kişilerin trestbps değişkenindeki 1 birimlik artış odds oranını 1.03 kat değiştirir.
exp(-0.029842) # kalp rahatsızlığı olan kişilerin thalach değişkenindeki 1 birimlik artış odds oranını 0.97 kat değiştirir.
exp(1.057276) # kalp rahatsızlığı olan kişilerin ca değişkenindeki 1 birimlik artış odds oranını 2.87 kat değiştirir.
exp(1.162959) # kalp rahatsızlığı olan kişilerin thal7 değişkenindeki 1 birimlik artış odds oranını 3.19 kat değiştirir.

confint.default(logmodel1)
#katsayısına ait %95 lik güven aralığı sıfır değerini kapsadığı için H 0 hipotezi red edilemeyerek aşağıdaki değişkenlerin
# kalp hastası olma etkisinin istatistiksel olarak anlamlı olmadığına karar verilir.

# age, cp3, chol,fbsTRUE, restecg1,restecg2,thalach,exangTRUE,oldpeak, slope2, slope3,thal6

# odds için güven aralığı
# Eğer odds oran değerine ait güven aralığı 1 değerini içermiyor ise H 0 hipotezi red edilerek ilgili katsayının istatistiksel olarak anlamlı olduğuna karar verilir.
odds.confint <- exp(confint.default(logmodel1))
odds.confint
#yorumlanacak
# anlamsız katsayılar: 
# age,cp3,chol,fbsTRUE,restecg1,restecg2,thalach,exangTRUE,oldpeak,slope2,slope3

# anlamlı katsayılar:
#sexTRUE,cp2,cp4,trestbps,thal6,thal7

# Yaşı 1 birim büyük olankalp hastasının oddsu %95 güvenle bir yaş küçük olanın 0.93 katı ile 1.04 katı arasında değer alır.
#  birim büyük olan bir kişinin düşük ağırlıklı çocuk doğurma oddsu %95 güvenle bir birim az olanın 1.39 katı ile 22.28 katı arasında değer alır.
# Smoking bir birim büyük olan bir kişinin düşük ağırlıklı çocuk doğurma oddsu %95 güvenle bir birim az olanın 1.03 katı ile 7.40 katı arasında değer alır.
# Hisofprematurelabor1 bir birim büyük olan bir kişinin düşük ağırlıklı çocuk doğurma oddsu %95 güvenle bir birim az olanın 2.02 katı ile 24.69 katı arasında değer alır.
# Hypertension1 bir birim büyük olan bir kişinin düşük ağırlıklı çocuk doğurma oddsu %95 güvenle bir birim az olanın 1.24 katı ile 31.86 katı arasında değer alır.

# modelin anlamlılığı

#𝐻 0 : 𝛽 1 = 𝛽 2 = ⋯ = 𝛽 𝑘 = 0
# 𝐻 1 : 𝐸𝑛 𝑎𝑧𝚤𝑛𝑑𝑎𝑛 𝑏𝑖𝑟 𝛽 𝑗 ≠ 0

summary(logmodel1)

# G= Null deviance-Residual Deviance
286.15 -135.72

1-pchisq(150.43,206-188) 

#p değeri 0 çıkıyor 

# H 0 :β AGE = 0 hipotezi red edilir.
# Bağımlı değişkenlerin lbw değişkenini açıklamada etkili olduğunu söyleyebilecek yeterli istatistiksel kanıtımız bulunmaktadır.



ppred <- fitted(logmodel1)
summary(ppred)

# İlk olarak threshold değeri medyan değeri kabul edilecek tahminlerde bulunulacaktır.
threshold <- 0.355467
ppred[ppred > threshold] <- 1
ppred[ppred < threshold] <- 0
ppred <- as.factor(ppred)
caret::confusionMatrix(ppred, train$class)
# Accuracy Rate : 0.8647
# Sensitivity : 0.8455
# Specificity : 0.8866
### Test
testpred <- predict(logmodel1, newdata = test)
testpred[testpred > threshold] <- 1
testpred[testpred < threshold] <- 0
testpred <- as.factor(testpred)
caret::confusionMatrix(testpred, test$class)
# Accuracy Rate : 0.8222
# Sensitivity : 0.9000
# Specificity : 0.7250

##LINEAR DISCRIMINANT ANALYSIS#####

#sadece numeric değerleri kullanacağız.

# Ayrıştırma Analizi, veri setindeki değişkenlerin iki veya daha fazla gerçek gruplara ayrılmasını sağlayan, 
# birimlerin p tane özelliğini ele alarak bu birimlerin doğal ortamdaki gerçek gruplarına optimal düzeyde atanmalarını sağlayacak fonksiyonlar türeten bir yöntemdir.

# Teorik olarak her grubun temel özellikleri vardır. Her grup bu temel özelliklerine göre tanımlanır ve bilinir.
# Ayrıştırma Analizinin iki temel görevi vardır.
# Grupları birbirinden ayırmayı sağlayan fonksiyonları bulmak. (AYIRMA)
# Hesaplanan fonksiyonlar aracılığı ile yeni gözlenen bir birimi sınıflandırma hatası minimum olacak şekilde uygun bir sınıfa atamaktır. (SINIFLANDIRMA)

# LDA yönteminde bilinen kategorilerin ayrılmasını en üst düzeye çıkarmaya çalışır.

# Grupların ortalamaları arasında maksimum ayrımı yapacak şekilde bileşen belirler.Varsayımlar: 

# 1. LDA yöntemi açıklayıcı değişkenlerin çok değişkenli normal dağılıma sahip olduğu varsayımına dayanır.
# 2. Doğrusal LDA her bir grup içinde değişkenlerin k x k boyutlu varyans-kovaryans matrislerinin aynı olduğu varsayar.
# 3. Değişkenler arasında çoklu doğrusal bağlantı bulunmamalıdır.
# 4. X matrisi gereğinden fazla ve gereksiz değişken içermemelidir.

# Ayrıştıran bilgi ortalamada değil varyansta ise LDA başarısız olacaktır.


# • Eğer gruplar iyi ayrılmış ise lojistik regresyona göre daha durağan sonuç verir.
# • Eğer örneklem büyüklüğü küçük ve açıklayıcı değişkenler normal dağılıma sahip ise LDA daha iyi performans gösteriyor.
# • Eğer bağımlı değişken ikiden fazla sıralı olmayan kategoriye sahip ise LDA daha iyi sonuç verir.
# • LDA ve lojistik regresyon yöntemleri doğrusal karar sınırları verir.
# • Eğer gerçek sınırlar doğrusal ise LDA ve Lojistik regresyon yöntemleri iyi performans gösterirler.


trainba= train[,c(1,4,5,8,10,14)]
testba= test[,c(1,4,5,8,10,14)]
pairs.panels(trainba[1:5],
             gap=0,
             bg=c("red","orange")[train$class],
             pch=21)
desc=describeBy(train[2:9], train[,1]) # her bir tür için bakmak skor hesaplaması için gerekli
desc

#thalach, age ve chol arasındaki ayrışmalar daha belirgin olarak göze çarpıyor.
#oldpeak ile diğer bütün değişkenler arasında belirgin bir ayrım olmuş.
#chol, age,trestbps arasındaki ayrışmalar çok belirgin değil.

#age ve thalach arasındaki korelasyon yüksek çıkmıştır.Çoklu bağlantı problemi olabilir.


model_lda <- lda(class~.,data=trainba)
model_lda
model_lda$prior # %kaç değer aldıklarını

# Birinci doğrusal ayrıştırma fonksiyonu ile %53 oranında ayrışma sağlanıyor.
#grup ortalamalarına bakıldığında thalach değişimindeki ayrım pair plotta olduğu gibi belirgin bir şekilde gözükmektedir.

tahmin_1<-predict(model_lda,trainba)
hist_lda1<-ldahist(data=tahmin_1$x[,1],g=trainba$class) 

#-1 ile 1 Arasında iki sınıfın birbiriyle örtüştüğü görülmektedir. Bu da çok iyi ayrışmanın sağlanamadığını bize gösterir.

tahmin_1$class # tahmine göre sınıf ayırması
tahmin_1$posterior # gözlemlerin gruplara dahil olma olasılıklarını verir. birbirine yakın olasılıkların değeri çok fazla
tahmin_1$x # fonksiyonda gözlemlerin aldığı değerleri gösterir .histogram karşılaştırmasıyla aynı değerleri alır

trainba$class <- as.factor(trainba$class)
install.packages("klaR")
library(klaR)
partimat(class ~., data=trainba,method="lda") #  kırmızılar yanlış etiketleme gösterir | iyi ayrışma yok

#değişken çiftlerinin ayrışma plotu incelendiğinde en hatalı ayrışmanın chol ve trestbps olarak görülmektedir.
#en az olan ise thalach ve oldpeaktir.

##LDA Confusion####

### Train

ldatrain <- predict(model_lda ,trainba)
caret::confusionMatrix(ldatrain$class, trainba$class)

# Accuracy Rate : 0.7536
# Sensitivity : 0.8364
# Specificity : 0.6598

### Test

ldatest <- predict(model_lda ,testba)
caret::confusionMatrix(ldatest$class, testba$class)

# Accuracy Rate : 0.722
# Sensitivity : 0.76
# Specificity : 0.6750

###QUADRATIC DISCRIMINANT ANALYSIS####

# farklı kovaryans matrisine sahip olabilir 
# eğrisel fonksiyonlarla çalışır
# lda'ya göre daha esnek olduğu için daha düşük varyansı vardır
# eğitim veride gözlem sayısı düşükse lda daha mantıklı
# tam tersi durumda qda daha mantıklı

model_qda<-qda(class~.,data=trainba) 

### Train

trainn <- predict(model_qda ,trainba)
caret::confusionMatrix(trainn$class, trainba$class)

# Accuracy Rate : 0.7536
# Sensitivity : 0.8455
# Specificity : 0.6495

### Test

qdatest <- predict(model_qda ,testba)
caret::confusionMatrix(qdatest$class, testba$class)

# Accuracy Rate : 0.7444
# Sensitivity : 0.80
# Specificity : 0.6750

# %74 tahmin oranı  test verisinde daha başarılı olmasını bekleriz. yeterli başarı yok. 
library(graphics)
partimat(class~., data=trainba, method="qda")

#oldpeak ve thalach en iyisi lda da öyle çıkmıştı. en kötüsü de age ve trestbps çıkmıştır.

###Varsayım Kontrolü#######
#burayı hiç anlamadım!!!!!

# Multivariate Normallik Testleri 

heartba= heart[,c(1,4,5,8,10,14)]
sıfır <- heartba[heartba$class==0,]
bir <- heartba[heartba$class==1,]

install.packages("mvnTest")
library(mvnTest)
# multivariate normallik testleri 

HZ.test(sıfır[, c(1:5)])
HZ.test(bir[, c(1:5)])

# data çok değişkenli normal dağılmıyor. ilk varsayım sağlanılamadı


DH.test(sıfır[, c(1:5)])
DH.test(bir[, c(1:5)])

# ikinci terstten de aynı sonuç verdi

library(bestNormalize)
library(nortest)



# multivariate normal değil



### Varyans Homojenliği Testi

##Assumption Checking of LDA vs. QDA


##http://thatdatatho.com/2018/02/19/assumption-checking-lda-vs-qda-r-tutorial-2/

library(car)
leveneTest(heartba$age ~ as.factor(heartba$class), heartba) # homojen, varyans homojenliği testine göre 0.05 anlamlılık düzeyinde h0 age değişkeni için sınıflara göre varyans homojenliği olduğu hipotezi reddedilebilir. 
leveneTest(heartba$trestbps ~ as.factor(heartba$class), heartba) # homojen değil,reddedilemez
leveneTest(heartba$chol ~ as.factor(heartba$class), heartba) # homojen değil,reddedilemez.
leveneTest(heartba$thalach ~ as.factor(heartba$class), heartba) # homojen, reddedilir
leveneTest(heartba$oldpeak ~ as.factor(heartba$class), heartba) #homojen, reddedilemez.


#We are using the BoxM test in order to check our assumption of homogeneity of variance-covariance matrices.
#H_o = Covariance matrices of the outcome variable are equal across all groups
#H_a = Covariance matrices of the outcome variable are different for at least one group

install.packages("heplots")
library(heplots)

boxm <- heplots::boxM(heartba[, c(1:5)], heartba$class) 
boxm # p-value 0.05'ten küçük çıktığı için bağımsız değişkenlerin kovaryans matrislerinin eşit olduğu varsayımı reddedilemez


###Yapay Sinir Ağları####

# neuralnet paketini yükle
install.packages("neuralnet")

# paketi yüklü olduğunu doğrula
library(neuralnet)

trainba= train[,c(1,4,5,8,10,14)]
testba= test[,c(1,4,5,8,10,14)]


# yapay sinir ağını oluştur
model_nn <- neuralnet(class~.,data=trainba)

tahmin_2<-predict(model_nn,trainba)
trainba$class <- as.factor(trainba$class)


# yapay sinir ağını eğit
nn_fit <- compute(model_nn, trainba)

modelnn <- train(class ~., data = trainba, method = "nnet",
               trControl = trainControl(method = "cv", number = 5),
               tuneLength = 10)
# Modeli test etme
predictions <- predict(modelnn, testba)
confusionMatrix(predictions, testba$class)

caret::confusionMatrix(qdatest$class, testba$class)

#accuracy: 0.67
#sensitivity: 0.72
#specificity: 0.60

## ROC####
install.packages("ROCR")
library(ROCR)

predct <- prediction(as.numeric(as.vector(prunedtree.predtest)), test$class)
predbag <- prediction(as.numeric(as.vector(baggintest)), test$class)
predrf <- prediction(as.numeric(as.vector(predrf_test)), test$class)
predlr <- prediction(as.numeric(as.vector(testpred)), test$class)
predlda <- prediction(as.numeric(as.vector(ldatest$class)), testba$class)
predqda <- prediction(as.numeric(as.vector(qdatest$class)), testba$class)
prednn <- prediction(as.numeric(as.vector(predictions)), test$class)
perfct <- performance( predct, "tpr", "fpr" )
perfbag <- performance(predbag, "tpr", "fpr")
perfrf <- performance(predrf, "tpr", "fpr")
perflr <- performance(predlr, "tpr", "fpr")
perflda <- performance(predlda, "tpr", "fpr")
perfqda <- performance(predqda, "tpr", "fpr")
perfnn <- performance(prednn, "tpr", "fpr")

plot(perfct, col = "darkcyan", lwd = 3)
plot(perfbag, add = TRUE, col = "darksalmon", lwd = 3)
plot(perfrf, add=T, col = "burlywood3", lwd = 3)
plot(perflda, add = TRUE, col = "coral1", lwd = 3)
plot(perfqda,add=T, col = "darkgrey", lwd = 3)
plot(perflr, add = TRUE, col = "dodgerblue3", lwd = 3)
plot(perfnn, add=T, col = "aquamarine", lwd = 2)



aucct <- performance( predct, measure= "auc" )
aucct <- aucct@y.values[[1]]
formatC(aucct, digits = 2)

aucbag <- performance(predbag, "auc")
aucbag <- aucbag@y.values[[1]]
formatC(aucbag, digits = 2)

aucrf <- performance(predrf, "auc")
aucrf <- aucrf@y.values[[1]]
formatC(aucrf, digits = 2)

auclr <- performance(predlr, "auc")
auclr <- auclr@y.values[[1]]
formatC(auclr, digits = 2)

auclda <- performance(predlda, "auc")
auclda <- auclda@y.values[[1]]
formatC(auclda, digits = 2)

aucqda <- performance(predqda, "auc")
aucqda <- aucqda@y.values[[1]]
formatC(aucqda, digits = 2)

aucnn <- performance(prednn, "auc")
aucnn <- aucnn
formatC(aucnn, digits = 2)

#roc ve auc değerlere bakıldığında en iyi sonuç veren modeller rf ve lr olarak saptanmıştır.

#kalp rahatsızlığı sensivity kalp hastası olanları kalp hastası olarak tahmin etme oranı. Sensivity daha yüksek olanı sınıflandrımak daha mantıklı kritik olduğu için sevs en y

