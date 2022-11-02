
#Το σύνολο δεδομένων CigarettesSW περιέχει δεδομένα για την κατανάλωση τσιγάρων στην Αμερική αναπολιτεία.

head(CigarettesSW)
tail(CigarettesSW)

# ελέγχουμε την δομή των δεδομένων

str(CigarettesSW)
summary(CigarettesSW)

#Τα ονόματα των μεταβλητών

names(CigarettesSW)

# έλεγχος για missing values

any(is.na(CigarettesSW))

# θα εφαρμόσουμε περιγραφική στατιστική για καποιες από τις μεταβλητές 
# ξεκινάμε από τους πίνακες συνχοτήτων και σχετικών συχνοτήτων για τις ποιοτικές 
#μιας και το έτος και το cpi παίρνουν μόνο 2 τιμές θα τις δουλέψουμε σαν ποιοτικές μεταβλητές γιατί μας ενδιαφέρει η συνχότητά τους
CigarettesSW$year=as.factor(CigarettesSW$year)
CigarettesSW$cpi=as.factor(CigarettesSW$cpi)
#year

freq.year= table(CigarettesSW$year)
freq.year
freq.year.df=as.data.frame((freq.year))
freq.year.df
colnames(freq.year.df)=c("year","frequency")
freq.year.df

rel.freq.year= prop.table(freq.year)
rel.freq.year
rel.freq.year=round(rel.freq.year,2)
rel.freq.year.df=as.data.frame(rel.freq.year)
colnames(rel.freq.year)=c("year","relative_frequency")
rel.freq.year.df

#cpi

freq.cpi= table(CigarettesSW$cpi)
freq.cpi
freq.cpi.df=as.data.frame((freq.cpi))
freq.cpi.df
colnames(freq.cpi.df)=c("cpi","frequency")
freq.cpi.df

rel.freq.cpi= prop.table(freq.cpi)
rel.freq.cpi
rel.freq.cpi=round(rel.freq.cpi,2)
rel.freq.cpi.df=as.data.frame(rel.freq.cpi)
colnames(rel.freq.cpi)=c("cpi","relative_frequency")
rel.freq.cpi.df

#state

freq.state= table(CigarettesSW$state)
freq.state
freq.state.df=as.data.frame((freq.state))
freq.state.df
colnames(freq.state.df)=c("state","frequency")
freq.state.df

rel.freq.state= prop.table(freq.state)
rel.freq.state
rel.freq.state=round(rel.freq.state,2)
rel.freq.state.df=as.data.frame(rel.freq.state)
colnames(rel.freq.state)=c("state","relative_frequency")
rel.freq.state.df

# Οι πίνακες συχνοτήτων μας οδήγησαν στο συμπέρασμα ότι σε αυτό το dataset κάθε πολιτεία εμφανίζεται μία φορά σε κάθε έτος 
#και η cvpi είναι σταθερή ανάλογα με το έτος
#θα συνεχίσουμε με περιγραφική στατιστική και οπτικοποίηση των ποσοτικών μεταβληών
#ξεκινάμε με τα βασικά στατιστικά μέτρα
install.packages("DescTools")
library(DescTools)
library(devtools)
install.packages("usethis")
install.packages("devtools")
install_github("okgreece/DescriptiveStats.OBeu")
install.packages("DescriptiveStats.OBeu")
library(DescriptiveStats.OBeu)



mean(CigarettesSW$population)
median(CigarettesSW$population)
quantile(CigarettesSW$population,c(0.25,0.75))
range(CigarettesSW$population)
Mode(CigarettesSW$population)
max(CigarettesSW$population)
min(CigarettesSW$population)
IQR(CigarettesSW$population)
var(CigarettesSW$population)
sd(CigarettesSW$population)
CV(CigarettesSW$population)
ds.skewness(CigarettesSW$population)
ds.kurtosis(CigarettesSW$population)

#μπορούμε να δούμε και πιο συνοπτικά τα στατιστικά μέσα από συγκεκριμένες εντολές όπως summary, stat.desc,describe & ds.analysis

summary(CigarettesSW$population)
install.packages("pastecs")
library(pastecs)
stat.desc(CigarettesSW$population)

mean(CigarettesSW$packs)
median(CigarettesSW$packs)
quantile(CigarettesSW$packs,c(0.25,0.75))
range(CigarettesSW$packs)
Mode(CigarettesSW$packs)
max(CigarettesSW$packs)
min(CigarettesSW$packs)
IQR(CigarettesSW$packs)
var(CigarettesSW$packs)
sd(CigarettesSW$packs)
CV(CigarettesSW$packs)
ds.skewness(CigarettesSW$packs)
ds.kurtosis(CigarettesSW$packs)

mean(CigarettesSW$income)
median(CigarettesSW$income)
quantile(CigarettesSW$income,c(0.25,0.75))
range(CigarettesSW$income)
Mode(CigarettesSW$income)
max(CigarettesSW$income)
min(CigarettesSW$income)
IQR(CigarettesSW$income)
var(CigarettesSW$income)
sd(CigarettesSW$income)
CV(CigarettesSW$income)
ds.skewness(CigarettesSW$income)
ds.kurtosis(CigarettesSW$income)

mean(CigarettesSW$tax)
median(CigarettesSW$tax)
quantile(CigarettesSW$tax,c(0.25,0.75))
range(CigarettesSW$tax)
Mode(CigarettesSW$tax)
max(CigarettesSW$tax)
min(CigarettesSW$tax)
IQR(CigarettesSW$tax)
var(CigarettesSW$tax)
sd(CigarettesSW$tax)
CV(CigarettesSW$tax)
ds.skewness(CigarettesSW$tax)
ds.kurtosis(CigarettesSW$tax)

mean(CigarettesSW$taxs)
median(CigarettesSW$taxs)
quantile(CigarettesSW$taxs,c(0.25,0.75))
range(CigarettesSW$taxs)
Mode(CigarettesSW$taxs)
max(CigarettesSW$taxs)
min(CigarettesSW$taxs)
IQR(CigarettesSW$taxs)
var(CigarettesSW$taxs)
sd(CigarettesSW$taxs)
CV(CigarettesSW$taxs)
ds.skewness(CigarettesSW$taxs)
ds.kurtosis(CigarettesSW$taxs)


mean(CigarettesSW$price)
median(CigarettesSW$price)
quantile(CigarettesSW$price,c(0.25,0.75))
range(CigarettesSW$price)
Mode(CigarettesSW$price)
max(CigarettesSW$price)
min(CigarettesSW$price)
IQR(CigarettesSW$price)
var(CigarettesSW$price)
sd(CigarettesSW$price)
CV(CigarettesSW$price)
ds.skewness(CigarettesSW$price)
ds.kurtosis(CigarettesSW$price)

# θα συνεχίσουμε με την οπτικοποίηση των δεδομένων με ιστογράμματα και ραβδογράμματα

h1<-hist(CigarettesSW$population,breaks = "scott",main = "histogram of population",xlab="population",,col = "blue",freq = TRUE)
str(h1)

b1<-boxplot(x=CigarettesSW$population,main="boxplot of populaton",col = "yellow")
str(b1)

h2<-hist(CigarettesSW$packs,breaks = "scott",main = "histogram of packs",xlab="packs",,col = "blue",freq = TRUE)

b2<-boxplot(x=CigarettesSW$packs,main="boxplot of packs",col = "yellow")

h3<-hist(CigarettesSW$income,breaks = "scott",main = "histogram of income",xlab="income",,col = "blue",freq = TRUE)

b3<-boxplot(x=CigarettesSW$income,main="boxplot of income",col = "yellow")

h4<-hist(CigarettesSW$tax,breaks = "scott",main = "histogram of tax",xlab="tax",,col = "blue",freq = TRUE)

b4<-boxplot(x=CigarettesSW$tax,main="boxplot of tax",col = "yellow")

h5<-hist(CigarettesSW$taxs,breaks = "scott",main = "histogram of taxs",xlab="taxs",,col = "blue",freq = TRUE)

b5<-boxplot(x=CigarettesSW$taxs,main="boxplot of taxs",col = "yellow")

h6<-hist(CigarettesSW$price,breaks = "scott",main = "histogram of price",xlab="price",,col = "blue",freq = TRUE)

b6<-boxplot(x=CigarettesSW$price,main="boxplot of price",col = "yellow")


#Συνεχίζουμε ελέγχοντας αν οι τιμές ακολουθούν κανονική κατανομή θεωρώντας ότι α=0.01

install.packages("nortest")
library(nortest)
normtest_pop = lillie.test(CigarettesSW$population)
normtest_pop
np1<- shapiro.test(CigarettesSW$population)
np1
str(np1)
 # Η τιμή του p_value<<0,01 άρα απορύπτεται η μηδενική υπόθεση άρα θα πάω σε μη παραμετρικούς ελέγχους για αυτή τη μεταβλητή

normtest_packs = lillie.test(CigarettesSW$packs)
normtest_packs
#p_value>0.01 άρα υποθέτω ότι προέρχονται από κανονική κατανομή 

normtest_income = lillie.test(CigarettesSW$income)
normtest_income
#p_value<<0.01 άρα απορύπτω την μηδενική υπόθεση 

normtest_tax= lillie.test(CigarettesSW$tax)
normtest_tax
#p_value<<0.01 άρα απορύπτω την μηδενική υπόθεση

normtest_taxs= lillie.test(CigarettesSW$taxs)
normtest_taxs
#p_value<<0.01 άρα απορύπτω την μηδενική υπόθεση

normtest_price= lillie.test(CigarettesSW$price)
normtest_price
#p_value<<0.01 άρα απορύπτω την μηδενική υπόθεση
# Για όλες οι μεταβλητές εκτός από την packs θα εκτελέσω μη παραμετρικούς ελέγχους

packsm_test = t.test(
  x = CigarettesSW$packs, 
  alternative = "two.sided", 
  mu = 109, 
  conf.level = 0.99)
packsm_test

#έχουμε ισχυρά στοιχεία υπέρ της μηδενικής υπόθεσης αφού το p_value=0.9451 και η τιμή 109 ανήκει στο 99% διάστημ εμπιστοσύνης
#Κάτι που σημένει ότι μπορούμε να υποθέσομε ότι οι Αμερικάνοι καπνίζουν κατά μέσο ΄όρο 109 πακέτα το χρόνο.

packs_test = t.test(formula = packs ~ year,
  data = CigarettesSW,
  alternative =
    "two.sided",
  paired = FALSE
  ,
  var.equal = TRUE,
  conf.level = 0.99
)
packs_test
#Τα απότελέσματα του τέστ ότι η μηδενική υπόθεση ότι στα δύο έτη καπνίζανε την ίδια ποσότητα πρέπει να απορυφτεί 
#αφού το μηδέν δεν ανήκει στο διάστημα της διαφοράς των μέσων τιμών των πακέτων το 1985-1995 είναι 13.6-37,8 
#Συνεπώς οι Αμερικάνοι κάπνησαν λιγότερο το 1995 από το 1985  

tax1_test = t.test(formula = tax ~ year,
                    data = CigarettesSW,
                    alternative =
                      "two.sided",
                    paired = FALSE
                    ,
                    var.equal = TRUE,
                    conf.level = 0.99
)
tax1_test
# Ο ίδιος έλεγχος για τους φόρους δίνει ότι οι φόροι αυξήθηκαν σημαντικά το 1995 σε σχέση με το 1985 κάτι που ίσως εξηγεί γιατί κάπνησαν λιγότερο
pop1= wilcox.test(
  formula = population ~ year,
  data = CigarettesSW,
  alternative =
    "two.sided"
  ,
  paired = FALSE
  ,
  conf.level = 0.95
)
pop1
#Παρατηρούμε μείωση του καπνίσματος σε κάθε πολειτεία στο έτος 1995 σε σχέση με το 1985
#Υποψιαζόμαστε συσχέτιση μεταξύ των tax και packs, Θα ελέγξουμε την συσχέτιση  αρχικά με ένα γράφημα
library(DescriptiveStats.OBeu)
compare.stats(df=CigarettesSW,group_var = "year",values = "tax")
compare.stats(df=CigarettesSW,group_var="year",values="packs")

#Θα φτιάξουμε θηκογράμματα για την αναπαράσταση αυτών των σχέσεων 

boxplot(formul=tax~year,data= CigarettesSW,main="taxes by year",ylab = "taxes",xlab = "year",col=rainbow(length(levels(CigarettesSW$year))))
boxplot(formula=packs~year,data=CigarettesSW,main="packs per year",ylab="packs",xlab = "year",col=rainbow(length(levels(CigarettesSW$year))))


install.packages("FSA")
library(FSA)

hist(formula=tax~year,data=CigarettesSW,breaks = "scott",xlab = "taxes per person",col="blue")
hist(formula=packs~year,data=CigarettesSW,breaks = "scott",xlab = "packs per person",col = "blue")

#Πρέπει να ελέγξουμε την ενξαρτισία των μεταβλητών tax και packs με τον έλεγχο χ^2 του Pearson

chisq.test(CigarettesSW$packs,CigarettesSW$tax)
#p_value=0.2906>0.05 άρα το τεστ μας λέει ότι είναι ανεξάρτητες ,έχουμε όμως τις αμφιβολίες μας για αυτό το αποτέλεσμα. Ας ελέγξουμε τον συντελεστή συσχέτισης
pcortest<- cor.test(CigarettesSW$packs,CigarettesSW$tax,method = "pearson")
pcortest
#R=-0.6421176 ο συντελεστής συσχέτισης του Pearson συνεπώς υπάρχει ένδειξη για κάποια αρνητική συσχέτιση μεταξύ των μεταβλητών 




plot(x=CigarettesSW$packs,y=CigarettesSW$tax,xlab = "packs per person",ylab = "taxes per person")
packstaxes_lm =lm(formula = CigarettesSW$packs~CigarettesSW$tax,data = CigarettesSW)
packstaxes_lm
summary(packstaxes_lm)
abline(a=packstaxes_lm,col="red")
#Αν χ είναι τα ακέτα και y είναι οι φόροι ανα άτομο τότε η ευθεία γραμμικής παλινδρόμισης θα είναι χ=153.121-1,029y

#Συνεχίζομαι με έλχγο κανονικόητας υπολοίπων με το Shapiro-Wilk
normtest=shapiro.test(packstaxes_lm$residuals)
normtest
install.packages("ggpubr")
install.packages("ggplot2")
library(ggpubr)
library(ggplot2)
ggqqplot(
  data =
    packstaxes_lm,
  conf.int = TRUE,
  conf.int.level = 0.95,
  title = "Q-Q plot for residuals")
#Η τιμή του P_value είναι πολύ μικρή συνεπώς δεν θα εμπιστευτούμε αυτό το μοντέλο για να κάνουμε προβλέψεις

# Παρόλα αυτά οφείλουμε να κάνουμε μία πρόβλεψη και για αυτό το μοντέλο 
predlm = predict(object = packstaxes_lm,newdata = data.frame(tax=c(50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50)))
predlm
ftable(predlm)

#θα ξεκινήσουμε με ένα δέντρο ταξινόμησης
library(tree)
#επειδή θέλουμε να μελετήσουμε κυρίως την μεταβλητή packs, θα βάλουμε ένα δείκτη ο οποίος θα καθορίζει αν κάπνισαν πολύ ή λίγο

set.seed(2)
train1=sample(1:nrow(CigarettesSW),nrow(CigarettesSW)/2)
tree.CigarettesSW=tree(packs~.,CigarettesSW,subset = train1)
summary(tree.CigarettesSW)

#ας το οπτικοποιήσουμε

plot(tree.CigarettesSW)
text(tree.CigarettesSW,pretty = 0,cex=0.7)

#Τώρα θα το βελτιστοποιήσουμε με cross validation
cv.CigarettesSW=cv.tree(tree.CigarettesSW)
cv.CigarettesSW
cvtable<-cbind(cv.CigarettesSW$size,cv.CigarettesSW$dev)
colnames(cvtable)<-c("size","error")
cvtable

#Ας το οπτικοποιήσουμε
plot(cv.CigarettesSW$size,cv.CigarettesSW$dev,type = 'b')

#Το δέντρο δεν χρειάζεται κλάδεμα, Θα κάνουμε προβλέψεις για 7 κόμβους
yhat=predict(tree.CigarettesSW,newdata = CigarettesSW[-train1,])
CigarettesSW.test=CigarettesSW[-train1,"packs"]
plot(yhat,CigarettesSW.test)
abline(0,1)

#Τώρα θα υπολογίσουμε το μέσο τετραγωνικό σφάλμα ώστε να αξιολογήσουμε το μοντέλο μας
library(Metrics)
mse(yhat,CigarettesSW.test)

#mse=468.1243 άρα το μοντέλο μας θα έχει μέσο σφάλμα περίπου 22 πακέτα

# Θα συνεχίσουμε κατασκευάζοντας ένα μοντέλο svm 
#Θα χρησιμοποιήσουμε το 80% του δείγματος για εκπαίδευση και το 20% για έλεγχο

smp_size <- floor(0.8 * nrow(CigarettesSW))
set.seed(3)
train_index <- sample(1:nrow(CigarettesSW), size = smp_size)



# Ορίζουμε τα σύνολα εκπαίδευσης και ελέγχου

train <- CigarettesSW[train_index, ]
test <- CigarettesSW[-train_index, ]

#Θα χρειαστούμε κάποιες βιβλιοθήκες
library("e1071")
library("mlbench")

#Θα ξεκινήσουμε με ένα αρχικό κόστος 10 και στην συνέχεια θα το βελτιστοποιήσουμε
svm.model_1<-svm(packs~.,data=CigarettesSW,kernel="linear",cost=10,gamma=1)


summary(svm.model_1)

#Ας το οπτικοποιήσουμε 

plot(svm.model_1,CigarettesSW,tax~packs)


svm.model$index

svm.pred_1=predict(svm.model_1,test)
xtabr<-table(predict=svm.pred_1,truth=test$packs)
xtabr
library(caret) 
confusionMatrix(xtabr,positive = "pos")

#Θα βελτιστοποιήσουμε το μοντέλο με 10-fold crossvalidation
tune.out<-tune(svm ,packs~., data=train, kernel="sigmoid",
               ranges =list(cost=c(0.01,0.1 ,1 ,10 ,100 ,1000)) )
summary(tune.out)

#το βέλτιστο μοντέλο έχει κόστος 0.1 σύμφωνα με το 10-fold crossvalidation
svm.model_3<-svm(packs~.,data=train,kernel="radial",cost=0.1,gamma=0.9)
bestmode<-svm.model_3

summary(bestmode)


svm.pred_2<-predict(svm.model_3,test)
xtabs<-table(predict=svm.pred_2,truth=test$packs)
xtabs
# Aw δούμε και τα χαρακτηριστικά του βέλτιστου μοντέλου
confusionMatrix(xtabs,positive = "pos")


#Νευρωνικό δύκτιο (Το άφησα μόνο για να μου λύσετε την απορία σχετικά με τον confusion matrix)
install.packages("neuralnet")
library("neuralnet")

ind=sample(2,nrow(CigarettesSW), replace=T, prob=c(0.7,0.3))
trainset=CigarettesSW[ind==1,]
testset=CigarettesSW[ind==2,]
set.seed(2)
library("neuralnet")


# Θα ορίσουμε σαν στήλες τις yes & no της εταβλήτής smokemuch


trainset$yes=trainset$smokemuch=="yes"
trainset$no=trainset$smokemuch=="no"

#Ήρθε η ώρα να καταστευάσουμε το νευρωνικό δίκτυο
network2=neuralnet(yes+no~population+income+tax+price+taxs,data = trainset,hidden = 2)


#Ας δούμε τα πρώτα αποτελέσματα όπως το threshold και τα βήματα (steps)
network2$result.matrix

#Ας δούμε και τα επιμέρους βάρη
head(network2$generalized.weights)

#Ας το οπτικοποιήσουμε

plot(network2)

#Βάζουμε την θέση 6 διότη σε αυτή την στήλη βρίσκεται η μεταβλητή packs που μας ενδιαφέρει
net.predict1=compute(network2,testset[-11])$net.result
net.predict1



net.prediction=c("yes","no")[apply(net.predict1,1,which.max)]
net.predict1

#Αυτή η εντολή κάνει classificaton

predict.table=table(testset$smokemuch,net.prediction)
predict.table
install.packages("caret")
library(caret)
library(Metrics)
install.packages("ggplot2")
install.packages("lattice")
confusionMatrix(predict.table)

