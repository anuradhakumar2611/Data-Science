detach(Car_Data)
attach(Car_Data)

# Convert to factor variables
Car_Data <- mutate(Car_Data,Fuel = as.factor(Fuel),
                         Colour = as.factor(Colour),
                         Auto = as.factor(mapvalues(Auto, c(0,1), c("No", "Yes"))),
                         Mfr_G = as.factor(mapvalues(Mfr_G, c(0,1), c("No", "Yes"))),
                         ABS = as.factor(mapvalues(ABS, c(0,1), c("No", "Yes"))),
                         Abag_1 = as.factor(mapvalues(Abag_1, c(0,1), c("No", "Yes"))),
                         Abag_2 = as.factor(mapvalues(Abag_2, c(0,1), c("No", "Yes"))),
                         AC = as.factor(mapvalues(AC, c(0,1), c("No", "Yes"))),
                         Comp = as.factor(mapvalues(Comp, c(0,1), c("No", "Yes"))),
                         PStr = as.factor(mapvalues(PStr, c(0,1), c("No", "Yes"))),
                         Radio = as.factor(mapvalues(Radio, c(0,1), c("No", "Yes"))),
                         Clock = as.factor(mapvalues(Clock, c(0,1), c("No", "Yes"))),
                         Pw = as.factor(mapvalues(Pw, c(0,1), c("No", "Yes"))),
                         M_Rim = as.factor(mapvalues(M_Rim, c(0,1), c("No", "Yes"))),
                         MC = as.factor(mapvalues(MC,c(0,1),c("No","Yes"))),
                         CD = as.factor(mapvalues(CD,c(0,1),c("No","Yes"))),
                         SpM = as.factor(mapvalues(SpM,c(0,1),c("No","Yes"))),
                         Tow_Bar = as.factor(mapvalues(Tow_Bar,c(0,1),c("No","Yes"))))

num_cols <- unlist(lapply(Car_Data,is.numeric))
num_cols
car_num <- Car_Data[,num_cols]


#normalize the datset
mins <- apply(car_num,2,min)
maxs <- apply(car_num,2,max)
mins       
maxs
scaled_data <- as.data.frame(scale(car_num,center=mins,scale=maxs-mins))
summary(scaled_data)

data <- data.frame(scaled_data,Car_Data[!num_cols])
str(data)

#nnet or neuralnet
set.seed(234)
indx <- sample(2,nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[indx == 1,]
test <- data[indx == 2,]

nn <- nnet(Price ~ Age+HP+CC+Grs+Colour+Clock+Pw+AC, data=train,linout = T,size=8,decay=0.01)
summary(nn)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot(nn)

nn.pred <- predict(nn,test)
                   
nn.pred
mse <- mean((nn.pred-test$Price)^2)
mse



group0 <- Price[Colour=="red"]
group1 <- Price[Colour=="blue"]
t.test(group0, group1)







set.seed(234)
datacv <- data[sample(nrow(data)),]

#Cross validation
k<- 10  #no of folds
nmodel <- 1 #eg logistic can be changed
folds <- cut(seq(1,nrow(datacv)),breaks = k,labels=FALSE)
moderr <- matrix(-1,k,nmodel,dimnames =list(paste0("Fold",1:k),c("NeuronNet")))

set.seed(234)

for(i in 1:k)
{
  testind <- which(folds==i,arr.ind = TRUE)
  testcv <- datacv[testind,]
  traincv <- datacv[-testind,]
 
  nn <- nnet(Price ~ Age+HP+CC+Grs+Colour+Clock+Pw+AC, data=traincv,linout = T,size=8,decay=0.01)
  pred_nn <- predict(nn,testcv)
  moderr[i] <- mean((pred_nn-testcv$Price)^2)
  
}
mean(moderr)

