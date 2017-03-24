
# 1 install the package -----------------------------------------------------

if (!isTRUE(require(mxnet))){
	install.packages("drat", repos="https://cran.rstudio.com")
	drat:::addRepo("dmlc")
	install.packages("mxnet")
}

if (!isTRUE(require(mlbench))){
	install.packages("mlbench")
}

require(mxnet)
require(mlbench)

# 2 two catagories classify model -------------------------------------------


data(Sonar, package="mlbench")
Sonar[,61] = as.numeric(Sonar[,61])-1
train.ind = c(1:50, 100:150)
train.x = data.matrix(Sonar[train.ind, 1:60])
train.y = Sonar[train.ind, 61]
test.x = data.matrix(Sonar[-train.ind, 1:60])
test.y = Sonar[-train.ind, 61]

mx.set.seed(0)

# self defined metric function:
# f = mx.metric.custom("1-moment", function(x,y) 1-abs(x-y))

model = mx.mlp(train.x, 
							 train.y,
							 hidden_node = c(10,10), 
							 out_node = 2, 
							 #activation = "relu", 
							 out_activation = "softmax", 
							 learning.rate = 0.07,
							 num.round=100, 
							 array.batch.size=15, 
							 momentum=0.9, 
							 #eval.metric=f,
							 eval.metric = mx.metric.accuracy)



preds = predict(model, test.x)
pred.label = max.col(t(preds))-1
table(pred.label, test.y)



# 3 self-defined structure network  SYMBOL!!! ------------------------------------------

data(BostonHousing, package="mlbench")

train.ind = seq(1, 506, 3)
train.x = data.matrix(BostonHousing[train.ind, -14])
train.y = BostonHousing[train.ind, 14]
test.x = data.matrix(BostonHousing[-train.ind, -14])
test.y = BostonHousing[-train.ind, 14]


# 定义输入数据
data <- mx.symbol.Variable("data")
# 完整连接的隐藏层
# data: 输入源
# num_hidden: 该层的节点数
fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)

# 针对回归任务，定义损失函数
lro <- mx.symbol.LinearRegressionOutput(fc1)

# 在神经网络中，回归与分类的差别主要在于输出层的损失函数。这里我们使用了平方误差来训练模型。

mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, 
																		 X=train.x, 
																		 y=train.y, 
																		 ctx=mx.cpu(), 
																		 num.round=50, 
																		 array.batch.size=20, 
																		 learning.rate=2e-6, 
																		 momentum=0.9, 
																		 eval.metric=mx.metric.rmse)
# self-defined evaluation method (metric)

demo.metric.mae <- mx.metric.custom("mae", function(label, pred) {
	res <- mean(abs(label-pred))
	return(res)
})

mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, 
																		 X=train.x, 
																		 y=train.y, 
																		 ctx=mx.cpu(), 
																		 num.round=50, 
																		 array.batch.size=20, 
																		 learning.rate=2e-6, 
																		 momentum=0.9, 
																		 eval.metric=demo.metric.mae)


# 4 MNIST -------------------------------------------------------------------

require(mxnet)



train <- read.csv('data/train.csv', header=TRUE) 
test <- read.csv('data/test.csv', header=TRUE) 
train <- data.matrix(train) 
test <- data.matrix(test) 

train.x <- train[,-1] 
train.y <- train[,1]

train.x <- t(train.x/255)
test <- t(test/255)


# 原始灰度图片数值处在[0,255]之间，我们将其变换到[0,1]之间。
# mxnet接受 像素X图片 的输入格式，所以我们对输入矩阵进行了转置。

# LeNet Sybol network structure:

# input
data <- mx.symbol.Variable('data')
# first conv
conv1 <- mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20)
tanh1 <- mx.symbol.Activation(data=conv1, act_type="tanh")
pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",
													 kernel=c(2,2), stride=c(2,2))
# second conv
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50)
tanh2 <- mx.symbol.Activation(data=conv2, act_type="tanh")
pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",
													 kernel=c(2,2), stride=c(2,2))
# first fullc
flatten <- mx.symbol.Flatten(data=pool2)
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")
# second fullc
fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=10)
# loss
lenet <- mx.symbol.SoftmaxOutput(data=fc2)


# array format
train.array <- train.x
dim(train.array) <- c(28, 28, 1, ncol(train.x))
test.array <- test
dim(test.array) <- c(28, 28, 1, ncol(test))

device.cpu <- mx.cpu()

mx.set.seed(0)
tic <- proc.time()

model <- mx.model.FeedForward.create(lenet, 
																		 X=train.array, 
																		 y=train.y, 
																		 ctx=device.cpu, 
																		 num.round=5, 
																		 array.batch.size=100, 
																		 learning.rate=0.05, 
																		 momentum=0.9, 
																		 wd=0.00001, 
																		 eval.metric=mx.metric.accuracy, 
																		 epoch.end.callback=mx.callback.log.train.metric(100))
print(proc.time() - tic)


preds <- predict(model, test.array)
pred.label <- max.col(t(preds)) - 1
submission <- data.frame(ImageId=1:ncol(test), Label=pred.label)
write.csv(submission, file='submission.csv', row.names=FALSE, quote=FALSE)