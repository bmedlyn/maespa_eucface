library(caTools)
set.seed(101)
data = out.df
# Create Split (any column is fine)
split = sample.split(out.df$x1, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(data)[-1]

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('y ~',f)

# Convert to formula
f <- as.formula(f)

library(neuralnet)
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

predicted.nn.values <- compute(nn,test[,2:4])
plot(nn,, rep="best")
