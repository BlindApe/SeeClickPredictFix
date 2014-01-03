RF_BC <- function(onlypred) {
	##
	## randomForest with BAYES_n_LOO_150 dataset and Big Column approach
	##
	## Set the CPUs parameter !
	##
	load(file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_150_big_column.RData', sep=''))
	require('data.table')
	require('randomForest')
	require('foreach')
	require('doParallel')
	TEST_BEGIN = 485
	TRAIN_LENGTH = 60
	MIN_FACTOR_CASES = 10
	TRAIN_BEGIN = TEST_BEGIN - TRAIN_LENGTH
	FEATURES <- c('id_', 'id', 'set', 'response', 'day', 'latitude', 'longitude', 'hour', 
		'week_day', 'summary_len', 'description_len', 'city', 'tag_type', 'source', 'votes', 'comments', 'views')
	MOIN = 5
	TREES = 2800
	CPUs = 4
	descrip <- paste('RF_BC', MOIN, 'trees', TREES, 'train', TRAIN_LENGTH, sep='_')
	cl <- makeCluster(CPUs)
	registerDoParallel(cl)
	alldata <- alldata[!(city=='Chicago' & year==2012 & month<10),]
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	setkey(alldata, tag_type)
	tag_type_freq <- alldata[set!='test' & day >= TRAIN_BEGIN,.N, by= tag_type]
	key_OK <- tag_type_freq[N >= MIN_FACTOR_CASES,tag_type]
	alldata[!(tag_type %in% key_OK), tag_type:='Others']
	alldata[, tag_type:=as.factor(tag_type)]
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	alldata[is.na(tag_type), tag_type:='NA']
	unique_keys <- unique(as.character(alldata$tag_type))
	for (i in unique_keys) {
		alldata[,paste('tag_type_',i,sep=''):=as.integer(tag_type==i)]
	}
	alldata[,c('tag_type','source'):=NULL]
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	if (! onlypred) {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
		cat('Training with ', nTrain, '\n')
		train_data <- as.data.frame(train[,5:ncol(train), with=FALSE])
		z1 <- unclass(Sys.time())
		x <- train_data
		y <- train$response
		RF1 <- foreach(ntree=rep(floor(TREES/CPUs), CPUs), .combine=combine, .packages='randomForest') %dopar% randomForest(x, y, ntree=ntree)
		z2 <- unclass(Sys.time())
		elapsed.time.minutes <- round((z2 - z1) / 60, 2)  
		cat(" elapsed time - ", round(elapsed.time.minutes, 2), "minutes", "\n\n")
		save(RF1, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
		write.csv(importance(RF1),file=paste(MODEL_PATH, 'summary_', descrip, '.csv', sep=''), row.names=FALSE)
	} else {
		if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
			load(paste(MODEL_PATH, descrip, '.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	options("scipen"=100)
	test <- alldata[set=='test',]
	test <- as.data.frame(test)
	pred <-predict(RF1, test[,5:ncol(test)])
	prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0), test$views, test$votes, test$comments))
	setnames(prediction, c('id_', 'id', 'response', 'views', 'votes', 'comments'))
	views <- prediction[views==1, 1:3, with=FALSE]
	votes <- prediction[votes==1, 1:3, with=FALSE]
	comments <- prediction[comments==1, 1:3, with=FALSE]
	setkey(views,id_)
	setkey(votes,id_)
	setkey(comments,id_)
	sent <- data.table(cbind(votes$id, views$response, votes$response, comments$response))
	setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(sent, file=paste(SUBMISSION_PATH, 'RF_BC.csv', sep=''), row.names = FALSE)
}

basic_3_models_train_60_hold_0 <- function(onlypred) {
	## 
	## Basic gbm model for each response trained in last 60 days before the test period
	##
	load(file=paste(DATA_PATH,'alldata_basic.RData',sep=''))
	TEST_BEGIN = 485
	TRAIN_LENGTH = 60
	MIN_FACTOR_CASES = 25
	HOLD_LENGTH = 0
	TRAIN_BEGIN = TEST_BEGIN - TRAIN_LENGTH
	FEATURES <- c('id_', 'id', 'set', 'num_views', 'num_votes', 'num_comments', 'day', 'latitude', 'longitude', 'hour', 
		'week_day', 'summary_len', 'description_len', 'city', 'tag_type', 'source')
	ID = 8
	MOIN = 25
	LR = 0.002
	SEED = 53
	BF = 0.90
	TREES = c(1080,11900,2900)
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	setkey(alldata, tag_type)
	tag_type_freq <- alldata[set!='test' & day > TRAIN_BEGIN,.N, by= tag_type]
	key_OK <- tag_type_freq[N >= MIN_FACTOR_CASES,tag_type]
	alldata[!(tag_type %in% key_OK), tag_type:='Others']
	alldata[, tag_type:=as.factor(tag_type)]
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	alldata[is.na(tag_type), tag_type:='NA']
	unique_keys <- unique(as.character(alldata$tag_type))
	for (i in unique_keys) {
		alldata[,paste('tag_type_',i,sep=''):=as.integer(tag_type==i)]
	}
	alldata[,c('tag_type','source'):=NULL]
	if (HOLD_LENGTH>0){
		alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
		train <- rbind(alldata[set=='train' & day >= TRAIN_BEGIN,], alldata[set=='hold'& day >= TRAIN_BEGIN,])
		nTrain <- nrow(train[set=='train',])
	} else {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
	}
	train <- as.data.frame(train)

	for (RESPONSE in (4:6)) {
		descrip <- paste('gbm_basic_3_models', 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, FEATURES[RESPONSE], sep='_')
		set.seed(SEED)
		##
		## Train or only predict (set the onlypred parameter in train.R)
		##
		if (! onlypred) {
			gbm <- gbm.fit(x=train[,7:ncol(train)], y=train[,RESPONSE],
			distribution = 'gaussian',
			n.trees = TREES[RESPONSE-3],
			interaction.depth = ID,
			n.minobsinnode = MOIN,
			shrinkage = LR,
			bag.fraction = BF,
			nTrain = nTrain,
			keep.data = FALSE,
			verbose = TRUE)
			save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
			write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
		} else {
			if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
				load(paste(MODEL_PATH, descrip, '.RData', sep=''))
			} else {
				cat('The model must be trained before new data predictions','\n')
				return (FALSE)
			}
		}
		##
		## Predict
		##
		test <- as.data.frame(alldata[set=='test',])
		pred <-predict(gbm, test[,7:ncol(test)], n.trees=TREES[RESPONSE-3])
		prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0)))
		setnames(prediction, c('id_','id',FEATURES[RESPONSE]))
		setkey(prediction,id_)
		if (RESPONSE==4){
			sent <- prediction[,2:3, with=FALSE]
		} else {
			sent <- cbind(sent, prediction[, 3, with=FALSE])
		}
	}
	write.csv(sent, file=paste(SUBMISSION_PATH, 'basic_3_models_train_60_hold_0.csv', sep=''), row.names = FALSE)
}

basic_3_models_train_120_hold_60 <- function(onlypred) {
	## 
	## Basic gbm model for each response trained in last 120 days before a hold period of 60 days
	##
	load(file=paste(DATA_PATH,'alldata_basic.RData',sep=''))
	TEST_BEGIN = 485
	HOLD_LENGTH = 60
	TRAIN_LENGTH = 120
	MIN_FACTOR_CASES = 20
	TRAIN_BEGIN = TEST_BEGIN - HOLD_LENGTH - TRAIN_LENGTH
	FEATURES <- c('id_', 'id', 'set', 'num_views', 'num_votes', 'num_comments', 'day', 'latitude', 'longitude', 'hour', 
		'week_day', 'summary_len', 'description_len', 'city', 'tag_type', 'source')
	ID = 8
	MOIN = 20
	SEED = 53
	BF = 0.90
	TREES = c(2700,10900,8000)
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	setkey(alldata, tag_type)
	tag_type_freq <- alldata[set!='test' & day > TRAIN_BEGIN,.N, by= tag_type]
	key_OK <- tag_type_freq[N >= MIN_FACTOR_CASES,tag_type]
	alldata[!(tag_type %in% key_OK), tag_type:='Others']
	alldata[, tag_type:=as.factor(tag_type)]
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	alldata[is.na(tag_type), tag_type:='NA']
	unique_keys <- unique(as.character(alldata$tag_type))
	for (i in unique_keys) {
		alldata[,paste('tag_type_',i,sep=''):=as.integer(tag_type==i)]
	}
	alldata[,c('tag_type','source'):=NULL]
	if (HOLD_LENGTH>0){
		alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
		train <- rbind(alldata[set=='train' & day >= TRAIN_BEGIN,], alldata[set=='hold'& day >= TRAIN_BEGIN,])
		nTrain <- nrow(train[set=='train',])
	} else {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
	}
	train <- as.data.frame(train)

	for (RESPONSE in (4:6)) {
		LR = 0.001
		if (RESPONSE==5) {LR = 0.002}
		descrip <- paste('gbm_basic_3_models', 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, FEATURES[RESPONSE], sep='_')
		set.seed(SEED)
		##
		## Train or only predict (set the onlypred parameter in train.R)
		##
		if (! onlypred) {
			gbm <- gbm.fit(x=train[,7:ncol(train)], y=train[,RESPONSE],
			distribution = 'gaussian',
			n.trees = TREES[RESPONSE-3],
			interaction.depth = ID,
			n.minobsinnode = MOIN,
			shrinkage = LR,
			bag.fraction = BF,
			nTrain = nTrain,
			keep.data = FALSE,
			verbose = TRUE)
			save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
			write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
		} else {
			if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
				load(paste(MODEL_PATH, descrip, '.RData', sep=''))
			} else {
				cat('The model must be trained before new data predictions','\n')
				return (FALSE)
			}
		}
		##
		## Predict
		##
		test <- as.data.frame(alldata[set=='test',])
		pred <-predict(gbm, test[,7:ncol(test)], n.trees=TREES[RESPONSE-3])
		prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0)))
		setnames(prediction, c('id_','id',FEATURES[RESPONSE]))
		setkey(prediction,id_)
		if (RESPONSE==4){
			sent <- prediction[,2:3, with=FALSE]
		} else {
			sent <- cbind(sent, prediction[, 3, with=FALSE])
		}
	}
	write.csv(sent, file=paste(SUBMISSION_PATH, 'basic_3_models_train_120_hold_60.csv', sep=''), row.names = FALSE)
}

gbm_BC_train_60_hold_0 <- function(onlypred) {
	## 
	## gbm model trained in last 60 days before test period with Big Column approach
	##
	load(file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_150_big_column.RData', sep=''))
	TEST_BEGIN = 485
	HOLD_LENGTH = 0
	TRAIN_LENGTH = 60
	MIN_FACTOR_CASES = 60
	TRAIN_BEGIN = TEST_BEGIN - HOLD_LENGTH - TRAIN_LENGTH
	FEATURES <- c('id_', 'id', 'set', 'response', 'day', 'latitude', 'longitude', 'hour', 
		'week_day', 'summary_len', 'description_len', 'city', 'tag_type', 'source', 'votes', 'comments', 'views')
	ID = 15
	MOIN = 30
	LR = 0.002
	RESPONSE = 4
	SEED = 53
	BF = 0.90
	TREES = 1700
	descrip <- paste('gbm_BC', 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, sep='_')
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	setkey(alldata, tag_type)
	tag_type_freq <- alldata[set!='test' & day > TRAIN_BEGIN,.N, by= tag_type]
	key_OK <- tag_type_freq[N >= MIN_FACTOR_CASES,tag_type]
	alldata[!(tag_type %in% key_OK), tag_type:='Others']
	alldata[, tag_type:=as.factor(tag_type)]
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	alldata[is.na(tag_type), tag_type:='NA']
	unique_keys <- unique(as.character(alldata$tag_type))
	for (i in unique_keys) {
		alldata[,paste('tag_type_',i,sep=''):=as.integer(tag_type==i)]
	}
	alldata[,c('tag_type','source'):=NULL]
	if (HOLD_LENGTH>0){
		alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
		train <- rbind(alldata[set=='train' & day >= TRAIN_BEGIN,], alldata[set=='hold'& day >= TRAIN_BEGIN,])
		nTrain <- nrow(train[set=='train',])
	} else {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
	}
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	if (! onlypred) {
		train <- as.data.frame(train)
		set.seed(SEED)
		gbm <- gbm.fit(x=train[,5:ncol(train)], y=train[,RESPONSE],
		distribution = 'gaussian',
		n.trees = TREES,
		interaction.depth = ID,
		n.minobsinnode = MOIN,
		shrinkage = LR,
		bag.fraction = BF,
		nTrain = nTrain,
		keep.data = FALSE,
		verbose = TRUE)
		save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
		write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
	} else {
		if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
			load(paste(MODEL_PATH, descrip, '.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	test <- as.data.frame(alldata[set=='test',])
	pred <-predict(gbm, test[,5:ncol(test)], n.trees=TREES)
	prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0), test$views, test$votes, test$comments))
	setnames(prediction, c('id_', 'id', 'response', 'views', 'votes', 'comments'))
	views <- prediction[views==1, 1:3, with=FALSE]
	votes <- prediction[votes==1, 1:3, with=FALSE]
	comments <- prediction[comments==1, 1:3, with=FALSE]
	setkey(views,id_)
	setkey(votes,id_)
	setkey(comments,id_)
	sent <- data.table(cbind(votes$id, views$response, votes$response, comments$response))
	setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(sent, file=paste(SUBMISSION_PATH, 'gbm_BC_train_60_hold_0.csv', sep=''), row.names = FALSE)
}

gbm_BC_train_60_hold_60 <- function(onlypred) {
	## 
	## gbm model trained in last 60 days before a hold period of 60 days with Big Column approach
	##
	load(file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_150_big_column.RData', sep=''))
	TEST_BEGIN = 485
	HOLD_LENGTH = 60
	TRAIN_LENGTH = 60
	MIN_FACTOR_CASES = 60
	TRAIN_BEGIN = TEST_BEGIN - HOLD_LENGTH - TRAIN_LENGTH
	FEATURES <- c('id_', 'id', 'set', 'response', 'day', 'latitude', 'longitude', 'hour', 
		'week_day', 'summary_len', 'description_len', 'city', 'tag_type', 'source', 'votes', 'comments', 'views')
	ID = 15
	MOIN = 20
	LR = 0.001
	RESPONSE = 4
	SEED = 53
	BF = 0.90
	TREES = 3600
	descrip <- paste('gbm_BC', 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, sep='_')
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	setkey(alldata, tag_type)
	tag_type_freq <- alldata[set!='test' & day > TRAIN_BEGIN,.N, by= tag_type]
	key_OK <- tag_type_freq[N >= MIN_FACTOR_CASES,tag_type]
	alldata[!(tag_type %in% key_OK), tag_type:='Others']
	alldata[, tag_type:=as.factor(tag_type)]
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	alldata[is.na(tag_type), tag_type:='NA']
	unique_keys <- unique(as.character(alldata$tag_type))
	for (i in unique_keys) {
		alldata[,paste('tag_type_',i,sep=''):=as.integer(tag_type==i)]
	}
	alldata[,c('tag_type','source'):=NULL]
	if (HOLD_LENGTH>0){
		alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
		train <- rbind(alldata[set=='train' & day >= TRAIN_BEGIN,], alldata[set=='hold'& day >= TRAIN_BEGIN,])
		nTrain <- nrow(train[set=='train',])
	} else {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
	}
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	if (! onlypred) {
		train <- as.data.frame(train)
		set.seed(SEED)
		gbm <- gbm.fit(x=train[,5:ncol(train)], y=train[,RESPONSE],
		distribution = 'gaussian',
		n.trees = TREES,
		interaction.depth = ID,
		n.minobsinnode = MOIN,
		shrinkage = LR,
		bag.fraction = BF,
		nTrain = nTrain,
		keep.data = FALSE,
		verbose = TRUE)		
		save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
		write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
	} else {
		if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
			load(paste(MODEL_PATH, descrip, '.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	test <- as.data.frame(alldata[set=='test',])
	pred <-predict(gbm, test[,5:ncol(test)], n.trees=TREES)
	prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0), test$views, test$votes, test$comments))
	setnames(prediction, c('id_', 'id', 'response', 'views', 'votes', 'comments'))
	views <- prediction[views==1, 1:3, with=FALSE]
	votes <- prediction[votes==1, 1:3, with=FALSE]
	comments <- prediction[comments==1, 1:3, with=FALSE]
	setkey(views,id_)
	setkey(votes,id_)
	setkey(comments,id_)
	sent <- data.table(cbind(votes$id, views$response, votes$response, comments$response))
	setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(sent, file=paste(SUBMISSION_PATH, 'gbm_BC_train_60_hold_60.csv', sep=''), row.names = FALSE)
}

gbm_BC_train_1000_hold_0 <- function(onlypred) {
	## 
	## gbm model trained for all training period with Big Column approach
	##
	load(file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_150_big_column.RData', sep=''))
	TEST_BEGIN = 485
	HOLD_LENGTH = 0
	TRAIN_LENGTH = 1000
	MIN_FACTOR_CASES = 60
	TRAIN_BEGIN = TEST_BEGIN - HOLD_LENGTH - TRAIN_LENGTH
	FEATURES <- c('id_', 'id', 'set', 'response', 'day', 'latitude', 'longitude', 'hour', 
		'week_day', 'summary_len', 'description_len', 'city', 'tag_type', 'source', 'votes', 'comments', 'views')
	ID = 15
	MOIN = 30
	LR = 0.001
	RESPONSE = 4
	SEED = 53
	BF = 0.90
	TREES = 6000
	descrip <- paste('gbm_BC', 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, sep='_')
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	setkey(alldata, tag_type)
	tag_type_freq <- alldata[set!='test' & day > TRAIN_BEGIN,.N, by= tag_type]
	key_OK <- tag_type_freq[N >= MIN_FACTOR_CASES,tag_type]
	alldata[!(tag_type %in% key_OK), tag_type:='Others']
	alldata[, tag_type:=as.factor(tag_type)]
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	alldata[is.na(tag_type), tag_type:='NA']
	unique_keys <- unique(as.character(alldata$tag_type))
	for (i in unique_keys) {
		alldata[,paste('tag_type_',i,sep=''):=as.integer(tag_type==i)]
	}
	alldata[,c('tag_type','source'):=NULL]
	if (HOLD_LENGTH>0){
		alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
		train <- rbind(alldata[set=='train' & day >= TRAIN_BEGIN,], alldata[set=='hold'& day >= TRAIN_BEGIN,])
		nTrain <- nrow(train[set=='train',])
	} else {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
	}
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	if (! onlypred) {
		train <- as.data.frame(train)
		set.seed(SEED)
		gbm <- gbm.fit(x=train[,5:ncol(train)], y=train[,RESPONSE],
		distribution = 'gaussian',
		n.trees = TREES,
		interaction.depth = ID,
		n.minobsinnode = MOIN,
		shrinkage = LR,
		bag.fraction = BF,
		nTrain = nTrain,
		keep.data = FALSE,
		verbose = TRUE)
		save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
		write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
	} else {
		if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
			load(paste(MODEL_PATH, descrip, '.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	test <- as.data.frame(alldata[set=='test',])
	pred <-predict(gbm, test[,5:ncol(test)], n.trees=TREES)
	prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0), test$views, test$votes, test$comments))
	setnames(prediction, c('id_', 'id', 'response', 'views', 'votes', 'comments'))
	views <- prediction[views==1, 1:3, with=FALSE]
	votes <- prediction[votes==1, 1:3, with=FALSE]
	comments <- prediction[comments==1, 1:3, with=FALSE]
	setkey(views,id_)
	setkey(votes,id_)
	setkey(comments,id_)
	sent <- data.table(cbind(votes$id, views$response, votes$response, comments$response))
	setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(sent, file=paste(SUBMISSION_PATH, 'gbm_BC_train_1000_hold_0.csv', sep=''), row.names = FALSE)
}

gbm_BC_BAYES_train_60_hold_0 <- function(onlypred) {
	## 
	## gbm model trained for the last 60 days before test period with Big Column approach
	## uses LOO (features based in leave one out time and geographic radial basis average) and BAYES (features based in leave one out geographic radial basis average of last 150 days)
	##
	load(file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_150_big_column.RData', sep=''))
	TEST_BEGIN = 485
	HOLD_LENGTH = 0
	TRAIN_LENGTH = 60
	MIN_FACTOR_CASES = 30
	TRAIN_BEGIN = TEST_BEGIN - HOLD_LENGTH - TRAIN_LENGTH
	BAYES = TRUE
	FEATURES <- c('id_', 'id', 'set', 'response', 'day', 'city', 'hour', 'week_day', 'summary_len', 'description_len', 'tag_group', 'source', 
	'votes', 'comments', 'views', 'time_left', 'rain_snow_city_3', 'rain_snow_city_14', 'rain_snow_city_60', 'rain_snow_district_3', 'rain_snow_district_14', 'rain_snow_district_60',
	'rain_snow_neighbour_3', 'rain_snow_neighbour_14', 'rain_snow_neighbour_60', 'crime_n_social_city_3', 'crime_n_social_city_14', 'crime_n_social_city_60', 
	'crime_n_social_district_3', 'crime_n_social_district_14', 'crime_n_social_district_60', 'crime_n_social_neighbour_3', 'crime_n_social_neighbour_14', 'crime_n_social_neighbour_60',
	'lights_city_3', 'lights_city_14', 'lights_city_60', 'lights_district_3', 'lights_district_14', 'lights_district_60', 'lights_neighbour_3', 'lights_neighbour_14', 'lights_neighbour_60',
	'pothole_city_3', 'pothole_city_14', 'pothole_city_60', 'pothole_district_3', 'pothole_district_14', 'pothole_district_60', 'pothole_neighbour_3', 'pothole_neighbour_14', 'pothole_neighbour_60',
	'graffiti_city_3', 'graffiti_city_14', 'graffiti_city_60', 'graffiti_district_3', 'graffiti_district_14', 'graffiti_district_60', 'graffiti_neighbour_3', 'graffiti_neighbour_14', 'graffiti_neighbour_60',
	'traffic_city_3', 'traffic_city_14', 'traffic_city_60', 'traffic_district_3', 'traffic_district_14', 'traffic_district_60', 'traffic_neighbour_3', 'traffic_neighbour_14', 'traffic_neighbour_60',
	'trash_city_3', 'trash_city_14', 'trash_city_60', 'trash_district_3', 'trash_district_14', 'trash_district_60', 'trash_neighbour_3', 'trash_neighbour_14', 'trash_neighbour_60', 
	'NA_city_3', 'NA_city_14', 'NA_city_60', 'NA_district_3', 'NA_district_14', 'NA_district_60', 'NA_neighbour_3', 'NA_neighbour_14', 'NA_neighbour_60', 'Other_city_3', 'Other_city_14', 'Other_city_60',
	'Other_district_3', 'Other_district_14', 'Other_district_60', 'Other_neighbour_3', 'Other_neighbour_14', 'Other_neighbour_60', 'trees_city_3', 'trees_city_14', 'trees_city_60',
	'trees_district_3', 'trees_district_14', 'trees_district_60', 'trees_neighbour_3', 'trees_neighbour_14', 'trees_neighbour_60', 'hydrant_city_3', 'hydrant_city_14', 'hydrant_city_60',
	'hydrant_district_3', 'hydrant_district_14', 'hydrant_district_60', 'hydrant_neighbour_3', 'hydrant_neighbour_14', 'hydrant_neighbour_60', 'total_city_3', 'total_city_14', 'total_city_60',
	'total_district_3', 'total_district_14', 'total_district_60', 'total_neighbour_3', 'total_neighbour_14', 'total_neighbour_60', 'self_city_3', 'self_city_14', 'self_city_60', 
	'self_district_3', 'self_district_14', 'self_district_60', 'self_neighbour_3', 'self_neighbour_14', 'self_neighbour_60')
	if (BAYES) {
		FEATURES <- c(FEATURES, 'views_city', 'views_district', 'views_neighbour', 'votes_city', 'votes_district', 'votes_neighbour',
		'comments_city', 'comments_district', 'comments_neighbour', 'self_views_city', 'self_views_district', 'self_views_neighbour',
		'self_votes_city',	'self_votes_district', 'self_votes_neighbour', 'self_comments_city', 'self_comments_district', 'self_comments_neighbour')
	}
	FIRST_FEATURE = 6
	ID = 8
	MOIN = 30
	LR = 0.0015
	RESPONSE = 4
	SEED = 53
	BF = 0.90
	TREES = 2200
	descrip = paste('gbm_BC_BAYES', 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, sep='_')
	cat(descrip, '\n')
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	unique_keys <- unique(as.character(alldata$tag_group))
	for (i in unique_keys) {
		alldata[,paste('tag_group_',i,sep=''):=as.integer(tag_group==i)]
	}
	alldata[,c('tag_group','source'):=NULL]
	if (HOLD_LENGTH>0){
		alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
		train <- rbind(alldata[set=='train' & day >= TRAIN_BEGIN], alldata[set=='hold'& day >= TRAIN_BEGIN,])
		nTrain <- nrow(train[set=='train',])
	} else {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
	}
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	if (! onlypred) {
		train <- as.data.frame(train)
		set.seed(SEED)
		gbm <- gbm.fit(x=train[,FIRST_FEATURE:ncol(train)], y=train[,RESPONSE],
		distribution = 'gaussian',
		n.trees = TREES,
		interaction.depth = ID,
		n.minobsinnode = MOIN,
		shrinkage = LR,
		bag.fraction = BF,
		nTrain = nTrain,
		keep.data = FALSE,
		verbose = TRUE)
		save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
		write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
	} else {
		if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
			load(paste(MODEL_PATH, descrip, '.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	options("scipen"=100)
	test <- as.data.frame(alldata[set=='test',])
	pred <-predict(gbm, test[,FIRST_FEATURE:ncol(test)], n.trees=TREES)
	prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0), test$views, test$votes, test$comments))
	setnames(prediction, c('id_', 'id', 'response', 'views', 'votes', 'comments'))
	views <- prediction[views==1, 1:3, with=FALSE]
	votes <- prediction[votes==1, 1:3, with=FALSE]
	comments <- prediction[comments==1, 1:3, with=FALSE]
	setkey(views,id_)
	setkey(votes,id_)
	setkey(comments,id_)
	sent <- data.table(cbind(votes$id, views$response, votes$response, comments$response))
	setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(sent, file=paste(SUBMISSION_PATH, 'gbm_BC_BAYES_train_60_hold_0.csv', sep=''), row.names = FALSE)
}

gbm_BC_LOO_train_60_hold_0 <- function(onlypred) {
	## 
	## gbm model trained for the last 60 days before test period with Big Column approach
	## uses LOO (features based in leave one out time and geographic radial basis average)
	##
	load(file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_150_big_column.RData', sep=''))
	TEST_BEGIN = 485
	HOLD_LENGTH = 0
	TRAIN_LENGTH = 60
	MIN_FACTOR_CASES = 30
	TRAIN_BEGIN = TEST_BEGIN - HOLD_LENGTH - TRAIN_LENGTH
	FEATURES <- c('id_', 'id', 'set', 'response', 'day', 'city', 'hour', 'week_day', 'summary_len', 'description_len', 'tag_group', 'source', 
	'votes', 'comments', 'views', 'time_left', 'rain_snow_city_3', 'rain_snow_city_14', 'rain_snow_city_60', 'rain_snow_district_3', 'rain_snow_district_14', 'rain_snow_district_60',
	'rain_snow_neighbour_3', 'rain_snow_neighbour_14', 'rain_snow_neighbour_60', 'crime_n_social_city_3', 'crime_n_social_city_14', 'crime_n_social_city_60', 
	'crime_n_social_district_3', 'crime_n_social_district_14', 'crime_n_social_district_60', 'crime_n_social_neighbour_3', 'crime_n_social_neighbour_14', 'crime_n_social_neighbour_60',
	'lights_city_3', 'lights_city_14', 'lights_city_60', 'lights_district_3', 'lights_district_14', 'lights_district_60', 'lights_neighbour_3', 'lights_neighbour_14', 'lights_neighbour_60',
	'pothole_city_3', 'pothole_city_14', 'pothole_city_60', 'pothole_district_3', 'pothole_district_14', 'pothole_district_60', 'pothole_neighbour_3', 'pothole_neighbour_14', 'pothole_neighbour_60',
	'graffiti_city_3', 'graffiti_city_14', 'graffiti_city_60', 'graffiti_district_3', 'graffiti_district_14', 'graffiti_district_60', 'graffiti_neighbour_3', 'graffiti_neighbour_14', 'graffiti_neighbour_60',
	'traffic_city_3', 'traffic_city_14', 'traffic_city_60', 'traffic_district_3', 'traffic_district_14', 'traffic_district_60', 'traffic_neighbour_3', 'traffic_neighbour_14', 'traffic_neighbour_60',
	'trash_city_3', 'trash_city_14', 'trash_city_60', 'trash_district_3', 'trash_district_14', 'trash_district_60', 'trash_neighbour_3', 'trash_neighbour_14', 'trash_neighbour_60', 
	'NA_city_3', 'NA_city_14', 'NA_city_60', 'NA_district_3', 'NA_district_14', 'NA_district_60', 'NA_neighbour_3', 'NA_neighbour_14', 'NA_neighbour_60', 'Other_city_3', 'Other_city_14', 'Other_city_60',
	'Other_district_3', 'Other_district_14', 'Other_district_60', 'Other_neighbour_3', 'Other_neighbour_14', 'Other_neighbour_60', 'trees_city_3', 'trees_city_14', 'trees_city_60',
	'trees_district_3', 'trees_district_14', 'trees_district_60', 'trees_neighbour_3', 'trees_neighbour_14', 'trees_neighbour_60', 'hydrant_city_3', 'hydrant_city_14', 'hydrant_city_60',
	'hydrant_district_3', 'hydrant_district_14', 'hydrant_district_60', 'hydrant_neighbour_3', 'hydrant_neighbour_14', 'hydrant_neighbour_60', 'total_city_3', 'total_city_14', 'total_city_60',
	'total_district_3', 'total_district_14', 'total_district_60', 'total_neighbour_3', 'total_neighbour_14', 'total_neighbour_60', 'self_city_3', 'self_city_14', 'self_city_60', 
	'self_district_3', 'self_district_14', 'self_district_60', 'self_neighbour_3', 'self_neighbour_14', 'self_neighbour_60')
	FIRST_FEATURE = 6
	ID = 8
	MOIN = 30
	LR = 0.0015
	RESPONSE = 4
	SEED = 53
	BF = 0.90
	TREES = 2200
	descrip = paste('gbm_BC_LOO', 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, sep='_')
	cat(descrip, '\n')
	alldata <- alldata[, FEATURES, with=FALSE]
	##
	## One hot encoding
	##
	unique_keys <- unique(as.character(alldata$source))
	for (i in unique_keys) {
		alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
	}
	unique_keys <- unique(as.character(alldata$tag_group))
	for (i in unique_keys) {
		alldata[,paste('tag_group_',i,sep=''):=as.integer(tag_group==i)]
	}
	alldata[,c('tag_group','source'):=NULL]
	if (HOLD_LENGTH>0){
		alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
		train <- rbind(alldata[set=='train' & day >= TRAIN_BEGIN], alldata[set=='hold'& day >= TRAIN_BEGIN,])
		nTrain <- nrow(train[set=='train',])
	} else {
		train <- alldata[set=='train' & day >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
	}
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	if (! onlypred) {
		train <- as.data.frame(train)
		set.seed(SEED)
		gbm <- gbm.fit(x=train[,FIRST_FEATURE:ncol(train)], y=train[,RESPONSE],
		distribution = 'gaussian',
		n.trees = TREES,
		interaction.depth = ID,
		n.minobsinnode = MOIN,
		shrinkage = LR,
		bag.fraction = BF,
		nTrain = nTrain,
		keep.data = FALSE,
		verbose = TRUE)	
		save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)
		write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
	} else {
		if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
			load(paste(MODEL_PATH, descrip, '.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	options("scipen"=100)
	test <- as.data.frame(alldata[set=='test',])
	pred <-predict(gbm, test[,FIRST_FEATURE:ncol(test)], n.trees=TREES)
	prediction <- data.table(cbind(test$id_, test$id, pmax(expm1(pred),0), test$views, test$votes, test$comments))
	setnames(prediction, c('id_', 'id', 'response', 'views', 'votes', 'comments'))
	views <- prediction[views==1, 1:3, with=FALSE]
	votes <- prediction[votes==1, 1:3, with=FALSE]
	comments <- prediction[comments==1, 1:3, with=FALSE]
	setkey(views,id_)
	setkey(votes,id_)
	setkey(comments,id_)
	sent <- data.table(cbind(votes$id, views$response, votes$response, comments$response))
	setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(sent, file=paste(SUBMISSION_PATH, 'gbm_BC_LOO_train_60_hold_0.csv', sep=''), row.names = FALSE)
}

gbm_by_city_BC_BAYES_n_BoW <- function(onlypred) {
	## 
	## gbm model trained by city with Big Column approach
	## uses LOO (features based in leave one out time and geographic radial basis average) and BAYES (features based in leave one out geographic radial basis average of last 150 days)
	## use binary Bag of Words based in 'summary' feature.
	##
	ID = 10
	MOIN = 25
	LR = 0.0005
	RESPONSE = 4
	SEED = 53
	BF = 0.90
	TEST_BEGIN = 485
	HOLD_LENGTH = 0
	TRAIN_LENGTH = 180
	TRAIN_BEGIN = TEST_BEGIN - TRAIN_LENGTH
	BAYES = TRUE
	FIRST_FEATURE = 7
	cities <- c('New_Haven', 'Oakland', 'Richmond', 'Chicago')
	for (CITY in cities) {
		load(file=paste(DATA_PATH, 'bigdata.RData', sep=''))
		if (CITY=='Chicago') {
			ID = 8
			LR = 0.002
			TREES = 1300
		}
		if (CITY=='Oakland') {
			LR = 0.005
			TRAIN_LENGTH = 1000
			TRAIN_BEGIN = TEST_BEGIN - TRAIN_LENGTH
			TREES = 2300
		}
		if (CITY=='Richmond') {
			TREES = 3500
		}
		if (CITY=='New_Haven') {
			TREES = 8500
		}
		descrip = paste('gbm_BC_BAYES_n_BoW', CITY, 'id', ID, 'lr', LR, 'moin', MOIN, 'bf', BF, 'train', TRAIN_LENGTH, 'hold', HOLD_LENGTH, sep='_')
		FEATURES <- c('id_', 'id', 'set', 'response', 'dayseq', 'city', 'hour', 'week_day', 'summary_len', 'description_len', 
		'votes', 'comments', 'views', 'latitude', 'longitude',
		'tag_type_street_light', 'tag_type_pothole', 'tag_type_graffiti', 'tag_type_traffic', 
		'tag_type_trash', 'tag_type_NA', 'tag_type_signs', 'tag_type_drain_problem', 'tag_type_tree', 'tag_type_sidewalk', 'tag_type_parking_meter', 
		'tag_type_drug_dealing', 'tag_type_blighted_property', 'tag_type_abandoned_vehicle', 'tag_type_bridge', 'tag_type_flood', 'tag_type_overgrowth', 'tag_type_homeless', 
		'tag_type_road_safety', 'tag_type_crosswalk', 'tag_type_bad_driving', 'tag_type_robbery', 'tag_type_test', 'tag_type_illegal_idling', 'tag_type_snow', 
		'tag_type_heat', 'tag_type_hydrant', 'tag_type_roadkill', 'tag_type_animal_problem', 'tag_type_bike_concern', 'tag_type_street_signal', 'tag_type_prostitution', 
		'tag_type_odor', 'tag_type_noise_complaint', 'tag_type_Others', 'tag_type_bench', 'tag_type_pedestrian_light', 'tag_type_rodents', 
		'source_Map_Widget', 'source_NA', 'source_android', 'source_iphone', 'source_remote_api_created', 'source_Mobile_Site', 'source_city_initiated', 
		'source_web', 'abandon', 'across', 'alley', 'alleyway', 'along', 'anim', 	'applianc', 'area', 'around', 'ave', 'avenu', 'back', 'bad', 
		'bag', 'bait', 'behind', 'big', 'bike', 'bin', 'blight', 'blink', 'blinker', 'block', 'blvd', 'box', 'boxspr', 'branch', 
		'brick', 'bridg', 'broad', 'broken', 'brook', 'brush', 'build', 'bulk', 'bump', 'burn', 'burnt', 'bus', 'bush', 'busi', 
		'can', 'car', 'cart', 'cat', 'cavein', 'chair', 'chamberlayn', 'chapel', 'christma', 'church', 'citi', 'clay', 'clean', 'clear', 
		'clog', 'code', 'collect', 'complaint', 'condit', 'construct', 'contain', 'corner', 'couch', 'cover', 'cross', 'crosswalk', 'curb', 'cut', 
		'damag', 'danger', 'day', 'dead', 'debri', 'deep', 'depress', 'deterior', 'dirt', 'discard', 'ditch', 'dog', 'door', 'down', 
		'drain', 'drainag', 'drive', 'driveway', 'drug', 'dump', 'east', 'edgewood', 'electr', 'encamp', 'enforc', 'entranc', 'etc', 'excess', 
		'fall', 'fallen', 'fenc', 'fill', 'fire', 'flood', 'found', 'freeway', 'front', 'full', 'function', 'furnitur', 'gang', 'garag', 
		'garbag', 'glass', 'grace', 'graffiti', 'grand', 'grass', 'gravel', 'green', 'ground', 'grove', 'grow', 'grown', 'gutter', 'hang', 
		'haven', 'hazard', 'help', 'high', 'hill', 'hole', 'home', 'homeless', 'hous', 'household', 'huge', 'hydrant', 'illeg', 'instal', 
		'intersect', 'irrig', 'issu', 'item', 'junk', 'kensington', 'lake', 'lamp', 'landscap', 'lane', 'larg', 'lawn', 'leaf', 'leak', 
		'leav', 'left', 'light', 'limb', 'line', 'litter', 'locat', 'loos', 'lost', 'lot', 'main', 'mainten', 'manhol', 'mattress', 
		'median', 'metal', 'meter', 'middl', 'misc', 'miss', 'month', 'mow', 'multipl', 'near', 'need', 'new', 'next', 'nois',
		'non', 'nonfunct', 'north', 'old', 'one', 'open', 'opposit', 'orang', 'outag', 'overflow', 'overgrown', 'paint', 'park', 'parkwood', 
		'pave', 'pavement', 'pedestrian', 'pick', 'pickup', 'pile', 'place', 'pleas', 'plow', 'pole', 'polic', 'pool', 'poor', 'post', 
		'pot', 'potenti', 'pothol', 'power', 'privat', 'problem', 'properti', 'public', 'quinnipiac', 'rat', 'rear', 'recycl', 'red', 'refus', 
		'remov', 'repair', 'replac', 'report', 'request', 'resid', 'residenti',	'restaur', 'right', 'road', 'roadway', 'rock', 'rodent', 'run', 
		'safeti', 'sanit', 'school', 'seminari', 'sever', 'sewer', 'shop', 'shovel', 'shrub', 'side', 'sidewalk', 'sign', 'signal', 'sink', 
		'sinkhol', 'small', 'snow', 'sofa', 'south', 'space', 'speed', 'spring', 'stand', 'state', 'station', 'still', 'stolen', 'stop', 
		'storm', 'street', 'streetlight', 'strip', 'stump', 'survey', 'sweep', 'tag', 'tall', 'time', 'tire', 'traffic', 'trash', 'trashbulk', 
		'trashfurnitur', 'tree', 'trim', 'truck', 'turn', 'two', 'unsaf', 'upgrad', 'use', 'util', 'vacant', 'veget', 'vehicl', 'view', 
		'violat', 'walk', 'wall', 'wast', 'water', 'way', 'weed', 'week', 'west', 'whalley', 'white', 'window', 'wire', 'wood', 
		'work', 'wrong', 'yard', 'zone', 'total_city_3', 'total_city_14', 'total_city_60', 'total_district_3', 'total_district_14', 'total_district_60', 
		'total_neighbour_3', 'total_neighbour_14', 'total_neighbour_60', 'self_city_3', 'self_city_14', 'self_city_60', 'self_district_3', 'self_district_14', 'self_district_60', 
		'self_neighbour_3', 'self_neighbour_14', 'self_neighbour_60')  
		if (BAYES) {
			FEATURES <- c(FEATURES, 'views_city', 'views_district', 'views_neighbour', 'votes_city', 'votes_district', 'votes_neighbour', 
			'comments_city', 'comments_district', 'comments_neighbour' )
		}
		bigdata <- bigdata[, FEATURES, with=FALSE]
		##
		## delete constant features in this city
		##
		cols_drop <-c()
		bigdata <- bigdata[city==CITY,]
		for (i in 16:ncol(bigdata)){
			if (min(bigdata[[i]], na.rm=TRUE)==max(bigdata[[i]], na.rm=TRUE)) {
				cols_drop <- c(cols_drop, names(bigdata)[i])
				cat(CITY, ' ', names(bigdata)[i], '\n')
			}
		}
		bigdata <- bigdata[, c(cols_drop):=NULL]
		setkey(bigdata, dayseq)
		train <- bigdata[set=='train' & dayseq >= TRAIN_BEGIN,]
		nTrain <- nrow(train)
		##
		## Train or only predict (set the onlypred parameter in train.R)
		##
		if (! onlypred) {
			train <- as.data.frame(train)
			set.seed(SEED)
			gbm <- gbm.fit(x=train[,FIRST_FEATURE:ncol(train)], y=train[,RESPONSE],
			distribution = 'gaussian',
			n.trees = TREES,
			interaction.depth = ID,
			n.minobsinnode = MOIN,
			shrinkage = LR,
			bag.fraction = BF,
			nTrain = nTrain,
			keep.data = FALSE,
			verbose = TRUE)
			save(gbm, file=paste(MODEL_PATH, descrip, '.RData', sep=''), compress = TRUE)		
			write.csv(as.data.frame(summary(gbm, plotit = FALSE)), file=paste(MODEL_PATH, 'summary_', descrip,'.csv',sep = ''), row.names=FALSE)
		} else {
			if (file.exists(paste(MODEL_PATH, descrip, '.RData', sep=''))) {
				load(paste(MODEL_PATH, descrip, '.RData', sep=''))
			} else {
				cat('The model must be trained before new data predictions','\n')
				return (FALSE)
			}
		}
		##
		## Predict
		##
		options("scipen"=100)
		test <- as.data.frame(bigdata[set=='test',])
		pred <-pmax(predict(gbm, test[,FIRST_FEATURE:ncol(test)], n.trees=TREES), 0)
		prediction <- data.table(cbind(test$id_, test$id, expm1(pred), test$views, test$votes, test$comments))
		setnames(prediction, c('id_', 'id', 'response', 'views', 'votes', 'comments'))
		views <- prediction[views==1, 1:3, with=FALSE]
		votes <- prediction[votes==1, 1:3, with=FALSE]
		comments <- prediction[comments==1, 1:3, with=FALSE]
		setkey(views,id_)
		setkey(votes,id_)
		setkey(comments,id_)
		sent <- data.table(cbind(votes$id, views$response, votes$response, comments$response))
		setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
		if (CITY=='New_Haven') {
			sent_all = copy(sent)
		} else {	
			sent_all = rbind(sent_all, sent)
		}	
	}
	write.csv(sent_all, file=paste(SUBMISSION_PATH, 'gbm_by_city_BC_BAYES_n_BoW.csv', sep=''), row.names = FALSE)
}

glm_BAYES_n_BoW <- function(onlypred) {
	## 
	## glm model trained for each response
	## uses LOO (features based in leave one out time and geographic radial basis average) and BAYES (features based in leave one out geographic radial basis average of last 150 days)
	## use binary Bag of Words based in 'summary' feature.
	##
	load(file=paste(DATA_PATH, 'BAYES_BOW_glm_data.RData', sep=''))
	load(paste(DATA_PATH, 'summary_SparseMatrix.RData', sep=''))
	require(glmnet)
	alldata[set=='test', num_votes:=-1]
	alldata[set=='test', num_comments:=-1]
	alldata[set=='test', num_views:=-1]

	fmla_views <- as.formula('num_views ~ city + source + tag_type + summary_len + description_len + 
	hour + week_day + time_left + lights_city_3 + lights_city_14 + lights_city_60 + lights_district_3 +
	lights_district_14 + lights_district_60 + lights_neighbour_3 + lights_neighbour_14 + lights_neighbour_60 + pothole_city_3 +  
	pothole_city_14 + pothole_city_60 + pothole_district_3 + pothole_district_14 + pothole_district_60 + pothole_neighbour_3 +  
	pothole_neighbour_14 + pothole_neighbour_60 + graffiti_city_3 + graffiti_city_14 + graffiti_city_60 + graffiti_district_3 +  
	graffiti_district_14 + graffiti_district_60 + graffiti_neighbour_3 + graffiti_neighbour_14 + graffiti_neighbour_60 + traffic_city_3 +  
	traffic_city_14 + traffic_city_60 + traffic_district_3 + traffic_district_14 + traffic_district_60 + traffic_neighbour_3 +  
	traffic_neighbour_14 + traffic_neighbour_60 + trash_city_3 + trash_city_14 + trash_city_60 + trash_district_3 +  
	trash_district_14 + trash_district_60 + trash_neighbour_3 + trash_neighbour_14 + trash_neighbour_60 + NA_city_3 +  
	NA_city_14 + NA_city_60 + NA_district_3 + NA_district_14 + NA_district_60 + NA_neighbour_3 +  
	NA_neighbour_14 + NA_neighbour_60 + rain_snow_city_3 + rain_snow_city_14 + rain_snow_city_60 + rain_snow_district_3 +  
	rain_snow_district_14 + rain_snow_district_60 + rain_snow_neighbour_3 + rain_snow_neighbour_14 + rain_snow_neighbour_60 + trees_city_3 +  
	trees_city_14 + trees_city_60 + trees_district_3 + trees_district_14 + trees_district_60 + trees_neighbour_3 +  
	trees_neighbour_14 + trees_neighbour_60 + Other_city_3 + Other_city_14 + Other_city_60 + Other_district_3 +  
	Other_district_14 + Other_district_60 + Other_neighbour_3 + Other_neighbour_14 + Other_neighbour_60 + crime_n_social_city_3 +  
	crime_n_social_city_14 + crime_n_social_city_60 + crime_n_social_district_3 + crime_n_social_district_14 + crime_n_social_district_60 + crime_n_social_neighbour_3 +  
	crime_n_social_neighbour_14 + crime_n_social_neighbour_60 + hydrant_city_3 + hydrant_city_14 + hydrant_city_60 + hydrant_district_3 +  
	hydrant_district_14 + hydrant_district_60 + hydrant_neighbour_3 + hydrant_neighbour_14 + hydrant_neighbour_60 + total_city_3 +  
	total_city_14 + total_city_60 + total_district_3 + total_district_14 + total_district_60 + total_neighbour_3 +  
	total_neighbour_14 + total_neighbour_60 + self_city_3 + self_city_14 + self_city_60 + self_district_3 +  
	self_district_14 + self_district_60 + self_neighbour_3 + self_neighbour_14 + self_neighbour_60 + views_city +  
	views_district + views_neighbour + votes_city + votes_district + votes_neighbour + comments_city +  
	comments_district + comments_neighbour + self_views_city + self_views_district + self_views_neighbour + self_votes_city +  
	self_votes_district + self_votes_neighbour + self_comments_city + self_comments_district + self_comments_neighbour')

	fmla_votes <- as.formula('num_votes ~ city + source + tag_type + summary_len + description_len + 
	hour + week_day + time_left + lights_city_3 + lights_city_14 + lights_city_60 + lights_district_3 +
	lights_district_14 + lights_district_60 + lights_neighbour_3 + lights_neighbour_14 + lights_neighbour_60 + pothole_city_3 +  
	pothole_city_14 + pothole_city_60 + pothole_district_3 + pothole_district_14 + pothole_district_60 + pothole_neighbour_3 +  
	pothole_neighbour_14 + pothole_neighbour_60 + graffiti_city_3 + graffiti_city_14 + graffiti_city_60 + graffiti_district_3 +  
	graffiti_district_14 + graffiti_district_60 + graffiti_neighbour_3 + graffiti_neighbour_14 + graffiti_neighbour_60 + traffic_city_3 +  
	traffic_city_14 + traffic_city_60 + traffic_district_3 + traffic_district_14 + traffic_district_60 + traffic_neighbour_3 +  
	traffic_neighbour_14 + traffic_neighbour_60 + trash_city_3 + trash_city_14 + trash_city_60 + trash_district_3 +  
	trash_district_14 + trash_district_60 + trash_neighbour_3 + trash_neighbour_14 + trash_neighbour_60 + NA_city_3 +  
	NA_city_14 + NA_city_60 + NA_district_3 + NA_district_14 + NA_district_60 + NA_neighbour_3 +  
	NA_neighbour_14 + NA_neighbour_60 + rain_snow_city_3 + rain_snow_city_14 + rain_snow_city_60 + rain_snow_district_3 +  
	rain_snow_district_14 + rain_snow_district_60 + rain_snow_neighbour_3 + rain_snow_neighbour_14 + rain_snow_neighbour_60 + trees_city_3 +  
	trees_city_14 + trees_city_60 + trees_district_3 + trees_district_14 + trees_district_60 + trees_neighbour_3 +  
	trees_neighbour_14 + trees_neighbour_60 + Other_city_3 + Other_city_14 + Other_city_60 + Other_district_3 +  
	Other_district_14 + Other_district_60 + Other_neighbour_3 + Other_neighbour_14 + Other_neighbour_60 + crime_n_social_city_3 +  
	crime_n_social_city_14 + crime_n_social_city_60 + crime_n_social_district_3 + crime_n_social_district_14 + crime_n_social_district_60 + crime_n_social_neighbour_3 +  
	crime_n_social_neighbour_14 + crime_n_social_neighbour_60 + hydrant_city_3 + hydrant_city_14 + hydrant_city_60 + hydrant_district_3 +  
	hydrant_district_14 + hydrant_district_60 + hydrant_neighbour_3 + hydrant_neighbour_14 + hydrant_neighbour_60 + total_city_3 +  
	total_city_14 + total_city_60 + total_district_3 + total_district_14 + total_district_60 + total_neighbour_3 +  
	total_neighbour_14 + total_neighbour_60 + self_city_3 + self_city_14 + self_city_60 + self_district_3 +  
	self_district_14 + self_district_60 + self_neighbour_3 + self_neighbour_14 + self_neighbour_60 + views_city +  
	views_district + views_neighbour + votes_city + votes_district + votes_neighbour + comments_city +  
	comments_district + comments_neighbour + self_views_city + self_views_district + self_views_neighbour + self_votes_city +  
	self_votes_district + self_votes_neighbour + self_comments_city + self_comments_district + self_comments_neighbour')

	fmla_comments <- as.formula('num_comments ~ city + source + tag_type + summary_len + description_len + 
	hour + week_day + time_left + lights_city_3 + lights_city_14 + lights_city_60 + lights_district_3 +
	lights_district_14 + lights_district_60 + lights_neighbour_3 + lights_neighbour_14 + lights_neighbour_60 + pothole_city_3 +  
	pothole_city_14 + pothole_city_60 + pothole_district_3 + pothole_district_14 + pothole_district_60 + pothole_neighbour_3 +  
	pothole_neighbour_14 + pothole_neighbour_60 + graffiti_city_3 + graffiti_city_14 + graffiti_city_60 + graffiti_district_3 +  
	graffiti_district_14 + graffiti_district_60 + graffiti_neighbour_3 + graffiti_neighbour_14 + graffiti_neighbour_60 + traffic_city_3 +  
	traffic_city_14 + traffic_city_60 + traffic_district_3 + traffic_district_14 + traffic_district_60 + traffic_neighbour_3 +  
	traffic_neighbour_14 + traffic_neighbour_60 + trash_city_3 + trash_city_14 + trash_city_60 + trash_district_3 +  
	trash_district_14 + trash_district_60 + trash_neighbour_3 + trash_neighbour_14 + trash_neighbour_60 + NA_city_3 +  
	NA_city_14 + NA_city_60 + NA_district_3 + NA_district_14 + NA_district_60 + NA_neighbour_3 +  
	NA_neighbour_14 + NA_neighbour_60 + rain_snow_city_3 + rain_snow_city_14 + rain_snow_city_60 + rain_snow_district_3 +  
	rain_snow_district_14 + rain_snow_district_60 + rain_snow_neighbour_3 + rain_snow_neighbour_14 + rain_snow_neighbour_60 + trees_city_3 +  
	trees_city_14 + trees_city_60 + trees_district_3 + trees_district_14 + trees_district_60 + trees_neighbour_3 +  
	trees_neighbour_14 + trees_neighbour_60 + Other_city_3 + Other_city_14 + Other_city_60 + Other_district_3 +  
	Other_district_14 + Other_district_60 + Other_neighbour_3 + Other_neighbour_14 + Other_neighbour_60 + crime_n_social_city_3 +  
	crime_n_social_city_14 + crime_n_social_city_60 + crime_n_social_district_3 + crime_n_social_district_14 + crime_n_social_district_60 + crime_n_social_neighbour_3 +  
	crime_n_social_neighbour_14 + crime_n_social_neighbour_60 + hydrant_city_3 + hydrant_city_14 + hydrant_city_60 + hydrant_district_3 +  
	hydrant_district_14 + hydrant_district_60 + hydrant_neighbour_3 + hydrant_neighbour_14 + hydrant_neighbour_60 + total_city_3 +  
	total_city_14 + total_city_60 + total_district_3 + total_district_14 + total_district_60 + total_neighbour_3 +  
	total_neighbour_14 + total_neighbour_60 + self_city_3 + self_city_14 + self_city_60 + self_district_3 +  
	self_district_14 + self_district_60 + self_neighbour_3 + self_neighbour_14 + self_neighbour_60 + views_city +  
	views_district + views_neighbour + votes_city + votes_district + votes_neighbour + comments_city +  
	comments_district + comments_neighbour + self_views_city + self_views_district + self_views_neighbour + self_votes_city +  
	self_votes_district + self_votes_neighbour + self_comments_city + self_comments_district + self_comments_neighbour')

	rowTrain <- which(alldata$set=='train')
	train<-alldata[set=='train',]
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	M_views <- sparse.model.matrix(fmla_views, data=alldata, row.names = FALSE, verbose = TRUE)
	M_views_2 <- cBind(M_views, summary_SparseMatrix)
	if (! onlypred) {
		lm_views_2 <- cv.glmnet(x=M_views_2[rowTrain,], y=train$num_views , family="gaussian", nfolds=5, alpha = 0, lambda = seq(0.001,0.005,0.001), standardize = TRUE)
		save(lm_views_2, file=paste(MODEL_PATH, 'glm_BAYES_n_BoW_views.RData', sep=''), compress = TRUE)
	} else {
		if (file.exists(paste(MODEL_PATH, 'glm_BAYES_n_BoW_views.RData', sep=''))) {
			load(paste(MODEL_PATH, 'glm_BAYES_n_BoW_views.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	lm_views_pred <- expm1(pmax(pmin(predict(lm_views_2, M_views_2[-rowTrain,], s=0.001),6),0))
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	M_votes <- sparse.model.matrix(fmla_votes, data=alldata, row.names = FALSE, verbose = TRUE)
	M_votes_2 <- cBind(M_votes, summary_SparseMatrix)
	if (! onlypred) {
		lm_votes_2 <- cv.glmnet(x=M_votes_2[rowTrain,], y=train$num_votes , family="gaussian", nfolds=5, alpha = 0, lambda = seq(0.001,0.005,0.001), standardize = TRUE)
		save(lm_votes_2, file=paste(MODEL_PATH, 'glm_BAYES_n_BoW_votes.RData', sep=''), compress = TRUE)
	} else {
		if (file.exists(paste(MODEL_PATH, 'glm_BAYES_n_BoW_votes.RData', sep=''))) {
			load(paste(MODEL_PATH, 'glm_BAYES_n_BoW_votes.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	lm_votes_pred <- expm1(pmax(pmin(predict(lm_votes_2, M_votes_2[-rowTrain,], s=0.001),5),0))
	##
	## Train or only predict (set the onlypred parameter in train.R)
	##
	M_comments <- sparse.model.matrix(fmla_comments, data=alldata, row.names = FALSE, verbose = TRUE)
	M_comments_2 <- cBind(M_comments, summary_SparseMatrix)
	if (! onlypred) {
		lm_comments_2 <- cv.glmnet(x=M_comments_2[rowTrain,], y=train$num_comments , family="gaussian", nfolds=5, alpha = 0, lambda = seq(0.001,0.005,0.001), standardize = TRUE)
		save(lm_comments_2, file=paste(MODEL_PATH, 'glm_BAYES_n_BoW_comments.RData', sep=''), compress = TRUE)
	} else {
		if (file.exists(paste(MODEL_PATH, 'glm_BAYES_n_BoW_comments.RData', sep=''))) {
			load(paste(MODEL_PATH, 'glm_BAYES_n_BoW_comments.RData', sep=''))
		} else {
			cat('The model must be trained before new data predictions','\n')
			return (FALSE)
		}
	}
	##
	## Predict
	##
	lm_comments_pred <- expm1(pmax(pmin(predict(lm_comments_2, M_comments_2[-rowTrain,], s=0.001),4),0))
	options("scipen"=100)
	sent <- data.table(cbind(alldata[set=='test', id], lm_views_pred, lm_votes_pred, lm_comments_pred))
	setnames(sent, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(sent, file=paste(SUBMISSION_PATH, 'glm_BAYES_n_BoW.csv', sep=''), row.names = FALSE)
}

naive <- function() {
	##
	## Mean day >= 295 by city
	##
	load(file=paste(DATA_PATH,'alldata_basic.RData',sep=''))
	setkey(alldata, city)
	mean_by_cities <- alldata[set!='test' & day >= 295, list('mean_num_votes' = mean(num_votes), 'mean_num_comments' = mean(num_comments), 'mean_num_views' = mean(num_views)), by = city]
	setkey(mean_by_cities, city)
	alldata <- mean_by_cities[alldata]
	pred_Richmond <- alldata[set=='test',list(city, id, mean_num_views, mean_num_votes, mean_num_comments)]
	pred_Richmond[city!='Richmond',mean_num_views:=0]
	pred_Richmond[city!='Richmond',mean_num_votes:=0]
	pred_Richmond[city!='Richmond',mean_num_comments:=0]
	pred_Richmond[,'city':=NULL]
	setnames(pred_Richmond,c('id','num_views','num_votes','num_comments'))
	write.csv(pred_Richmond, file=paste(SUBMISSION_PATH, 'mean_Richmond_295.csv', sep=''), row.names = FALSE)

	pred_New_Haven <- alldata[set=='test',list(city, id, mean_num_views, mean_num_votes, mean_num_comments)]
	pred_New_Haven[city!='New_Haven',mean_num_views:=0]
	pred_New_Haven[city!='New_Haven',mean_num_votes:=0]
	pred_New_Haven[city!='New_Haven',mean_num_comments:=0]
	pred_New_Haven[,'city':=NULL]
	setnames(pred_New_Haven,c('id','num_views','num_votes','num_comments'))
	write.csv(pred_New_Haven, file=paste(SUBMISSION_PATH, 'mean_New_Haven_295.csv', sep=''), row.names = FALSE)

	pred_Oakland <- alldata[set=='test',list(city, id, mean_num_views, mean_num_votes, mean_num_comments)]
	pred_Oakland[city!='Oakland',mean_num_views:=0]
	pred_Oakland[city!='Oakland',mean_num_votes:=0]
	pred_Oakland[city!='Oakland',mean_num_comments:=0]
	pred_Oakland[,'city':=NULL]
	setnames(pred_Oakland,c('id','num_views','num_votes','num_comments'))
	write.csv(pred_Oakland, file=paste(SUBMISSION_PATH, 'mean_Oakland_295.csv', sep=''), row.names = FALSE)

	pred_Chicago <- alldata[set=='test',list(city, id, mean_num_views, mean_num_votes, mean_num_comments)]
	pred_Chicago[city!='Chicago',mean_num_views:=0]
	pred_Chicago[city!='Chicago',mean_num_votes:=0]
	pred_Chicago[city!='Chicago',mean_num_comments:=0]
	pred_Chicago[,'city':=NULL]
	setnames(pred_Chicago,c('id','num_views','num_votes','num_comments'))
	write.csv(pred_Chicago, file=paste(SUBMISSION_PATH, 'mean_Chicago_295.csv', sep=''), row.names = FALSE)
	##
	## Mean of day >= 60 for each response
	##
	mean_by_response <- alldata[set!='test' & day >= 60, list('mean_num_votes' = mean(num_votes), 'mean_num_comments' = mean(num_comments), 'mean_num_views' = mean(num_views))]
	mean_by_response[,set:='test']
	test <- alldata[set=='test',]
	setkey(mean_by_response, set)
	setkey(test, set)
	test <- mean_by_response[test]
	test <- test[,list(id, mean_num_views, mean_num_votes, mean_num_comments)]
	setkey(test, id)
	setnames(test,c('id','num_views','num_votes','num_comments'))
	test1 = copy(test)
	test2 = copy(test)
	test3 = copy(test)
	test1[,num_votes:=0]
	test1[,num_comments:=0]
	test2[,num_views:=0]
	test2[,num_comments:=0]
	test3[,num_votes:=0]
	test3[,num_views:=0]
	write.csv(test1, file=paste(SUBMISSION_PATH, 'mean_views_60.csv', sep=''), row.names=FALSE)
	write.csv(test2, file=paste(SUBMISSION_PATH, 'mean_votes_60.csv', sep=''), row.names=FALSE)
	write.csv(test3, file=paste(SUBMISSION_PATH, 'mean_comments_60.csv', sep=''), row.names=FALSE)
	##
	## Mean by city day >= 180 
	##
	setkey(alldata, city)
	mean_by_cities <- alldata[set!='test' & day >= 180, list('mean_num_votes' = mean(num_votes), 'mean_num_comments' = mean(num_comments), 'mean_num_views' = mean(num_views)), by = city]
	setkey(mean_by_cities, city)
	alldata <- mean_by_cities[alldata]
	alldata[ ,mean_num_votes:=expm1(mean_num_votes)]
	alldata[ ,mean_num_comments:=expm1(mean_num_comments)]
	alldata[ ,mean_num_views:=expm1(mean_num_views)]
	pred <- alldata[set=='test',list(id, mean_num_views, mean_num_votes, mean_num_comments)]
	setnames(pred,c('id','num_views','num_votes','num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'mean_by_city_180.csv', sep=''), row.names=FALSE)
	##
	## Dummies
	##
	pred <- alldata[set=='test',list(id, city)]
	pred[, num_views:=0.]
	pred[, num_votes:=0.]
	pred[, num_comments:=0.]
	pred[city=='Oakland', num_views:=1.]
	pred[city=='Oakland', num_votes:=1.]
	pred[city=='Oakland', num_comments:=1.]
	pred <- pred[, list(id, num_views, num_votes, num_comments)]
	setnames(pred, c('id','num_views','num_votes','num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'Oakland_bin.csv', sep=''), row.names=FALSE)

	pred <- alldata[set=='test',list(id, city)]
	pred[, num_views:=0.]
	pred[, num_votes:=0.]
	pred[, num_comments:=0.]
	pred[city=='New_Haven', num_views:=1.]
	pred[city=='New_Haven', num_votes:=1.]
	pred[city=='New_Haven', num_comments:=1.]
	pred <- pred[, list(id, num_views, num_votes, num_comments)]
	setnames(pred, c('id', 'num_views', 'num_votes', 'num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'New_Haven_bin.csv', sep=''), row.names=FALSE)

	pred <- alldata[set=='test',list(id, city)]
	pred[, num_views:=0.]
	pred[, num_votes:=0.]
	pred[, num_comments:=0.]
	pred[city=='Richmond', num_views:=1.]
	pred[city=='Richmond', num_votes:=1.]
	pred[city=='Richmond', num_comments:=1.]
	pred <- pred[, list(id, num_views, num_votes, num_comments)]
	setnames(pred, c('id', 'num_views', 'num_votes', 'num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'Richmond_bin.csv', sep=''), row.names=FALSE)

	pred <- alldata[set=='test',list(id, source)]
	pred[, num_views:=0.]
	pred[, num_votes:=0.]
	pred[, num_comments:=0.]
	pred[source=='remote_api_created', num_views:=1.]
	pred[source=='remote_api_created', num_votes:=1.]
	pred[source=='remote_api_created', num_comments:=1.]
	pred <- pred[, list(id, num_views, num_votes, num_comments)]
	setnames(pred, c('id', 'num_views', 'num_votes', 'num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'Remote_api_created_bin.csv', sep=''), row.names=FALSE)

	pred <- alldata[set=='test',list(id)]
	pred[, num_views:=1.]
	pred[, num_votes:=1.]
	pred[, num_comments:=1.]
	pred <- pred[, list(id, num_views, num_votes, num_comments)]
	setnames(pred, c('id', 'num_views', 'num_votes', 'num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'Constant_1.csv', sep=''), row.names=FALSE)

	pred <- alldata[set=='test',list(id)]
	pred[, num_views:=1.]
	pred[, num_votes:=0.]
	pred[, num_comments:=0.]
	pred <- pred[, list(id, num_views, num_votes, num_comments)]
	setnames(pred, c('id', 'num_views', 'num_votes', 'num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'Views_bin.csv', sep=''), row.names=FALSE)

	pred <- alldata[set=='test',list(id)]
	pred[, num_views:=0.]
	pred[, num_votes:=1.]
	pred[, num_comments:=0.]
	pred <- pred[, list(id, num_views, num_votes, num_comments)]
	setnames(pred, c('id', 'num_views', 'num_votes', 'num_comments'))
	write.csv(pred, file=paste(SUBMISSION_PATH, 'Votes_bin.csv', sep=''), row.names=FALSE)	
}

blending <- function() {
	##
	## Ridge blending based in BigChaos's team method of Netflix Prize
	##
	lb <- c(0.33199, 0.30792, 0.65245, 0.31381, 0.31803, 0.31259, 0.30497, 0.33984, 0.31775, 0.33264, 0.45272, 0.63216, 0.72227, 0.68518, 0.67258,
		0.65266, 0.70035, 0.56259, 0.69672, 0.68451, 0.77329, 0.31707, 0.67213, 0.70093, 0.55531)
	model <- 1	
	for (fi in list.files(paste(SUBMISSION_PATH))) {
		cat(model, '  ', fi, '\n')
		if (model==1) {
			auxiliar <- data.table(read.csv(paste(SUBMISSION_PATH, fi, sep=''), header = TRUE))
			auxiliar <- auxiliar[, views:=0L]
			auxiliar <- auxiliar[, votes:=0L]
			auxiliar <- auxiliar[, comments:=0L]
			setkey(auxiliar, id)
			views <- auxiliar[, list(views, votes, comments, id, num_views)]
			setnames(views,c('views', 'votes', 'comments', 'id', fi))
			views[,views:=1L]
			votes <- auxiliar[, list(views, votes, comments, id, num_votes)]
			setnames(votes,c('views', 'votes', 'comments', 'id', fi))
			votes[, votes:=1L]
			comments <- auxiliar[, list(views, votes, comments, id, num_comments)]
			setnames(comments,c('views', 'votes', 'comments', 'id', fi))
			comments[, comments:=1L]
			ridge <- rbind(views, votes, comments)
		} else {
			auxiliar<-data.table(read.csv(paste(SUBMISSION_PATH, fi, sep=''), header = TRUE))
			setkey(auxiliar, id)
			views <- auxiliar[, list(as.numeric(num_views))]
			setnames(views,c(fi))
			votes <- auxiliar[, list(as.numeric(num_votes))]
			setnames(votes,c(fi))
			comments <- auxiliar[, list(as.numeric(num_comments))]
			setnames(comments,c(fi))
			auxiliar <- rbind(views, votes, comments)
			ridge <- cbind(ridge, auxiliar)
		}
		model <- model + 1
	}
	lb <- as.matrix(lb)
	ridge_ <- ridge[,5:ncol(ridge), with=FALSE]
	ridge_ <-as.data.frame(ridge_)
	nummodels <- nrow(lb)
	##
	## Transform to log scale
	##
	for (i in 1:nummodels) {ridge_[,i] <- log1p(ridge_[,i])}
	N <- 448725
	score0 <- 0.521832864
	XtY<-matrix(0, nrow = nummodels, ncol = 1)
	for (i in 1:nummodels) {
		XtY[i,1] <- as.matrix(((N*score0) + sum(ridge_[,i]**2) - ((lb[i]**2)*N))/2)
	}
	X <- as.matrix(ridge_)
	options("scipen"=100)
	betas <- solve((t(X) %*% X) + 0.0005*N*diag(nummodels)) %*% XtY
	betas
	betas<-as.matrix(betas)
	blendmodel<- (X %*% betas)
	pred <- pmax(expm1(blendmodel),0)
	sent <- cbind(ridge[,1:4,with=FALSE], pred)
	views <- sent[views==1, 4:5, with=FALSE]
	setnames(views, c('id', 'pred'))
	votes <- sent[votes==1, 4:5, with=FALSE]
	setnames(votes, c('id', 'pred'))
	comments <- sent[comments==1, 4:5, with=FALSE]
	setnames(comments, c('id', 'pred'))
	setkey(views,id)
	setkey(votes,id)
	setkey(comments,id)
	prediction <- data.table(cbind(votes$id, views$pred, votes$pred, comments$pred))
	setnames(prediction, c('id','num_views', 'num_votes', 'num_comments'))
	write.csv(prediction, file=paste(MAIN_PATH, 'blending.csv', sep=''), row.names = FALSE, quote=FALSE)
}

