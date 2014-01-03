##
## The file can be run at once or step by step (determined by intermediate save())
##
rm(list = ls(all = TRUE)) 
require('data.table')
MAIN_PATH = 's://SeeClickFix/'
DATA_PATH = 'data/'
MODEL_PATH = 'model/'
PRED_PATH = 'pred/'
setwd(MAIN_PATH)

train <- data.table(read.csv(paste(DATA_PATH,'train.csv',sep=''), header = TRUE, stringsAsFactors = FALSE, na.strings = "NULL"))
test <- data.table(read.csv(paste(DATA_PATH,'test.csv',sep=''), header = TRUE, stringsAsFactors = FALSE, na.strings = "NULL"))
train[, set := 'train']
test[, set := 'test']
test[, num_votes := as.integer(NA)]
test[, num_comments := as.integer(NA)]
test[, num_views := as.integer(NA)]
alldata <-rbind(train, test, use.names = TRUE)
rm(train)
rm(test)
alldata[, id_:=seq(1,nrow(alldata))]
##
## Alldata basic data set
##
alldata[, created_time := as.POSIXct(created_time, format = "%Y-%m-%d %H:%M:%S")]
min_time <- min(alldata[set=='train', created_time], na.rm = TRUE)
alldata[set=='train' & is.na(created_time),created_time := min_time]
min_time <- min(alldata[set=='test', created_time], na.rm = TRUE)
alldata[set=='test' & is.na(created_time), created_time := min_time]
##
## TIME & DATE
##
## Day begins at 6:00 AM
##
alldata[, hour := (hour(created_time) + 18) %% 24]
alldata[, year_day := yday(created_time) %% 365]
##
## Week begins on Monday
alldata[, week_day := (wday(created_time) + 5) %% 7]
alldata[, month_day := mday(created_time)]
alldata[, day := as.integer(as.numeric(created_time, format(created_time, "%Y%m%d"))/3600/24) ]
alldata[, day:=day-min(alldata$day)]
alldata[, month:=month(created_time)]
alldata[, year:=year(created_time)]
##
## City
##
alldata[longitude < -100, city := 'Oakland']
alldata[-90 < longitude & longitude < -80, city := 'Chicago']
alldata[-80 < longitude & longitude < -75, city := 'Richmond']
alldata[city=='New Haven', city := 'New_Haven']
alldata[is.na(city), city := 'New_Haven']
##
## Text length
alldata[, summary_len:=nchar(summary)]
alldata[, description_len:=nchar(description)]
table(alldata[year==2013, list(source,month,city)])
table(alldata[year==2012, list(source,month,city)])
##
## Factors
##
alldata[, city:=as.factor(city)]
alldata[tag_type=='abandoned_vehicles', tag_type:= 'abandoned_vehicle']
alldata[source=='Mobile Site', source:='Mobile_Site']
alldata[source=='New Map Widget', source:='Map_Widget']
alldata[source=='Map Widget', source:='Map_Widget']
alldata[, source:=as.factor(source)]
##
## Grouping tags
##
alldata[tag_type %in% c('drug_dealing', 'homeless', 'robbery', 'prostitution'), tag_group:='crime_n_social']
alldata[tag_type %in% c('flood', 'drain_problem', 'snow'), tag_group:='rain_snow']
alldata[tag_type %in% c('crosswalk', 'bad_driving', 'bike_concern', 'roadkill', 'road_saffety', 'street_signal', 'traffic', 'signs'), tag_group:='traffic']
alldata[tag_type %in% c('pedestrian_light', 'street_light'), tag_group:='lights']
alldata[tag_type %in% c('tree', 'overgrowth'), tag_group:='trees']
alldata[tag_type %in% c('trash'), tag_group:='trash']
alldata[tag_type=='hydrant', tag_group:='hydrant']
alldata[tag_type=='graffiti', tag_group:='graffiti']
alldata[tag_type=='pothole', tag_group:='pothole']
alldata[tag_type=='NA', tag_group:='NA']
alldata[is.na(tag_group), tag_group:='Other']
table(alldata$tag_group)
##
## response transform
##
alldata[, num_votes:=log1p(num_votes)]
alldata[, num_comments:=log1p(num_comments)]
alldata[, num_views:=log1p(num_views)]

save(alldata,file=paste(DATA_PATH,'alldata_basic.RData',sep=''))

################################################################################################################
################################################################################################################

##
## BAYES and LOO features based in leave one out radial basis average
##
require('data.table')
MAIN_PATH = 's://SeeClickFix/'
DATA_PATH = 'data/'
MODEL_PATH = 'model/'
PRED_PATH = 'pred/'
setwd(MAIN_PATH)
load('data/alldata_basic.RData')
options("scipen"=30)
HUGE_VAL = 1000000
NOZERO = 0.00000001
TEST_BEGIN = 485
HOLD_LENGTH = 0
TRAIN_LENGTH = HUGE_VAL
TRAIN_BEGIN = TEST_BEGIN - HOLD_LENGTH - TRAIN_LENGTH
SHORT = 3
MIDDLE = 14
LONG = 60
EARTH_RADIUS = 6371
DAY_SECONDS = 60 * 60 * 24
EPSILON_CITY = 0.1
EPSILON_DISTRICT = 0.4
EPSILON_NEIGHBOUR = 1.2
FORWARD_WINDOW = 7
BAYES_WINDOW = 150
BAYES_BEGIN = TEST_BEGIN - HOLD_LENGTH - BAYES_WINDOW

mycolnames <- c('id_','time_left')
for (tag in c(unique(alldata$tag_group),'total','self')) {
	for (ra in c('city', 'district', 'neighbour')) {
		for (pe in c(SHORT, MIDDLE, LONG)) {
			mycolnames <- c(mycolnames, paste(tag, ra, pe, sep='_'))
		}
	}
}
for (resp in c('views', 'votes', 'comments','self_views', 'self_votes', 'self_comments')) {
	for (ra in c('city', 'district', 'neighbour')) {
			mycolnames <- c(mycolnames, paste(resp, ra, sep='_'))
	}
}
period <- c(SHORT, MIDDLE, LONG)
cities <- c('New_Haven', 'Oakland', 'Richmond', 'Chicago')
radius <- c(EPSILON_CITY, EPSILON_DISTRICT, EPSILON_NEIGHBOUR)
w_by_tag <- NULL
if (HOLD_LENGTH > 0) {
	alldata[set=='train' & day >= TEST_BEGIN - HOLD_LENGTH, set:='hold']
	hold_end = alldata[set=='hold', max(day, na.rm = TRUE)]
}
alldata <- alldata[day >= TRAIN_BEGIN,]
alldata[, longitude:=longitude * pi/180]
alldata[, latitude:=latitude * pi/180]
alldata[, distance_w:=-1.0]
test_end = alldata[set=='test', max(day, na.rm = TRUE)]
train_end = alldata[set=='train', max(day, na.rm = TRUE)]
alldata[set=='hold', my_set_end:=hold_end]
alldata[set=='test', my_set_end:=test_end]
alldata[set=='train', my_set_end:=train_end]

for (ci in cities) {
	count <- 0
	alldata_id_ = alldata[,list(id_)]
	for (col in mycolnames[-1]) {alldata_id_[,paste(col,"",sep=""):=as.numeric(0)]}
	alldata_id_[,time_left:=-9999]
	setkey(alldata_id_,id_)
	alldata_city = alldata[city==ci,]
	alldata_city[,distance_w:=-1.0]
	setkey(alldata_city, id_)
	z1 <- unclass(Sys.time())
	for (i in 1:nrow(alldata_city)) {
		my_id_ <- alldata_city[i, id_]
		count <- count + 1
		if (count %% 1000==0) {
			cat(ci, ' ', count, ' / ', nrow(alldata_city), ' elapsed time: ', round((unclass(Sys.time()) - z1) / 60, 2), ' minutes', "\n")
		}
		my_time <- alldata_city[i, created_time]
		my_tag_group <- alldata_city[i, tag_group]
		my_latitude <- alldata_city[i, latitude]
		my_longitude <- alldata_city[i, longitude]
		my_day_left <- alldata_city[i, my_set_end - day]
		##
		## LOO (features based in leave one out time and geographic radial basis average)
		##
		for (pe in period) {
			for (ra in radius) {
				if (ra==EPSILON_CITY) {radesc <- 'city'
				} else if (ra==EPSILON_DISTRICT) {radesc <- 'district'
				} else if (ra==EPSILON_NEIGHBOUR) {radesc <- 'neighbour'}
				alldata_city[,distance_w:=-1.0]
				alldata_city[(created_time > (my_time - pe * DAY_SECONDS)) & (created_time <= my_time + min(FORWARD_WINDOW, pe) * DAY_SECONDS) & id_!=my_id_ & (day <= my_set_end), 
					distance_w:= exp(-ra^2 * EARTH_RADIUS^2 * ((longitude-my_longitude)^2+(latitude-my_latitude)^2))]
				my_time_left = min(my_day_left, min(FORWARD_WINDOW, pe))
				w_by_tag <- alldata_city[distance_w > 0, list('radius' = radesc, 'period' = pe, 'id_' = my_id_, 'distance_w' = sum(distance_w)), by=tag_group]
				w_by_tag_sum <- w_by_tag[, list('tag_group' = 'total', 'radius' = radesc[1], 'period' = pe[1], 'id_' = my_id_[1], 'distance_w' = sum(distance_w))]
				##
				## self tag_group
				##
				alldata_city[,distance_w:=-1.0]
				alldata_city[(created_time > (my_time - pe * DAY_SECONDS)) & (created_time <= my_time + min(FORWARD_WINDOW, pe) * DAY_SECONDS) & id_!=my_id_ & (day <= my_set_end) & tag_group==my_tag_group, 
					distance_w:= exp(-ra^2 * EARTH_RADIUS^2 * ((longitude-my_longitude)^2+(latitude-my_latitude)^2))]
				w_by_self_tag_sum <- alldata_city[distance_w > 0, list('tag_group' = 'self', 'radius' = radesc[1], 'period' = pe[1], 'id_' = my_id_[1], 'distance_w' = sum(distance_w))]					
				w_by_tag <- rbind(w_by_tag, w_by_tag_sum, w_by_self_tag_sum)
				set(alldata_id_, my_id_, 'time_left', my_time_left)
				for (r in 1:nrow(w_by_tag)){
					set(alldata_id_, my_id_, paste(w_by_tag$tag_group[r], w_by_tag$radius[r], w_by_tag$period[r], sep='_'), w_by_tag$distance_w[r])
				}
				##
				## BAYES (features based in leave one out geographic radial basis average of last 150 days)
				##
				if (pe==period[1]) {
					alldata_city[,distance_w:=-1.0]
					alldata_city[set=='train' & id_!=my_id_ & day >= BAYES_BEGIN, distance_w:= exp(-ra^2 * EARTH_RADIUS^2 * ((longitude-my_longitude)^2+(latitude-my_latitude)^2))]
					w_response <- alldata_city[distance_w > 0, list('id_' = my_id_, 'views_w' = sum(distance_w * num_views) / sum(distance_w), 
					'votes_w' = sum(distance_w * num_votes) / sum(distance_w), 'comments_w' = sum(distance_w * num_comments) / sum(distance_w))]
					set(alldata_id_, my_id_, paste('views', radesc, sep='_'), w_response$views_w[1])
					set(alldata_id_, my_id_, paste('votes', radesc, sep='_'), w_response$votes_w[1])
					set(alldata_id_, my_id_, paste('comments', radesc, sep='_'), w_response$comments_w[1])
					alldata_city[,distance_w:=-1.0]
					alldata_city[set=='train' & id_!=my_id_ & tag_group==my_tag_group & day >= BAYES_BEGIN, distance_w:= exp(-ra^2 * EARTH_RADIUS^2 * ((longitude-my_longitude)^2+(latitude-my_latitude)^2))]
					w_response <- alldata_city[distance_w > 0, list('id_' = my_id_, 'views_w' = sum(distance_w * num_views) / sum(distance_w), 
					'votes_w' = sum(distance_w * num_votes) / sum(distance_w), 'comments_w' = sum(distance_w * num_comments) / sum(distance_w))]
					set(alldata_id_, my_id_, paste('self_views', radesc, sep='_'), w_response$views_w[1])
					set(alldata_id_, my_id_, paste('self_votes', radesc, sep='_'), w_response$votes_w[1])
					set(alldata_id_, my_id_, paste('self_comments', radesc, sep='_'), w_response$comments_w[1])
				}
			}
		}
	}
	cat(" elapsed time: ", round((unclass(Sys.time()) - z1) / 60, 2), " minutes", "\n")
	alldata_id_ok <- alldata_id_[time_left>=0,]
	save(alldata_id_ok, file=paste(DATA_PATH, ci, if (HOLD_LENGTH >0) {'_hold'}, '.RData', sep=''))
}

load(file=paste(DATA_PATH,'New_Haven.RData',sep=''))
bayes_n_loo <- copy(alldata_id_ok)
load(file=paste(DATA_PATH,'Oakland.RData',sep=''))
bayes_n_loo <- rbind(bayes_n_loo, alldata_id_ok)
load(file=paste(DATA_PATH,'Richmond.RData',sep=''))
bayes_n_loo <- rbind(bayes_n_loo, alldata_id_ok)
load(file=paste(DATA_PATH,'Chicago.RData',sep=''))
bayes_n_loo <- rbind(bayes_n_loo, alldata_id_ok)

save(bayes_n_loo, file=paste(DATA_PATH,'BAYES_n_LOO.RData', sep=''))

################################################################################################################
################################################################################################################
##
## conversion to Big Column data set (stacking the three responses, so the file has 3 * rows)
## 
require('data.table')
MAIN_PATH = 's://SeeClickFix/'
DATA_PATH = 'data/'
MODEL_PATH = 'model/'
PRED_PATH = 'pred/'
setwd(MAIN_PATH)

load(file=paste(DATA_PATH,'alldata_basic.RData',sep=''))
load(file=paste(DATA_PATH,'BAYES_n_LOO.RData',sep=''))

options("scipen"=30)
NOZERO = 0.00000001
BAYES_WINDOW = 150
for (colnum in 2:ncol(bayes_n_loo)) {
		meanval <- mean(bayes_n_loo[[colnum]], na.rm = TRUE)
		maxval <- max(bayes_n_loo[[colnum]], na.rm = TRUE)
		set(bayes_n_loo, which(is.na(bayes_n_loo[[colnum]])), colnum, meanval)
		bayes_n_loo[[colnum]] <- bayes_n_loo[[colnum]]/maxval
		set(bayes_n_loo, which(bayes_n_loo[[colnum]] < NOZERO), colnum, 0)
}
setkey(alldata, id_)
setkey(bayes_n_loo, id_)
alldata <- bayes_n_loo[alldata]

save(alldata, file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_', BAYES_WINDOW, '.RData', sep=''))
##
## Votes
##
alldata_votes = copy(alldata)
alldata_votes[, c('num_comments','num_views'):=NULL]
setnames(alldata_votes, 'num_votes', 'response')
alldata_votes[, votes:=1L]
alldata_votes[, comments:=0L]
alldata_votes[, views:=0L]
##
## Comments
##
alldata_comments = copy(alldata)
alldata_comments[, c('num_votes','num_views'):=NULL]
setnames(alldata_comments, 'num_comments', 'response')
alldata_comments[, votes:=0L]
alldata_comments[, comments:=1L]
alldata_comments[, views:=0L]
##
## Views
##
alldata_views = copy(alldata)
alldata_views[, c('num_comments','num_votes'):=NULL]
setnames(alldata_views, 'num_views', 'response')
alldata_views[, votes:=0L]
alldata_views[, comments:=0L]
alldata_views[, views:=1L]
##
## Stacking
##
alldata <- rbind(alldata_views, alldata_votes, alldata_comments, use.names=TRUE)

save(alldata,file=paste(DATA_PATH, 'alldata_BAYES_n_LOO_', BAYES_WINDOW, '_big_column.RData', sep=''))

################################################################################################################
################################################################################################################
##
## Binary Bag of Words based in 'summary' feature
##
require('data.table')
require('tm')
require('Matrix')
MAIN_PATH = 's://SeeClickFix/'
DATA_PATH = 'data/'
MODEL_PATH = 'model/'
PRED_PATH = 'pred/'
setwd(MAIN_PATH)
MIN_CASES = 25

load(paste(DATA_PATH, 'alldata_BAYES_n_LOO_150.RData', sep=''))

bayes <- alldata[, c(1:137), with=FALSE]
load(file=paste(DATA_PATH,'alldata_basic.RData',sep=''))
alldata <- alldata[!(city=='Chicago' & year==2012 & month<11),]
alldata <- alldata[!(city=='Chicago' & year==2013 & month==3),]
alldata[,set_order:=1L]
alldata[set=='test',set_order:=2L]
setkey(alldata, set_order)
rowTrain <- which(alldata$set=='train')
rowTest <- which(alldata$set=='test')
alldata$summary <- gsub( "/" , " " , alldata$summary)
alldata$summary <- gsub( "grafitti" , "graffiti" , alldata$summary)
alldata$summary <- gsub( "matress" , "mattress" , alldata$summary)
summary_corpus <- Corpus(VectorSource(alldata$summary))
summary_corpus <- tm_map(summary_corpus, tolower) 
summary_corpus <- tm_map(summary_corpus, stripWhitespace) 
summary_corpus <- tm_map(summary_corpus, removeNumbers) 
summary_corpus <- tm_map(summary_corpus, removePunctuation)
summary_corpus <- tm_map(summary_corpus, removeWords, stopwords('english'))
summary_corpus <- tm_map(summary_corpus, stemDocument, language = "english") 
summary_dtm <- DocumentTermMatrix(summary_corpus, control = list(weighting = weightBin, bounds = list(global = c(50,Inf))))
summary_dt <- data.table(as.matrix(summary_dtm))
alldata <- alldata[, list(id_, id, summary, description, set, num_votes, num_comments, num_views, day, city, latitude, longitude, source, tag_type, 
	hour, week_day, summary_len, description_len)]
setnames(alldata, 'day', 'dayseq')
##
## One hot encoding
##
tag_type_freq <- alldata[,.N, by= tag_type]
key_OK <- tag_type_freq[N >= MIN_CASES,tag_type]
alldata[!(tag_type %in% key_OK), tag_type:='Others']
unique_keys <- unique(as.character(alldata$tag_type))
for (i in unique_keys) {
	alldata[,paste('tag_type_',i,sep=''):=as.integer(tag_type==i)]
}
source_freq <- alldata[,.N, by= source]
key_OK <- source_freq[N >= MIN_CASES,source]
alldata[!(source %in% key_OK), source:='Others']
unique_keys <- unique(as.character(alldata$source))
for (i in unique_keys) {
	alldata[,paste('source_',i,sep=''):=as.integer(source==i)]
}
alldata <- cbind(alldata, summary_dt[,2:ncol(summary_dt), with=FALSE])

save(alldata,file=paste(DATA_PATH, 'bow_data.RData', sep=''))
##
## Data for glm model
##
summary_SparseMatrix <- sparseMatrix(i=summary_dtm$i, j=summary_dtm$j, x=summary_dtm$v, dims=c(summary_dtm$nrow, summary_dtm$ncol))
setkey(bayes, id_)
setkey(alldata, id_)
alldata <- bayes[alldata]
MIN_CASES=25
alldata[,summary_len:=log1p(summary_len)]
alldata[,summary_len:=summary_len/max(alldata$summary_len, na.rm=TRUE)]
alldata[,description_len:=log1p(description_len)]
alldata[,description_len:=description_len/max(alldata$description_len, na.rm=TRUE)]
alldata[,lat_factor:=round(latitude,2)]
tbl <- alldata[,.N,by=lat_factor]
alldata[!(lat_factor %in% tbl[N >=MIN_CASES,lat_factor]), lat_factor:=0]
alldata[, lat_factor:=as.factor(lat_factor)]
alldata[,lon_factor:=round(longitude,2)]
tbl <- alldata[,.N,by=lon_factor]
alldata[!(lon_factor %in% tbl[N >=MIN_CASES,lon_factor]), lon_factor:=0]
alldata[, lon_factor:=as.factor(lon_factor)]
alldata[,lonlat_factor:=10000*round(longitude,2)+round(latitude,2)]
tbl <- alldata[,.N,by=lonlat_factor]
alldata[!(lonlat_factor %in% tbl[N >=MIN_CASES,lonlat_factor]), lonlat_factor:=0]
alldata[, lonlat_factor:=as.factor(lonlat_factor)]
alldata[, city:=as.factor(city)]
alldata[,remote_api:=as.integer(source=='remote_api_created')]
tbl <- alldata[,.N,by=source]
alldata[!(source %in% tbl[N >=MIN_CASES,source]), source:='Other']
alldata[, source:=as.factor(source)]
tbl <- alldata[,.N,by=tag_type]
alldata[!(tag_type %in% tbl[N >=MIN_CASES,tag_type]), tag_type:='Other']
alldata[, tag_type:=as.factor(tag_type)]
alldata[, hour:=as.factor(hour)]
alldata[, week_day:=as.factor(week_day)]

save(alldata,file=paste(DATA_PATH, 'BAYES_BOW_glm_data.RData', sep=''))
save(summary_SparseMatrix,file=paste(DATA_PATH, 'summary_SparseMatrix.RData', sep=''))

################################################################################################################
################################################################################################################
##
## Big column with text features
##
require('data.table')
MAIN_PATH = 's://SeeClickFix/'
DATA_PATH = 'data/'
MODEL_PATH = 'model/'
PRED_PATH = 'pred/'
setwd(MAIN_PATH)

load(paste(DATA_PATH, 'alldata_BAYES_n_LOO_150_big_column.RData', sep=''))

alldata <- alldata[!(city=='Chicago' & year==2012 & month<11),]
alldata <- alldata[!(city=='Chicago' & year==2013 & month==3),]
bigdata <- alldata[, list(id_, response, votes, comments, views, time_left, total_city_3, total_city_14, total_city_60, total_district_3, total_district_14, total_district_60, total_neighbour_3, 
total_neighbour_14, total_neighbour_60, self_city_3, self_city_14, self_city_60, self_district_3, self_district_14, self_district_60,
self_neighbour_3, self_neighbour_14, self_neighbour_60, views_city, views_district, views_neighbour, votes_city, votes_district, votes_neighbour, comments_city,
comments_district, comments_neighbour)]

load(paste(DATA_PATH, 'bow_data.RData', sep=''))
setkey(bigdata,id_)
setkey(alldata,id_)
bigdata <- alldata[bigdata]
response <- bigdata[,list(id_, response)]
bigdata[,c('id_','response'):=NULL]
bigdata <- cbind(response, bigdata)
save(bigdata, file=paste(DATA_PATH, 'bigdata.RData', sep=''))
