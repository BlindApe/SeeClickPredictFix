rm(list = ls(all = TRUE)) 
MAIN_PATH = 's://SeeClickFix/'
DATA_PATH = 'data/'
MODEL_PATH = 'model/'
SUBMISSION_PATH = 'pred/'
setwd(MAIN_PATH)
DATAPROCESS = TRUE
ONLYPRED = FALSE
options("scipen"=100)
require('data.table')
require('gbm')
source(paste(MAIN_PATH, 'models.R', sep=''))
if (DATAPROCESS) {
	##
	## Process data.csv and test csv
	## The LOO and BAYES feature set takes several days in process
	##
	source(paste(MAIN_PATH, 'data.R', sep=''))
}	
##
## See the models.R file for description of each function
##
basic_3_models_train_120_hold_60(ONLYPRED)
basic_3_models_train_60_hold_0(ONLYPRED)
gbm_BC_train_60_hold_0(ONLYPRED)
gbm_BC_train_60_hold_60(ONLYPRED)
gbm_BC_train_1000_hold_0(ONLYPRED)
RF_BC(ONLYPRED)
gbm_by_city_BC_BAYES_n_BoW(ONLYPRED)
gbm_BC_LOO_train_60_hold_0(ONLYPRED)
gbm_BC_BAYES_train_60_hold_0(ONLYPRED)
glm_BAYES_n_BoW (ONLYPRED)
naive()
blending()
