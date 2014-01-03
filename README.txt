This is the code used in the See Click Predict Fix challenge in Kaggle.

1. Hardware / OS platform used

This code was tested in a Windows 7 Professional 64 bits Xeon E5-2687W with 192 Gb RAM.

Contact: José A. Guerrero (blindape@ono.com)


2. How to train the model and make predictions on a new test set

For train & predict:

- Download the data (train.csv and test.csv) from Kaggle web and put in DATA_PATH directory
- Set parameters in run_models.R
	DATAPROCESS = TRUE
	ONLYPRED = FALSE
- Execute run_models.R (Data process takes several days for LOO and BAYES features!)
- The models are saved in MODEL_PATH directory and the predictions in SUBMISSION_PATH directory
- The final solution is generated in MAIN_PATH\blending.csv

For predict only new data:

- Set parameters in run_models.R
	DATAPROCESS = TRUE
	ONLYPRED = TRUE
- Execute run_models.R (Data process takes several days for LOO and BAYES features!)
- The predictions are saved in SUBMISSION_PATH directory (copy all previous prediction to other fold!)
- The final solution is generated in MAIN_PATH\blending.csv

	
3. 3rd-party software

R version 3.0.0 (2013-04-03) -- "Masked Marvel"
Copyright (C) 2013 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R packages:

Package gbm

Version: 2.1
Date: 2013-05-10
Depends: R (>= 2.9.0), survival, lattice, mgcv
License: GPL (version 2 or newer)
URL: http://code.google.com/p/gradientboostedmodels/

Greg Ridgeway <gregridgeway@gmail.com> with contributions by Daniel Edwards, Brian Kriegler,
Stefan Schroedl and Harry Southworth.

Package randomForest

Version 4.6-7
Date 2012-10-16
Depends R (>= 2.5.0), stats
Suggests RColorBrewer, MASS
Author Fortran original by Leo Breiman and Adele Cutler, R port by Andy Liaw and MatthewWiener.
Description Classification and regression based on a forest of trees using random inputs.
Maintainer Andy Liaw <andy_liaw@merck.com>
License GPL (>= 2)
URL http://stat-www.berkeley.edu/users/breiman/RandomForests

Package glmnet

Version:	1.9-5
Depends:	Matrix (= 1.0-6), utils
Suggests:	survival, foreach
Published:	2013-08-04
Author:	Jerome Friedman, Trevor Hastie, Rob Tibshirani
Maintainer:	Trevor Hastie <hastie at stanford.edu>
License:	GPL-2
URL:	http://www.jstatsoft.org/v33/i01/

Package Matrix

Version:	1.1-1.1
Depends:	R (= 2.15.0), methods
Imports:	graphics, grid, stats, utils, lattice
Suggests:	expm, MASS
Enhances:	MatrixModels, graph, SparseM, sfsmisc
Published:	2013-12-30
Author:	Douglas Bates and Martin Maechler
Maintainer:	Martin Maechler <mmaechler+Matrix at gmail.com>
Contact:	Doug and Martin <Matrix-authors@R-project.org>
License:	GPL-2 | GPL-3 [expanded from: GPL (= 2)]
URL:	http://Matrix.R-forge.R-project.org/

Package tm

Version:	0.5-9.1
Depends:	R (= 2.14.0), methods
Imports:	parallel, slam (= 0.1-22)
Suggests:	filehash, proxy, Rgraphviz, SnowballC, XML
Published:	2013-07-10
Author:	Ingo Feinerer [aut, cre], Kurt Hornik [aut]
Maintainer:	Ingo Feinerer <feinerer at logic.at>
License:	GPL-2 | GPL-3 [expanded from: GPL (= 2)]
URL:	http://tm.r-forge.r-project.org/

Package data.table

Version:	1.8.10
Depends:	R (= 2.12.0)
Imports:	methods
Suggests:	chron, ggplot2 (= 0.9.0), plyr, reshape, testthat (= 0.4), hexbin, fastmatch, nlme, xts, bit64
Published:	2013-09-03
Author:	M Dowle, T Short, S Lianoglou with contributions from A Srinivasan, R Saporta
Maintainer:	Matthew Dowle <mdowle at mdowle.plus.com>
BugReports:	https://r-forge.r-project.org/tracker/?group_id=240
License:	GPL-2 | GPL-3 [expanded from: GPL (= 2)]
URL:	http://datatable.r-forge.r-project.org/, http://stackoverflow.com/questions/tagged/data.table
