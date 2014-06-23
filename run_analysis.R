run_analysis <- function() {
	library(data.table)
	library(reshape2)
	# set main data source path
	src <- "./UCI HAR Dataset/"

	# set test and training data paths
	train_dir <- paste(src,"train/", sep="")
	test_dir <- paste(src,"test/", sep="")

	# set test data file paths
	testdata_x 	<- paste(test_dir, "X_test.txt", sep="")
	testdata_y <- paste(test_dir, "y_test.txt", sep="")	
	testdata_s	<- paste(test_dir, "subject_test.txt", sep="")

	# set train data file paths
	traindata_x <- paste(train_dir, "X_train.txt", sep="")
	traindata_y <- paste(train_dir, "y_train.txt", sep="")
	traindata_s	<- paste(train_dir, "subject_train.txt", sep="")

	# read original data labels
	data_labels	<- paste(src, "features.txt", sep="")
	
	# Merging subject_test and subject_train row wise respectively, creating first column "subj_id"
	subject_df <- rbind(read.table(testdata_s,  col.names = "subj_id",  header=FALSE), read.table(traindata_s, col.names = "subj_id",  header=FALSE))
	subject_df[,1] <- as.factor(subject_df[,1])

	# Merging y_test and y_train row wise respectively, creating second column "activity"
	y_df <- rbind(read.table(testdata_y,  col.names = "activity",  header=FALSE), read.table(traindata_y, col.names = "activity",  header=FALSE))

	# converting activity id's into descriptive form
	act_list <- c("Walking", "Up-stairs", "Down-stairs", "Sitting", "Standing", "Laying")
	y_df[,1] <- factor(y_df[,1], labels=act_list)

	# Merging x_test and x_train row wise respectively, to complete reading
	x_df  <- rbind(read.table(testdata_x,  header=FALSE), read.table(traindata_x, header=FALSE))
	
	# Reading 2nd column of features file
	col_names <- read.table(data_labels, sep=" ", header=FALSE)[,2]
		
	# Giving column names read from features file to merged x data frame
	colnames(x_df) <- col_names
	
	# Combining subject, x and y (test and train) data frames 
	full_df <- cbind(subject_df, y_df, x_df)
	

	# Creating a table by subsetting data to mean and standard deviation
	mean_stdev <- cbind(x_df[c(grep("mean\\(\\)\\-", names(x_df)), grep("std\\(\\)\\-",  names(x_df)))])
	clean_df <- data.table(mean_stdev)

	# Modifying column labels
	names_list <- colnames(clean_df)
	names_list <- gsub("mean\\(\\)\\-", "Mean", names_list)
	names_list <- gsub("std\\(\\)\\-", "Std", names_list)
	names_list <- gsub("\\-", "", names_list)
	setnames(clean_df, names_list)
	
	clean_data <- data.table(cbind(subject_df, y_df, clean_df))

	fact <- c("subj_id", "activity")
	
	# Computing mean and stdev by subj_id and activity
	melt_data <- melt(clean_data, id.vars=fact, measure.vars=names_list)
	final_data <- dcast.data.table(melt_data, subj_id + activity ~ ..., fun=mean)
	final_data
}
