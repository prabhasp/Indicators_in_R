# Using these libraries down below
water <- read.csv("~/Code/nmis/nmis/dropbox/facility_csvs/Water_Baseline_PhaseII_all_merged_cleaned_09_19_2011.csv")
library('plyr')
library('doBy')
library('reshape')

###### DATA CLEANING / NEW COLUMNS ########
cleaner_data <- function (data_frame, source) {
	if (source=="Water_Baseline") {
		data_frame <- data_frame[,c('lga','water_source_type', 'water_source_physical_state', 'lift_mechanism')]

		# merge 'newly_constructed' and 'well_maintained'
		data_frame$water_source_physical_state <- factor(recodeVar(data_frame$water_source_physical_state, src=c('newly_constructed'), tgt=c('well_maintained')))
		# merge boreholes (#1) and tubewell (#8) into borehole_or_tubewell
		data_frame$water_source_type <- factor(recodeVar(as.character(data_frame$water_source_type), src=c('borehole', 'tube_well', 'developed_and_treated_surface_water', 'developed_protected_spring_water', 'rainwater_harvesting_scheme'), tgt=c('borehole_or_tubewell', 'borehole_or_tubewell', 'developed_and_treated_spring_and_surface_water', 'developed_and_treated_spring_and_surface_water', 'developed_and_treated_spring_and_surface_water')))
		fprotected <- function(df) factor(recodeVar(as.character(df$water_source_type),
 src=c('borehole_or_tubewell', 'developed_and_treated_spring_and_surface_water','other_protected','protected_dug_well','other_unprotected'),
 tgt=c('protected','protected','protected','protected','unprotected')))
 	    # note: missing do_not_know; will be mapped to 
		fmotorized <- function(df) factor(recodeVar(as.character(df$lift_mechanism), src=c('diesel','solar','electric','other_powered','other_powered','animal','hand_pump','rope_pulley','other_nonpowered','do_not_know'), tgt=c('motorized','motorized','motorized','motorized','non_motorized','non_motorized','non_motorized','non_motorized','non_motorized','N/A')))
		cbind(data_frame, protected = fprotected(data_frame), motorized = fmotorized(data_frame))
	}
}

####### METHOD0 #####
# mash will take:
#   a dataframe (like the whole water data-set, perhaps a column subset)
#   a vector of types (like .(lga, water_source_type, lift_mechanims), etc) to pass to ddply
#   a separator to name indicators by
#   an aggregation function (the default, nrow, just counts the number per type)
# and return a data frame, which has an LGA, column, and an indicator column (ex. borehole&solar) with an aggregated value
mash <- function (df, type_vec, sep="&", fun=nrow) {
	res <- ddply(df, type_vec, fun, .drop=FALSE)
	# now, we combine all of the result factors into one factor; not including the lga factor for obvious reasons
	working_type_vec <- type_vec[names(type_vec)!="lga"]
	combined_column <- function () with(res, do.call(paste, c(working_type_vec, sep=sep)))
	summarize(res, lga = lga, indicator=as.factor(combined_column()), value = V1)
}
# mash_some will take:
#   a dataframe (like the whole water data-set, perhaps a column subset)
#   a vector of types (like .(lga, water_source_type, lift_mechanism), etc) to pass to ddply
#   a list of constraints (like list(lift_mechanism=c("solar", "diesel"))) which to subset by
#   a separator to name indicators by
#   an aggregation function (the default, nrow, just counts the number per type)
# and return the result of mash, but only for the set of data which have types specified by constrain_vec
# ie, mash_some(df, lift_mechanism, list(lift_mechanism="solar")) will only mash solar values
# mash_some(df, lift_mechanims, "all") won't do any subsetting; will mash all values
mash_some <- function (df, type_vec, constraint_dict, sep="&", fun=nrow) {
	for(i in 1:length(constraint_dict)) {
		colname <- names(constraint_dict)[[i]]
		df <- subset (df, df[[colname]] %in% constraint_dict[[i]])
		df[[colname]] <- factor(as.character(df[[colname]]))
	}
	mash(df, type_vec, sep=sep, fun=fun)
}


#### MAIN ####
water_clean <- cleaner_data(water, "Water_Baseline")
table1 <- mash(water_clean, .(lga, water_source_type))
table2denom <- mash_some(water_clean, .(lga, protected),		  
	list(protected="protected"))
table2num <- mash_some(water_clean, .(lga, protected, water_source_physical_state),		  
	list(protected="protected", water_source_physical_state = "poorly_maintained"))	
table4num <- mash_some(water_clean, .(lga, water_source_type, motorized), 
	list(water_source_type="borehole_or_tubewell", motorized=c("motorized", "non_motorized")))
table4num2 <- mash_some(water_clean, .(lga, water_source_type, lift_mechanism), 
	list(water_source_type="borehole_or_tubewell", lift_mechanism=c("solar","diesel","electric")))
	
table5num <- mash_some(water_clean, .(lga, water_source_type, motorized, water_source_physical_state), 
	list(water_source_type="borehole_or_tubewell", motorized=c("motorized", "non_motorized"), water_source_physical_state="poorly_maintained"))
table5num2 <- mash_some(water_clean, .(lga, water_source_type, lift_mechanism, water_source_physical_state), 
	list(water_source_type="borehole_or_tubewell", lift_mechanism=c("solar","diesel","electric"), water_source_physical_state="poorly_maintained"))

res <- rbind(table1, table2denom, table2num, table4num, table4num2, table5num, table5num2)
(res)

########### MAIN #######################
# should be able to take a command that looks like:
# process(sources=list(c(type="Water_Baseline", file="~/Code/nmis/nmis/dropbox/facility_csvs/Water_Baseline_PhaseII_all_merged_cleaned_09_19_2011.csv"),
#		             c(type="Water_Pilot", file="~/Code/nmis/nmis/dropbox/facility_csvs/Water_Baseline_PhaseII_all_merged_cleaned_09_19_2011.csv")),
#		indicators=list(),
#		lgas=list()
#	    )



