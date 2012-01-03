# Using these libraries down below
#water <- read.csv("~/Code/nmis/nmis/dropbox/facility_csvs/Water_Baseline_PhaseII_all_merged_cleaned_09_19_2011.csv")
library('plyr')
library('doBy')
library('reshape')
library('R.oo')

###### DATA CLEANING / NEW COLUMNS ########

# cleaner_data takes data and a source indicator (such as Water_Baseline), does some basic re-coding, and produces some extra variables that we need (such as a new "motorized" factor)
cleaner_data <- function (data_frame, source_type) {
	if (source_type=="Water_Baseline") {
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
	} else {
		throw('Source_type ', source_type, ' not supported yet.')
	}
}

####### METHOD0 #####
subset_by_constraint_dict <- function(df, constraint_dict) {
	for(i in 1:length(constraint_dict)) {
		colname <- names(constraint_dict)[[i]]
		old_levels <- levels(df[[colname]])
		df <- subset (df, df[[colname]] %in% constraint_dict[[i]])
		df[[colname]] <- factor(as.character(df[[colname]]), levels=constraint_dict[[i]])
	}
	df
}

# mash_some will take:
#   a dataframe (like the whole water data-set, perhaps a column subset)
#   a vector of types (like .(lga, water_source_type, lift_mechanism), etc) to pass to ddply
#      (TODO: take out type_vec, and just constrain it to names(constraint_dict) ?)
#   a list of constraints (like list(lift_mechanism=c("solar", "diesel"))) which to subset by
#   a separator to name indicators by
#   an aggregation function (the default, nrow, just counts the number per type)
# and return the result of mash, but only for the set of data which have types specified by constrain_vec
# ie, mash_some(df, lift_mechanism, list(lift_mechanism="solar")) will only mash solar values
# mash_some(df, lift_mechanims, "all") won't do any subsetting; will mash all values
mash_some <- function (df, type_vec, constraint_dict=list(), sep="&", fun=nrow) {
	df2 <- subset_by_constraint_dict(df, constraint_dict)
	if (nrow(df2) == 0) {
		#HACK: assign a random row to df, so that we get expected results from df still; see 
		# reason: https://groups.google.com/forum/#!searchin/manipulatr/ddply$20empty$20data$20frame/manipulatr/dboVcECswoE/vyvRTWUebcYJ
		# it works because this will be taken out down below in the subset_by_constraint line
		df2 <- df[1,] 
	}
	res <- ddply(df2, type_vec, fun, .drop=FALSE)
	res <- subset_by_constraint_dict(res, constraint_dict)
	
	# now, we combine all of the result factors into one factor; not including the lga factor for obvious reasons
	# note: some syntactic hacking is required to support both .(lga, water_source_type) and c("lga", "water_source_type") arguments
	if(class(type_vec) == "quoted") working_type_vec <- type_vec[names(type_vec)!="lga"]
	else if (class(type_vec) == "character")	working_type_vec <- as.quoted(type_vec[type_vec!="lga"])
	combined_column <- function () with(res, do.call(paste, c(working_type_vec, sep=sep)))
	summarize(res, lga = lga, indicator=as.factor(combined_column()), value = V1)
}


#### MAIN ####

########### UTILS ######################
# indicators_to_indicatordict("borehole_or_tubewell&protected", df) should produce list(water_source_type="borehole_or_tube_well", protected="protected")
# the df is used to get the factor levels, really.
# sep is the separator among the various types.
indicator_to_indicatordict <- function(indicator, df, sep='&') {
	res <- list()
	subindic_levels <- sapply(names(df), function (x) levels(as.factor(df[[x]])), simplify=FALSE)
	subindic_vals <- strsplit(indicator, sep)[[1]]
	for(i in 1:length(subindic_vals)) {
		this <- subindic_vals[[i]]
		which_level <- sapply(subindic_levels, function(level) this %in% level)
		if (sum(which_level) != 1) {throw(this, " not in data, or matches more than one column.")}
		res[names(which_level[which_level==TRUE])[1]] <- this # res
	}
	res
}
indicators_to_indicatordicts <- function(indicators, df, sep='&') {
	res <- lapply(indicators, function(x) indicator_to_indicatordict(x, df, sep=sep))
	names(res) <- indicators
	res
}
process_one_src <- function(source, indicators, lgas='all') {
	#TODO: refactor into an apply function
	res_df <- data.frame()

	clean_src <- cleaner_data(read.csv(source[["file"]]), source[["type"]])
	indicatordicts <- indicators_to_indicatordicts(indicators, clean_src)
	for(i in 1:length(indicatordicts)) {
		indicatordict <- indicatordicts[[i]]
# TODO: fix lga filtration (work left in mash_some, i think). only the all lga case is working now. 
		if(lgas=='all') {	
				this_res <- mash_some(clean_src, c("lga", names(indicatordict)), indicatordict)
		} else {
			indicatordict<- c(lga=lgas, indicatordict)
			this_res <- mash_some(clean_src, names(indicatordict), indicatordict)
		}
		
		res_df <- rbind(res_df, this_res)		
	}
	res_df
}
process <- function(sources, indicators, lgas) {
	#TODO: refactor into an apply function
	res_df <- data.frame()
	for(i in 1:length(sources)) {
		source <- sources[[i]]

		this_res <- process_one_src(source, indicators, lgas)

		res_df <- rbind(res_df, this_res)
	}
	res_df
}


########### MAIN #######################
# should be able to take a command that looks like:
res2 <- process_one_src(c(type="Water_Baseline", file="~/Code/nmis/nmis/dropbox/facility_csvs/Water_Baseline_PhaseII_all_merged_cleaned_09_19_2011.csv"),
		  list("borehole_or_tubewell", "developed_and_treated_spring_and_surface_water", "other_protected", "other_unprotected", "protected_dug_well", "protected", "protected&poorly_maintained", "borehole_or_tubewell&motorized", "borehole_or_tubewell&non_motorized", "borehole_or_tubewell&diesel", "borehole_or_tubewell&electric", "borehole_or_tubewell&solar", "borehole_or_tubewell&motorized&poorly_maintained", "borehole_or_tubewell&non_motorized&poorly_maintained", "borehole_or_tubewell&diesel&poorly_maintained", "borehole_or_tubewell&electric&poorly_maintained", "borehole_or_tubewell&solar&poorly_maintained"))
print(head(res2))
res3 <- process_one_src(c(type="Water_Baseline", file="~/Code/nmis/nmis/dropbox/facility_csvs/Water_Baseline_PhaseII_all_merged_cleaned_09_19_2011.csv"),
		  list("borehole_or_tubewell", "developed_and_treated_spring_and_surface_water", "other_protected", "other_unprotected", "protected_dug_well", "protected", "protected&poorly_maintained", "borehole_or_tubewell&motorized", "borehole_or_tubewell&non_motorized", "borehole_or_tubewell&diesel", "borehole_or_tubewell&electric", "borehole_or_tubewell&solar", "borehole_or_tubewell&motorized&poorly_maintained", "borehole_or_tubewell&non_motorized&poorly_maintained", "borehole_or_tubewell&diesel&poorly_maintained", "borehole_or_tubewell&electric&poorly_maintained", "borehole_or_tubewell&solar&poorly_maintained"), lgas=list("ABAJI"))
print(res3)
