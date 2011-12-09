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

#### DATA ####
water_clean <- cleaner_data(water, "Water_Baseline")

####### METHOD0 #####
mash <- function (df, type_vec, sep="&", fun=nrow) {
	res <- ddply(df, type_vec, fun, .drop=FALSE)
	# now, we combine all of the result factors into one factor; not including the lga factor for obvious reasons
	working_type_vec <- type_vec[names(type_vec)!="lga"]
	combined_column <- function () with(res, do.call(paste, c(working_type_vec, sep=sep)))
	summarize(res, lga = lga, indicator=as.factor(combined_column()), value = V1)
}
# this one subsets each type by the values specified in which_vec
# ie, mash_some(df, lift_mechanism, "solar") will only mash solar values
# mash_some(df, lift_mechanims, "all") won't do any subsetting; will mash all values
mash_some <- function (df, type_vec, constraint_vec, sep="&", fun=nrow) {
	for(in in 1:length(constraint_vec)) {
		df <- subset(df, df$names(constraint_vec[[i]]) %in% constraint_vec[[i]])
	}
	df
}
`
mash(water_clean, .(lga, water_source_type))
mash_some(water_clean, .(lga, protected, water_source_physical_state), (protected="protected", water_source_physical_state="poorly_maintained"))
mash_some(water_clean, .(lga, protected), ("all", "protected"))
mash_some(water_clean, .(lga, water_source_type), ("all", "borehole_or_tubewell")
mash_some(water_clean, .(lga, water_source_type, motorized, water_source_physical_state), ("all", "borehole_or_tubewell", "all"))
mash_some(water_clean, .(lga, water_source_type, lift_mechanism, water_source_physical_state), ("all", "borehole_or_tubewell", c("solar","diesel","electric")))
mash_some(water_clean, .(lga, water_source_type, motorized, water_source_physical_state), ("all", "borehole_or_tubewell", "all", "poorly_maintained"))
mash_some(water_clean, .(lga, water_source_type, lift_mechanism, water_source_physical_state), ("all", "borehole_or_tubewell", c("solar","diesel","electric"), "poorly_maintained"))
`
combo1 <- mash(water_clean, .(lga, water_source_type, lift_mechanism, water_source_physical_state))
combo3 <- mash(water_clean, .(lga, water_source_type, motorized,      water_source_physical_state))
combo2 <- mash(water_clean, .(lga, protected,         lift_mechanism, water_source_physical_state))
combo4 <- mash(water_clean, .(lga, protected,         motorized,      water_source_physical_state))
combo5 <- mash(water_clean, .(lga, water_source_type, lift_mechanism))
combo1 <- mash(water_clean, .(lga, water_source_type, motorized))
combo5 <- mash(water_clean, .(lga, water_source_type, lift_mechanism))
combo1 <- mash(water_clean, .(lga, water_source_type, motorized))
res <- rbind(combo1, combo2, combo3, combo4)



####### METHOD1 #####
####### The aggregation functions #####
numbers <- function(data_frame, type, withall=TRUE) { 
	f <- ddply(data_frame, c("lga", type), nrow, .drop=FALSE)
	if (withall) cast(melt(f), margins=TRUE, fun=sum)[0:2+length(type)]
	else f 
}
just_numbers <- function(data_frame, type, srcval, tgtval, withall=TRUE) {
	res <- numbers(data_frame, type, withall=withall)
	res[[type]] <- factor(recodeVar(as.character(res[[type]]), src=srcval, tgt=tgtval, default=NA))
	res
}
# tables
(table1 <- just_numbers(subset(water_clean, 
		subset=TRUE),
    "water_source_type",
	 srcval=c('borehole_or_tubewell', 'developed_and_treated_spring_and_surface_water','protected_dug_well','other_protected','other_unprotected', '(all)'),
 	 tgtval=c('W.BT.*.*.N', 'W.SS.*.*.N', 'W.PD.*.*.N', 'W.OP.*.*.N',  'W.OU.*.*.N', 'W.*.*.*.N')))
(table2 <- just_numbers(subset(water_clean, 
	     subset=(water_clean$protected=='protected')), 
     "water_source_physical_state",
	 srcval=c('poorly_maintained', '(all)','well_maintained'),
 	 tgtval=c('W.PR.*.PO.N', 'W.*.*.PO.N', NA)))
# table3 no can do right now
(table41 <- just_numbers(subset(water_clean, 
	    subset=(water_clean$water_source_type=='borehole_or_tubewell')),
    "motorized",
     withall=FALSE,
	 srcval=c('motorized','non_motorized'),
 	 tgtval=c('W.BT.MOT.*.N', 'W.BT.MAN.*.N')))
(table42 <- just_numbers(subset(water_clean, 
	    subset=(water_clean$water_source_type=='borehole_or_tubewell')),
    "lift_mechanism",
     withall=FALSE,
	 srcval=c('solar','diesel', 'electric'),
 	 tgtval=c('W.BT.SOL.*.N', 'W.BT.DIE.*.N', 'W.BT.ELE.*.N')))
(table51 <- just_numbers(subset(water_clean, 
	    subset=(water_clean$water_source_type=='borehole_or_tubewell' & water_source_physical_state=='poorly_maintained')),
    "motorized",
     withall=FALSE,
	 srcval=c('motorized','non_motorized'),
 	 tgtval=c('W.BT.MOT.PO.N', 'W.BT.MAN.PO.N')))
(table52 <- just_numbers(subset(water_clean, 
		subset=(water_clean$water_source_type=='borehole_or_tubewell' & water_source_physical_state=='poorly_maintained')),
    "lift_mechanism",
     withall=FALSE,
	 srcval=c('solar','diesel', 'electric'),
 	 tgtval=c('W.BT.SOL.PO.N', 'W.BT.DIE.PO.N', 'W.BT.ELE.PO.N')))
 	 
 table <- rbind(table1, table2, table3, table41, table42, table51, table52)
 write.csv(table, "~/Desktop/output.csv")
