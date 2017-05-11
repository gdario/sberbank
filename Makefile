.PHONY = clean all

vpath %.RData output
vpath %.R R

R := R CMD BATCH --no-save

%.Rout: %.R
	  $(R) $<

# Select the most important features
#select_features.Rout: create_dummies.Rout

# Create dummy variables for the multi-category columns
#create_dummies.Rout: add_new_features.Rout

# Add new features (to the training set only)
# add_new_features.Rout: clean_columns.Rout

# Clean the columns that have the wrong data types etc.
#clean_columns.Rout: select_columns.Rout

# Remove the columns that are unlikely to contribute. Note, no NA
# filtering is performed here.
#select_columns.Rout: merge_with_macro.Rout

clean_trianing_set.Rout: 
# merge_with_macro.Rout:

clean:
		rm -f *.Rout
		rm -f output/*.RData
		