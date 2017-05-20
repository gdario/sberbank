.PHONY = clean all

vpath %.RData output
vpath %.R R

R := R CMD BATCH --no-save

%.Rout: %.R
	  $(R) $<

# xgb_complete.Rout: preprocess_macro.Rout create_submission.R
create_train_test_matrices.Rout: 	preprocess_train_and_test.Rout\
																	utilities.R
preprocess_train_and_test.Rout: utilities.R

clean:
		rm -f *.Rout
		rm -f output/*.RData
		