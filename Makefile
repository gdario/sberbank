.PHONY = clean all

vpath %.RData output
vpath %.R R

R := R CMD BATCH --no-save

%.Rout: %.R
	  $(R) $<

xgb_complete.Rout: create_val_set.Rout create_submission.R
create_val_set.Rout: preprocess_macro.Rout
preprocess_macro.Rout: preprocess_train_and_test.Rout
preprocess_train_and_test.Rout: clean_dataset.R

clean:
		rm -f *.Rout
		rm -f output/*.RData
		