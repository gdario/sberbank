import pandas as pd
import numpy as np
# import matplotlib.pyplot as plt
# %matplotlib osx


# Read the dataset using the timestamp as index. Not sure this is a good idea
train = pd.read_csv('../data/train.csv.gz', compression='gzip',
                    index_col='timestamp')
test = pd.read_csv('../data/test.csv.gz', compression='gzip',
                   index_col='timestamp')
macro = pd.read_csv('../data/macro.csv.zip', compression='zip',
                    index_col='timestamp')

# To later identify the training and the test set
id_train = train['id']
id_test = test['id']

# Data Exploration
# It is very easy to concatenate two datsets, and we don't have to worry about
# the fact that the `price_doc` column is not in the test set
train_test = pd.concat([train, test], axis=0)
train_test_macro = train_test.join(macro, how='left')
del train_test

# 1) Percentage of missing values
# The fastest way to see the fraction of missing values is using `isnull`
# (NOT `isnan`).
perc_missing = train_test_macro.isnull().mean().sort_values(ascending=False)

# We can see that groups of variables have identical fractions of missing
# values.

# Cleaning up accoring to
# https://www.kaggle.com/captcalculator/a-very-extensive-sberbank-exploratory-analysis
# full_sq has unreaistically high values
idx = train_test_macro['full_sq'] > 1000
train_test_macro.loc[idx, 'full_sq'] = np.nan

# Remove the cases where life_sq is larger than full_sq
idx = train_test_macro['life_sq'] > train_test_macro['full_sq']
train_test_macro.loc[idx, 'life_sq'] = np.nan

# Remove the cases where the build year is less than 1600
idx = train_test_macro['build_year'] < 1600
train_test_macro.loc[idx, 'build_year'] = np.nan

# Remove the cases that have floor > max_floor
idx = train_test_macro['floor'] > train_test_macro['max_floor']
train_test_macro.loc[idx, 'floor'] = np.nan

# Add the relative floor, i.e. floor/max_floor
floor = train_test_macro['floor'].values
max_floor = train_test_macro['max_floor'].values
rel_floor = floor / max_floor
train_test_macro['rel_floor'] = rel_floor

# Split numeric and 'object' columns
numeric_set = train_test_macro.select_dtypes(exclude=['object'])
object_set = train_test_macro.select_dtypes(include=['object'])
assert numeric_set.shape[1] + object_set.shape[1] == train_test_macro.shape[1]

# There is only a limited number of object variables we need to deal with.
# The simplest way to count the number of unique values is the following:
# (note the use of x.unique instead of unique(x) and axis=0)
unique_values = object_set.apply(lambda x: len(x.unique()), axis=0)

# Most columns are binary, but some are not. In particular:
# sub_area: 146 values
# ecology: 5 values
# product_type: 3 values
# child_on_acc_pre_school: 5 values
# modern_education_share: 4 values
# old_education_build_share: 4 values
unique_values[unique_values > 2]

# ecology contains a 'no_data' field
idx = object_set['ecology'] == 'no data'
object_set.loc[idx, 'ecology'] = np.nan

# We drop the last three variables as they add nothing
object_set = object_set.drop(['child_on_acc_pre_school',
                              'modern_education_share',
                              'old_education_build_share'], axis=1)
binary_vars = object_set.drop(['ecology', 'sub_area'], axis=1)
binary_dummies = pd.get_dummies(binary_vars, drop_first=True)
multicategory_vars = object_set[['ecology', 'sub_area']]
multicategory_dummies = pd.get_dummies(multicategory_vars)

dummies_set = pd.concat([binary_dummies, multicategory_dummies], axis=1)

# Putting everything together
train_test_macro = pd.concat([numeric_set, dummies_set], axis=1)

train = train_test_macro.loc[train_test_macro['id'].isin(id_train)]
test = train_test_macro.loc[train_test_macro['id'].isin(id_test)]

# Save to HDF5
store = pd.HDFStore('train_test_macro.h5')
store['train'] = train
store['test'] = test
store.close()

