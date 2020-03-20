
# This function cleans the country of the dataframe df entered as argument.
# It matches the country df to a "dictionary" of countries: Data/Processed Data/To Clean Countries/countries.csv
# and returns a "uniform" name. 
# The basis of the country names is the GTD. As a country without any terrorisme event is not in the GTD.

# More precisely, it matches df.country to countries.country, and uses countries.new_country as new "uniform" country
# So the column country should contain all the different spelling occurring.

# For each new dataset that is merged to the GTD, it is important to:
#     1. initially clean their countries with the function, and try to merge.
#     2. evaluate the countries of the new dataset that were not matched (country = NA)
#     3. update Data/Processed Data/To Clean Countries/countries.csv
#       i.e. add a new row with unmatched country and desired name.
#     4. re-clean the countries and re-merge.


clean_countries <- function(df, path_to_country_dictionary) {
  # argument is a dataframe, returns a dataframe, with a clean country name
  
  country_dic <- read.csv(path_to_country_dictionary) # load country dictionary
  
  return(
    left_join(df, country_dic, by = "country") %>% # merge by "country" column with df
      dplyr::select(-country) # keep only the new "consolidated_country" name
  )
  
}

