import pandas as pd
from datetime import timedelta
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from sklearn import svm
from sklearn import metrics
import itertools
import datetime
from sklearn.preprocessing import MinMaxScaler



# This function will add a zero to the beginning of a string if it is a certain length. 
# This allows the string to work with the next functions.
def add_zeros(string):
    if len(str(string)) == 7:
        return '0' + str(string)
    else:
        return str(string)

# This function will grab the four digit year from the values in the first column of the raw data.
def grab_four_digit_year(string):
    if len(str(string)) == 10:
        return str(string)[-6:-2]
    else:
        return str(string[-4:])
    
# This function will grab the two digit year from the values in the first column of the raw data.
def grab_two_digit_year(string):
    if len(str(string)) == 6:
        return '19' + str(string)[-2:]
    else:
        return '19' + str(string[-4:-2])
    
# This function will grab the state number from the values in the first column.
def grab_state(string):
    return str(string)[:2]

# This function will grab the division number from the values in the first column.
def grab_division(string):
    return str(string)[2:4]

def initial_pull_of_clean_data(years_to_pull_data_from):
    """
    Takes in a list of years. These years tell the function which files to open. 
    The files should be in the format of soil_moisture_(year).xlsx and saved in a subdirectory called 'moisture_data'.
    It will pull the data for each year into separate dataframes and save them in a dictionary named 'years'.
    """
    years_on_file = years_to_pull_data_from

    years = {}
    for year in years_on_file:
        df = pd.read_excel(f'/Users/MatthewZapata/Main_data_repo/moisture_data/soil_moisture_{year}.xlsx', header=None, 
                        usecols=[0, 1, 2, 3, 4, 21, 22, 23, 31, 34], 
                        dtype={0:'object', 1:'object'})
        df.columns = ['div_year', 'week', 'precip_total_inches', 'temp_average_F', 
                    'available_moisture_in_both_layers_start_in', 'moisture_anomaly_index', 
                    'available_moisture_surface_layer_in', 'available_moisture_underlying_layer_in',
                    'modified_palmer_drought_index', 'crop_moisture_index']
        
        df['div_year_with_zeros'] = df.div_year.apply(add_zeros)

        if int(year) >1996:
            df['year'] = df.div_year_with_zeros.apply(grab_four_digit_year)
        else:
            df['year'] = df.div_year_with_zeros.apply(grab_two_digit_year)

        df['state'] = df.div_year_with_zeros.apply(grab_state)

        df['division'] = df.div_year_with_zeros.apply(grab_division)
        
        
        df = df[df.state == '41']
        
        years[f'soil_moisture_{year}'] = df
    return years


def correcting_dates(dictionary_of_data):
    for key in dictionary_of_data:
        # print(key)
        list_of_div = []
        for div_number in dictionary_of_data[key].division.unique():
            df = dictionary_of_data[key][dictionary_of_data[key].division == div_number]
            df['date'] = f'{df.year.iloc[0]}-03-01'
            df['date'] = pd.to_datetime(df['date'])

            dates = []
            i=0
            for value in df.date:
                new_date = value + timedelta(days=i)
                dates.append(new_date)
                i += 7
        
            df['final_date'] = dates
            list_of_div.append(df)
        joined_df = pd.concat(list_of_div)
            
        dictionary_of_data[key] = joined_df

division_table = pd.DataFrame({'id':['01', '02', '03', '04', '05', '06', '07', '08', '09', '10'], 
    'division_name':['high_plains', 'low_rolling_plains', 'north_central', 'east_texas', 'trans_pecos', 
    'edwards_plateau', 'south_central', 'upper_coast', 'southern', 'lower_valley']})

def combine_keys(dictionary_of_data):
    soil = pd.concat(dictionary_of_data).reset_index()
    soil = soil[['state', 'division', 'year', 'week', 'date', 'final_date', 'precip_total_inches',
        'temp_average_F', 'available_moisture_in_both_layers_start_in',
        'moisture_anomaly_index', 'available_moisture_surface_layer_in',
        'available_moisture_underlying_layer_in', 'modified_palmer_drought_index',
        'crop_moisture_index']]
    labeled_soil = soil.merge(division_table, left_on='division', right_on='id')
    labeled_soil = labeled_soil[['final_date', 'division_name', 'precip_total_inches',
       'temp_average_F', 'available_moisture_in_both_layers_start_in',
       'moisture_anomaly_index', 'available_moisture_surface_layer_in',
       'available_moisture_underlying_layer_in', 'modified_palmer_drought_index',
       'crop_moisture_index']]
    return labeled_soil
    
def make_final_dataframe(df):
    south_central_moisture = df[df.division_name == 'south_central']
    south_central_moisture = south_central_moisture[['final_date', 'available_moisture_in_both_layers_start_in', 'modified_palmer_drought_index']]
    south_central_moisture.columns = ['final_date', 'soil_moisture', 'pdsi']
    return south_central_moisture

def clean_and_prep(years_to_pull_data_from):
    """
    Takes in years to pull in and include in a dataframe.
    """
    years = initial_pull_of_clean_data(years_to_pull_data_from)
    correcting_dates(years)
    joined_df = combine_keys(years)
    final_df = make_final_dataframe(joined_df)
    return final_df