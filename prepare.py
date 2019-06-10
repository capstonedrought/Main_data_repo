import pandas as pd
from datetime import timedelta
import numpy as np
import datetime
from sklearn.preprocessing import MinMaxScaler

def get_data():
    '''creates a dataframe from the soil moisture csv, scales the data, adds the datetime index'''
    df = pd.read_csv('south_central_moisture_data_1988_2017.csv')
    df = df[['final_date', 'soil_moisture']]
    mmscaler = MinMaxScaler(feature_range=(0, 1))
    mmscaler.fit(df[['soil_moisture']])
    df.soil_moisture = mmscaler.transform(df[['soil_moisture']])
    df.final_date = pd.to_datetime(df.final_date)
    df = df.set_index('final_date')
    return df

def agg_data(df):
    '''aggregates the data by monthly mean and only takes data from 1997 to present to match the PDSI data'''
    df_agg = df.soil_moisture.resample('M').agg('mean')
    df_agg = df_agg['1997':]
    df_agg = df_agg.to_frame(name=None)
    return df_agg

csv_name = 'three_week_soil_moisture_predictions.csv'
def clean_predictions(csv_name):
    '''Takes the csv of predictions in weeks and matches column names to the soil moisture dataframe,
    scales the data, creates datetime index, and resamples by monthly mean'''
    predictions = pd.read_csv(csv_name)
    predictions.rename(columns={'date':'final_date'}, inplace=True)
    predictions.rename(columns={'predicted_soil_moisture':'soil_moisture'}, inplace=True)
    mmscaler = MinMaxScaler(feature_range=(0, 1))
    mmscaler.fit(predictions[['soil_moisture']])
    predictions.soil_moisture = mmscaler.transform(predictions[['soil_moisture']])
    predictions.final_date = pd.to_datetime(predictions.final_date)
    predictions = predictions.set_index('final_date')
    predictions = predictions.soil_moisture.resample('M').agg('mean')
    predictions = predictions.to_frame(name=None)
    return predictions

def soil_warning():
    '''Gets the soil moisture data, aggregates, and adds the cleaned predictions to the dataframe'''
    df = get_data()
    df_agg = agg_data(df)
    csv_name = 'three_week_soil_moisture_predictions.csv'
    predictions = clean_predictions(csv_name)
    joined = pd.concat([df_agg, predictions])
    return joined

def find_outliers(df):
    '''calculates a lower bound value to determine if soil moisture values are within normal levels or not'''
    agg_mean = df.soil_moisture.mean()
    agg_stdev = df.soil_moisture.std()
    df["lower_bound"] = (agg_mean - (4*agg_stdev))
    df["sm_lb"] = (df.soil_moisture - df.lower_bound)
    df['outside_normal'] = (df.sm_lb < 1)
    return df