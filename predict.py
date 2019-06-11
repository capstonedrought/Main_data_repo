import pandas as pd
from datetime import timedelta
import numpy as np
from sklearn import svm
from sklearn import metrics
import datetime
from sklearn.preprocessing import MinMaxScaler
from clean_prep_moisture_data import clean_and_prep

def embed_data(x, window_size, steps_away):
    n = len(x)
    # steps = n - steps_away
    # print(f'n: {n}')
    xout = np.zeros((n-window_size-steps_away, window_size))
    # print(f'xout: {xout}')
    # print(f'shape of xout: {xout.shape}')
    yout = x[(window_size + steps_away):]
    # print(f'yout: {yout}')
    # print(f'shape of yout: {yout.shape}')
    if steps_away == 0:
        for i in np.arange(window_size, n):
            xout[i-window_size] = x[i-window_size:i]
        # print(f'xout: {xout}')
        return xout, yout
    else:
        for i in np.arange(window_size, n-steps_away):
            xout[i-window_size] = x[i-window_size:i]
        # print(f'xout: {xout}')
        return xout, yout



def get_inputs(year_in_a_list):
    new_soil = clean_and_prep(year_in_a_list)

    new_soil = new_soil[['final_date', 'soil_moisture']]

    new_soil.final_date = pd.to_datetime(new_soil.final_date)
    new_soil = new_soil.set_index('final_date')
    
    new_soil_agg = new_soil.soil_moisture.resample('W').agg('mean')
    date = new_soil_agg.index[-1]
    array_of_data = np.array(new_soil_agg)
   
    filled_array_of_data = np.array(pd.Series(array_of_data).bfill())

    x, y = embed_data(filled_array_of_data, 3, 2)

    a, b, c = y[-3:]
    x_for_pred = np.array([[a , b, c]])

    return date, x_for_pred



new_soil = pd.read_csv('south_central_moisture_data_1988_2017.csv')

new_soil = new_soil[['final_date', 'soil_moisture']]

new_soil.final_date = pd.to_datetime(new_soil.final_date)
new_soil = new_soil.set_index('final_date')

new_soil_agg = new_soil.soil_moisture.resample('W').agg('mean')

new_soil_agg = new_soil_agg['1997-08-10':]

split_point_test = round(new_soil_agg.shape[0] * .66)
split_point_train_validate = round(split_point_test/2)

train = np.array(new_soil_agg[:split_point_train_validate])
validate = np.array(new_soil_agg[split_point_train_validate:split_point_test])
test = np.array(new_soil_agg[split_point_test:])

train = np.array(pd.Series(train).bfill())
validate = np.array(pd.Series(validate).bfill())
test = np.array(pd.Series(test).bfill())


# The actual model

window_size = 3
steps_away = 2
C = 32
gamma = .03125
kernel = 'rbf'

xtrain, ytrain = embed_data(train, window_size, steps_away)
xvalidate, yvalidate = embed_data(validate, window_size, steps_away)
xtest, ytest = embed_data(test, window_size, steps_away)
m = svm.SVR(kernel=kernel, C=C, gamma=gamma)

# drought_SVR is our fitted model. 
# We can pull it from this file into other files and used the predict method to make predictions.

drought_SVR = m.fit(xtrain, ytrain)

def predict_soil_moisture(current_date, array_of_values):
    """
    Takes in the date of the most recent weekly soil moisture record (as a string) 
    and a list of the three most recent weekly soil moisture values. 
    Returns the future date for the prediction and the actual prediction.
    """
    # future_date = pd.to_datetime(current_date)
    future_date = current_date + timedelta(days=14)
    prediction = drought_SVR.predict(array_of_values)[0]
    return future_date, prediction

def get_inputs_and_predict(year=datetime.datetime.today().year):
    list_of_years = [year]
    date, array = get_inputs(list_of_years)
    return predict_soil_moisture(date, array)
