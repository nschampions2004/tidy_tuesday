
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import re
import seaborn as sns
import plotly
from os import listdir

us_state_abbrev = {
    'Alabama': 'AL',
    'Alaska': 'AK',
    'American Samoa': 'AS',
    'Arizona': 'AZ',
    'Arkansas': 'AR',
    'California': 'CA',
    'Colorado': 'CO',
    'Connecticut': 'CT',
    'Delaware': 'DE',
    'District of Columbia': 'DC',
    'Florida': 'FL',
    'Georgia': 'GA',
    'Guam': 'GU',
    'Hawaii': 'HI',
    'Idaho': 'ID',
    'Illinois': 'IL',
    'Indiana': 'IN',
    'Iowa': 'IA',
    'Kansas': 'KS',
    'Kentucky': 'KY',
    'Louisiana': 'LA',
    'Maine': 'ME',
    'Maryland': 'MD',
    'Massachusetts': 'MA',
    'Michigan': 'MI',
    'Minnesota': 'MN',
    'Mississippi': 'MS',
    'Missouri': 'MO',
    'Montana': 'MT',
    'Nebraska': 'NE',
    'Nevada': 'NV',
    'New Hampshire': 'NH',
    'New Jersey': 'NJ',
    'New Mexico': 'NM',
    'New York': 'NY',
    'North Carolina': 'NC',
    'North Dakota': 'ND',
    'Northern Mariana Islands':'MP',
    'Ohio': 'OH',
    'Oklahoma': 'OK',
    'Oregon': 'OR',
    'Pennsylvania': 'PA',
    'Puerto Rico': 'PR',
    'Rhode Island': 'RI',
    'South Carolina': 'SC',
    'South Dakota': 'SD',
    'Tennessee': 'TN',
    'Texas': 'TX',
    'Utah': 'UT',
    'Vermont': 'VT',
    'Virgin Islands': 'VI',
    'Virginia': 'VA',
    'Washington': 'WA',
    'West Virginia': 'WV',
    'Wisconsin': 'WI',
    'Wyoming': 'WY'
}

abbrev_us_state = dict(map(reversed, us_state_abbrev.items()))


data_folder = 'data'
broadband = 'broadband.csv'
broadband_zip = 'broadband_zip.csv'

bband = pd.read_csv(Path(data_folder) / broadband)
bband_zip = pd.read_csv(Path(data_folder) / broadband_zip)

for df in [bband, bband_zip]:
    df.columns = [str.lower(str.replace(col, ' ', '_')) for col in df.columns]
    df.columns = [re.sub('\+\/\-', '', re.sub('\(|\)', '', col)) for col in df.columns]
    df.columns = [re.sub('\\%', '', col) for col in df.columns]
    df['state']  = [abbrev_us_state[val] for val in df['st']]

for col in ['broadband_usage', 'broadband_availability_per_fcc']:
    bband[col] = bband[col].replace('\\-', np.nan, regex = True)
    bband[col] = bband[col].astype(float)


bband = bband.rename(columns = {'county_id': 'fips'})
bband_zip = bband_zip.rename(columns = {'county_id': 'fips'})

twenty_tens = [str(i + 2010) for i in range(10)]
columns_excel = ['county', 'census', 'estimated_base'] + twenty_tens
cty_pop = pd.read_excel(Path(data_folder) / 'co-est2019-annres.xlsx',
                                        skiprows=3,
                                        nrows=3143,
                                        names=columns_excel)

cty_pop = cty_pop.loc[:, ['county', '2017']]
cty_pop = cty_pop.rename(columns = {'2017': 'population_2017' })
cty_pop['county_name'], cty_pop['state'] = cty_pop['county'].str.split(',', 1).str
cty_pop = cty_pop.loc[:, ['county_name', 'state', 'population_2017']]
cty_pop['state'] = cty_pop['state'].str.strip()
cty_pop = cty_pop.dropna(subset = ['state'])
cty_pop['county_name'] = cty_pop['county_name'].str.replace('^\\.', '',regex = True)

bband_cty = bband.merge(cty_pop, how = 'left', on = ['county_name', 'state'])
# there aren't many here that are null.  dropping for now
# bband_cty[bband_cty['population_2017'].isnull()]

bband_cty = bband.merge(cty_pop, how = 'inner', on = ['county_name', 'state'])
bband_cty = bband_cty.loc[:, ['county_name', 'state', 'fips', 'population_2017', 'broadband_availability_per_fcc', 'broadband_usage']]
bband_cty['county_name'] = bband_cty['county_name'].str.replace(' County|Parish', '')
bband_cty['county'] = bband_cty['county_name'] + ', ' + bband_cty['state']

# general sorting and exploring
bband_cty.sort_values(by = 'broadband_availability_per_fcc', ascending = False)
bband_cty.sort_values(by = 'broadband_usage', ascending = False)
bband_cty.sort_values(by = 'population_2017', ascending = False)

def top_plotter(df, how_many, col_to_sort_by, plot = 'bar', ascending = False):
    "plot how_many number of bars to rank counties by col_to_sort_by"

    df = df.sort_values(col_to_sort_by, ascending=ascending)
    df = df.head(how_many)

    plt.figure(figsize=(16,8))
    if plot == 'bar':
        ax = sns.barplot(x = col_to_sort_by, y = 'county', color = 'grey', data = df)
    elif plot == 'point':
        ax = sns.scatterplot(x = col_to_sort_by, y = 'county', color = 'grey', data = df)
    return ax


# top_plotter(bband_cty, 25, 'broadband_availability_per_fcc', plot = 'bar')
# plt.show()
#
# top_plotter(bband_cty, 25, 'broadband_usage', plot = 'bar')
# plt.show()

# top_plotter(bband_cty, 25, 'broadband_usage', plot = 'point')
# top_plotter(bband_cty, 40, 'broadband_availability_per_fcc', plot = 'point')
