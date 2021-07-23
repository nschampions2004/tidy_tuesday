import geopandas as gpd
import pandas as pd
import numpy as np
from matplotlib import pyplot as plt


data = pd.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv"
)

# initial check
data.head(1).T

# do any countries have more than one independence date they celebrate?
mo_than_one = (data
    .groupby('country')
    .agg({'year': 'count'})
    .sort_values('year', ascending = False)
    .query('year > 1')
    .index.to_numpy()
 )

# what's the difference in time between these country's multiple independence days?
mult_indep_countries = data[data['country'].isin(mo_than_one)]

diff = (mult_indep_countries
    .sort_values(['country', 'year'], ascending = True)
    .groupby('country')
    .agg({'year': 'diff'})
    .dropna()
    [['year']]
    .values
 )

# reshape a dataframe in by passing in it's length to np.reshape
diff = diff.reshape(diff.shape[0])

diff_independence = pd.DataFrame({'country': mo_than_one,
              'difference_between_independence': diff
    })

diff_independence.sort_values('difference_between_independence', ascending=False)


# when did most countries celebrate independence day?
data.hist('year')
plt.show()


# how can I make a map in Python
# country by latest year of independence

country = (data
    .dropna(subset = ['year'])
    .groupby('country')
    .agg({'year': 'max'})
    .reset_index()
)

country['country'].values
country['country'] = country['country'].str.replace('United States', 'United States of America')

# bring in geopandas data file
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))

merged_cnty = (world
    .merge(country,
           left_on = 'name',
           right_on = 'country',
           how = "left")
)

pd.set_option('expand_frame_repr', False)
merged_cnty
merged_cnty.info()
merged_cnty.head()


# okay with no null entires, we're ready to plot
merged_cnty = merged_cnty.loc[merged_cnty['year'].isnull(), 'year_char'] = 'No Data'

ax = merged_cnty.plot(column='year',
        cmap='viridis',
        figsize=(15,9),
        scheme='quantiles',
        k=6,
        legend=True,
        missing_kwds={'color': 'lightgrey',
                      "label": "Missing values"})

# ax.get_legend().set_bbox_to_anchor((.12,.12))
ax.get_figure()
plt.title('Latest Date of Independence by Country')
plt.show()


merged_cnty