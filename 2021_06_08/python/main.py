import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import altair as alt
from plotnine import *
import seaborn as sns


pd.set_option('expand_frame_repr', False)

fishing = pd.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv"
)
stocked = pd.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv"
)

fishing = fishing.loc[fishing['values'] >= 0.0]
# fishing['species'] = fishing['species'].str.replace(regex)
# help(pd.Series.str.replace)


fishing.head()
(fishing.groupby('lake')
    .count()
    .sort_values('lake', ascending=False)
    [['values']]
)

fishing[['year']].value_counts()

def summarize_fishing(df, col):
    """
    :param df: df to be summarized
    :param col: the column(s) to be grouped by
    :return: data frame of Summed Values and row counts
    """

    if len(col) > 1:
        counts_df = df.groupby(col).size().to_frame()
        counts_df.columns = ['n_obs']
    else:
        counts_df = df[col].value_counts().to_frame()

    sum_value = df.groupby(col)['values'].sum().to_frame()

    agg_df = (sum_value.join(counts_df)\
        .rename({'values':'total_production',
                 'count': 'n_obs'}, axis = 1)
        [['n_obs', 'total_production']]
    )
    agg_df = (agg_df.reset_index()
        .sort_values('total_production', ascending=False)
              )

    return agg_df

summarize_fishing(fishing, 'lake')
summarize_fishing(fishing, ['lake', 'decade'])


# stupid matplotlib
values_offset = fishing[['values']] + 1
values_offset.plot.hist(grid=True, bins = 30)
plt.title('histogram of values')
plt.xscale("log")
plt.show()

help(pd.Series.plot.hist)

# altair histogram
fishing['values_offset'] = fishing['values'] + 1

# altair trash
(alt.Chart(fishing.sample(5000, random_state=42069))
    .transform_filter(alt.datum.values_offset > 0)
    .mark_bar()
    .encode(
    alt.X("values_offset:Q",
          bin=True,
          scale=alt.Scale(type='symlog')),
    y = "count()"
    )
 .show()
 )

# plotnine ftw?
p = ggplot(data=fishing) +\
geom_histogram(mapping=aes(x='values_offset')) +\
scale_x_log10()

print(p)

# seaborn is maybe cool?
hist_plt = sns.histplot(fishing,x='values_offset', bins = 'fd')
hist_plt.set_xscale('log')
plt.show()

fishing[['lake']].value_counts()

agg_df1 = (fishing[['year' , 'lake', 'species', 'values', 'grand_total']]
    .groupby(['year', 'lake', 'species'])
    .agg(
        total_values = ('values', 'sum'),
        first_grand_total = ('grand_total', 'min'),
        n_grand_total = ('grand_total', pd.Series.nunique)
    )
    .reset_index(col_level = 0)
 )

# ties now
agg_df1[['n_grand_total']].value_counts()

fishing.loc[(fishing['year'] == 2000) &
            (fishing['lake'] == 'Erie') &
            (fishing['species'] == 'Carp')]


fishing['decade'] = (fishing['year'] // 10) * 10

# plotnine is what I feel most comfortable with
decade_summary = summarize_fishing(fishing, 'decade').reset_index()
decade_plot = ggplot(decade_summary) +\
    geom_col(aes(x='decade', y='total_production'))
print(decade_plot)

# seaborn
sns.set(rc={'figure.figsize':(11.7,8.27)})
sns.barplot(data=decade_summary,
            x='decade',
            y='total_production',
            color= 'grey')
plt.show()

# figure out the top 15
species_summary = summarize_fishing(fishing, 'species')
top_15 = species_summary.reset_index()[['species']].head(15)

# make a species_altered column
fishing['species_other'] = np.where(fishing['species'].isin(top_15['species'].values),
         fishing['species'],
         'other')

# get the top 15 by total_production
species_other_summary = summarize_fishing(fishing, ['decade', 'species_other'])

full_species_other = (species_other_summary.groupby('species_other')
    .agg({'decade': 'count'})
    .reset_index()
    .query('decade == 16')
 )

species_other_df = fishing.merge(full_species_other, on = 'species_other', how = 'inner')

species_other_plt = (
        ggplot(species_other_df, aes(x='decade', y='total_production', fill = 'species')) +
        geom_area() +
        facet_wrap('species_other')
                            )

print(species_other_plt)

# TODO: I'm plotting the wrong dataframe,
# cleanup the dataframe to be plotted and call it a day