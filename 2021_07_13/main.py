import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

pd.set_option('expand_frame_repr', False)

scooby_doo = pd.read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv'
)


# I want to see suspects over time
scooby_doo['date_aired'] = pd.to_datetime(scooby_doo['date_aired'])



# which network


import datetime as dt


series = plt.
ax.vline(dt.datetime(2021, 7, 12, 21, 2, 0), color = 'r')