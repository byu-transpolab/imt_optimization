
#
# Filename: delay_calculations.py
# Code to build delay csvs from average delay and link volumes
#
# Flags - 
#
# Code Purpose:
#
# Code History:
#
# Version: 0.01
# Date: Oct 18, 2023
# Author: Brynn Woolley, bwool00@byu.edu

import pandas as pd
import numpy as np
import os

# read in code csv that contains scenario information
data_directory = 'data'
file_path = os.path.join(data_directory, 'code_key.csv')
key = pd.read_csv(file_path)

seeds = key['Seed Value'].unique().tolist()
key.head()

# get unique list of seeds
seeds = sorted(seeds)
seed_dict = dict(zip(key['Seed Value'], key['Number of Incidents']))

# read in feeder csv
data_directory = 'data/feeder_links'
file_path = os.path.join(data_directory, 'feeders.csv')
feeders = pd.read_csv(file_path)


###---------- helper functions ---------- ###
def get_same_indices(link_vol_df, avg_delay_df):
    # Make sure link id's are unique
    vol_i, avg_del_i = set(link_vol_df.index), set(avg_delay_df.index)

    # Find the links that are unique to each DataFrame
    vol__i, avg_del__i = list(vol_i - avg_del_i), list(avg_del_i - vol_i)

    # Create a boolean masks to identify rows with Link IDs in avg_del__i_list
    mask = avg_delay_df.index.isin(avg_del__i)
    avg_delay_df = avg_delay_df[~mask]

    mask = link_vol_df.index.isin(vol__i)
    link_vol_df = link_vol_df[~mask]
    
    for j in range(len(link_vol_df)):
        if link_vol_df.index[j] != avg_delay_df.index[j]:
            raise ValueError(f'ERROR: link_vol: {link_vol_df.index[j]}, avg_delay: {avg_delay_df.index[j]}')


    if len(link_vol_df) != len(avg_delay_df):
        raise ValueError('oops something weird happend:\nlen(link_vol_df) != len(avg_delay_df)')
    
    else:
        return link_vol_df, avg_delay_df
    
""" This function only creates delay csv for the baseline scenario
    It is seperate, in part due to simplicity, but also for testing purposes"""
def save_baseline_delay():
    # get average delay
    filepath = 'data/average_delay'
    filename = os.path.join(filepath, 'Baseline/0-0-000.baseline.delays_perLink_100.csv')
    base_avg_delay = pd.read_csv(filename, index_col=0)


    # get link volumes
    filepath = 'data/link_volumes'
    filename = os.path.join(filepath, 'Baseline/0-0-000.volume.csv')
    base_link_vol = pd.read_csv(filename, index_col=0)


    # make sure they have the same indices & that they're in the same order
    base_link_vol, base_avg_delay = get_same_indices(base_link_vol, base_avg_delay)
    for i in range(len(base_link_vol)):
        if base_link_vol.index[i] != base_avg_delay.index[i]:
            print(f'ERROR: link_vol: {base_link_vol.index[i]}, avg_delay: {base_avg_delay.index[i]}')

    # try to get delay
    base_delay = base_avg_delay * base_link_vol.astype(float)

    # save it baby
    filepath = 'data/link_delays'
    filename = os.path.join(filepath, 'Baseline/0-0-000.delay.csv')
    os.makedirs(os.path.dirname(filename), exist_ok=True)
    base_delay.to_csv(filename)


def save_seed_delay():
    """
    Save delay data for multiple seeds and scenarios.
    
    Args:
        seeds (list): List of seed values.
        seed_dict (dict): A dictionary containing the number of incidents for each seed.
    """
    for seed in seeds:
        # get number of incidents for that seed
        n = seed_dict[seed]

        # make 'Seed ###' folders within Delay Folder
        base_directory = 'data/link_delays'
        seed_folder = os.path.join(base_directory, f'Seed {seed}')

        if not os.path.exists(seed_folder):
            os.makedirs(seed_folder)
        
        # iterate through i(mt_deployment) = 1, 2, 3
        for i in range(1,4):            
            # get average delay
            avg_filename = os.path.join('data/average_delay', f'Seed {seed}/{i}-{n}-{seed}.delays_perLink_100.csv')
            avg_df = pd.read_csv(avg_filename, index_col=0)
            
            # get link volumes
            vol_filename = os.path.join('data/link_volumes', f'Seed {seed}/{i}-{n}-{seed}.volume.csv')
            vol_df = pd.read_csv(vol_filename, index_col=0)

            # fix the column naming issue
            vol_df.columns = avg_df.columns.to_list()

            # ensure they have the same indices:
            link_vol_df, avg_delay_df = get_same_indices(vol_df, avg_df)
            link_vol_df, avg_delay_df = link_vol_df.astype(float), avg_delay_df.astype(float)

            # compute delay
            delay = link_vol_df * avg_delay_df

            # save it as a properly labeled csv
            delay_filename = os.path.join(seed_folder, f'{i}-{n}-{seed}.delay.csv')
            delay.to_csv(delay_filename)
