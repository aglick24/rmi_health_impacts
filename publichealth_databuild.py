import os
import glob
import csv
import pandas as pd
import collections
import numpy as np


def main():
    # READS IN STATE LEVEL CACES DATA FOR GROUND LEVEL AND ELEVATED POINT SOURCES CONTAINING DAMAGE BY COUNTY BY
    # POLLUTANT
    c_ground = pd.read_csv('CACES_county_groundlevel.csv')
    c_elevated = pd.read_csv('CACES_county_elevated.csv')

    # Read in NEI data
    n = pd.read_csv('NEI_county_2017.csv')

    # READS IN THE BRIDGE BETWEEN NEI AND CACES (ASK MORE ABOUT THIS) CONTAINS CO2e VALUES OF SECTORS
    g = pd.read_csv('GHG-NEI-CACES_bridge.csv')
    g = g[['ma_sector', 'MMT_CO2e']]

    # MERGES THE CACES DATAFRAMES
    c = pd.concat([c_ground, c_elevated])

    # RENAMES COLUMNS AND DROPS STATE_ABBR
    c = c.rename(columns={'fips': 'county'}).drop(columns='state_abbr')
    c.columns = ['county', 'pollutant', 'model', 'season', 'elevated', 'damage']

    # GETS RID OF NON-ANNUAL VALUES IN C
    c = c.loc[c['season'] == 'annual']

    # GROUPS C BY STATE, POLLUTANT, AND ELEVATED, THEN TAKES MEAN FOR EACH GROUP
    caces = c.groupby(by=['county', 'pollutant', 'season', 'elevated']).mean().reset_index()

    # PADS COUNTY STRING WITH 0s AND TACKS STATE FIPS ONTO THE FRONT (SAME FORMAT AS CACES)
    n['COUNTY_FIPS'] = n['COUNTY_FIPS'].astype(str).str.pad(width=3, side='left', fillchar='0')
    n['county'] = (n['STATE_FIPS'].astype(str) + n['COUNTY_FIPS']).astype(int)

    # RENAMES COLUMNS TO MATCH CACES AND ONLY KEEPS COLUMNS THAT WE WILL LOOK AT
    n = n.rename(columns={"SECTOR": "sector",
                          "POLLUTANT": "pollutant",
                          "POLLUTANT TYPE": "pollutant_type",
                          "EMISSIONS": "emissions"})
    n = n[['county', 'sector', 'pollutant', 'emissions']]

    # CONVERTS TO METRIC TONS
    n['emissions'] = n['emissions'] * 0.9071847

    # GETS RID OF LEAD
    n = n.loc[n['pollutant'] != 'Lead']

    # RENAMES POLLUTANTS TO BE IN-LINE WITH CACES
    n.loc[n['pollutant'] == "Ammonia", 'pollutant'] = "nh3"
    n.loc[n['pollutant'] == "Carbon Dioxide", 'pollutant'] = "co2"
    n.loc[n['pollutant'] == "Carbon Monoxide", 'pollutant'] = "co"
    n.loc[n['pollutant'] == "Volatile Organic Compounds", 'pollutant'] = "voc"
    n.loc[n['pollutant'] == "PM10 Primary (Filt + Cond)", 'pollutant'] = "pm10"
    n.loc[n['pollutant'] == "Nitrogen Oxides", 'pollutant'] = "nox"
    n.loc[n['pollutant'] == "Nitrous Oxide", 'pollutant'] = "nitrous oxide"
    n.loc[n['pollutant'] == "Sulfur Dioxide", 'pollutant'] = "so2"
    n.loc[n['pollutant'] == "Organic Carbon portion of PM2.5-PRI", 'pollutant'] = "organic carbon pm25"
    n.loc[n['pollutant'] == "Elemental Carbon portion of PM2.5-PRI", 'pollutant'] = "elemental carbon pm25"
    n.loc[n['pollutant'] == "PM2.5 Primary (Filt + Cond)", 'pollutant'] = "pm25"
    n.loc[n['pollutant'] == "Methane", 'pollutant'] = "ch4"

    # ASSIGNS SOURCES BASED ON SECTORS TO BE MORE BROAD AND IN-LINE WITH CACES
    n.loc[(n['sector'] == "Fuel Comb - Electric Generation - Coal") |
          (n['sector'] == "Fuel Comb - Electric Generation - Natural Gas") |
          (n['sector'] == "Fuel Comb - Electric Generation - Oil") |
          (n['sector'] == "Fuel Comb - Electric Generation - Other"), 'source'] = 'Electricity'

    n.loc[(n['sector'] == "Fuel Comb - Residential - Natural Gas") |
          (n['sector'] == "Fuel Comb - Residential - Oil") |
          (n['sector'] == "Fuel Comb - Residential - Other"), 'source'] = 'Residential'

    n.loc[(n['sector'] == "Fuel Comb - Comm/Institutional - Coal") |
          (n['sector'] == "Fuel Comb - Comm/Institutional - Natural Gas") |
          (n['sector'] == "Fuel Comb - Comm/Institutional - Oil") |
          (n['sector'] == "Fuel Comb - Comm/Institutional - Biomass") |
          (n['sector'] == "Fuel Comb - Comm/Institutional - Other"), 'source'] = 'Commercial/Industrial'

    # FILTERS OUT HIGH STACK INDUSTRIAL TO USE ELEVATED DAMAGES LATER
    n.loc[(n['sector'] == "Fuel Comb - Industrial Boilers, ICEs - Coal") |
          (n['sector'] == "Fuel Comb - Industrial Boilers, ICEs - Natural Gas") |
          (n['sector'] == "Fuel Comb - Industrial Boilers, ICEs - Oil") |
          (n[
               'sector'] == "Fuel Comb - Industrial Boilers, ICEs - Biomass"), 'source'] = 'Commercial/Industrial high stack'

    n.loc[(n['sector'] == "Mobile - On-Road non-Diesel Heavy Duty Vehicles") |
          (n['sector'] == "Mobile - On-Road non-Diesel Light Duty Vehicles") |
          (n['sector'] == "Gas Stations") |
          (n['sector'] == "Bulk Gasoline Terminals") |
          (n['sector'] == "Mobile - On-Road Diesel Heavy Duty Vehicles") |
          (n['sector'] == "Mobile - On-Road Diesel Light Duty Vehicles") |
          (n['sector'] == "Mobile - Commercial Marine Vessels") |
          (n['sector'] == "Dust - Paved Road Dust") |
          (n['sector'] == "Mobile - Locomotives"), 'source'] = 'Transportation'

    # FILTERS OUT HIGH STACK TRANSPORTATION TO USE ELEVATED DAMAGES LATER
    n.loc[(n['sector'] == "Mobile - Aircraft"), 'source'] = 'Transportation high stack'

    n.loc[(n['sector'] == "Industrial Processes - Storage and Transfer"), 'source'] = 'Fossil Fuel Industry'

    n.loc[(n['sector'] == "Industrial Processes - Cement Manuf") |
          (n['sector'] == "Industrial Processes - Chemical Manuf") |
          (n['sector'] == "Industrial Processes - Ferrous Metals") |
          (n['sector'] == "Industrial Processes - NEC") |
          (n['sector'] == "Industrial Processes - Non-ferrous Metals") |
          (n['sector'] == "Industrial Processes - Petroleum Refineries") |
          (n['sector'] == "Industrial Processes - Pulp & Paper") |
          (n['sector'] == "Miscellaneous Non-Industrial NEC"), 'source'] = 'Industrial Processes'

    n.loc[(n['sector'] == "Agriculture - Livestock Wast") |
          (n['sector'] == "Agriculture - Fertilizer Application"), 'source'] = 'Agriculture'

    """ list of sectors not given a source:
    ['Mobile - Non-Road Equipment - Diesel'
     'Mobile - Non-Road Equipment - Gasoline'
     'Mobile - Non-Road Equipment - Other' 'Biogenics - Vegetation and Soil'
     'Fuel Comb - Residential - Wood' 'Waste Disposal'
     'Solvent - Industrial Surface Coating & Solvent Use' 'Fires - Wildfires'
     'Solvent - Consumer & Commercial Solvent Use'
     'Agriculture - Crops & Livestock Dust' 'Dust - Construction Dust'
     'Dust - Unpaved Road Dust' 'Solvent - Non-Industrial Surface Coating'
     'Agriculture - Livestock Waste' 'Solvent - Degreasing'
     'Commercial Cooking' 'Industrial Processes - Mining'
     'Solvent - Graphic Arts' 'Fuel Comb - Electric Generation - Biomass'
     'Fires - Prescribed Fires' 'Fuel Comb - Industrial Boilers, ICEs - Other'
     'Solvent - Dry Cleaning' 'Industrial Processes - Oil & Gas Production']"""

    # REMOVES ROWS NOT ASSIGNED A SOURCE
    n = n.loc[~n['source'].isna()]

    nei_caces = pd.merge(n, caces[['county', 'pollutant', 'elevated', 'damage']], how="inner",
                         on=["county", "pollutant"])

    # SEPARATES OUT HIGH STACK AND GROUND LEVEL SOURCES
    ground_level = nei_caces.loc[((nei_caces['source'] == 'Agriculture') |
                                  (nei_caces['source'] == 'Residential') |
                                  (nei_caces['source'] == 'Commercial/Industrial') |
                                  (nei_caces['source'] == 'Fossil Fuel Industry') |
                                  (nei_caces['source'] == 'Transportation')) &
                                 (nei_caces['elevated'] == 'ground level')]

    highstack = nei_caces.loc[((nei_caces['source'] == 'Industrial Processes') |
                               (nei_caces['source'] == 'Electricity') |
                               (nei_caces['source'] == 'Commercial/Industrial high stack') |
                               (nei_caces['source'] == 'Fossil Fuel Industry') |
                               (nei_caces['source'] == 'Transportation high stack')) &
                              (nei_caces['elevated'] == 'high stack')]

    # RECOMBINES THE GROUND LEVEL AND HIGH STACK SOURCES
    d = pd.concat([ground_level, highstack])

    # RENAMES THE TWO HIGH STACK SOURCES TO NORMAL
    d.loc[d['sector'] == 'Commercial/Industrial high stack', 'sector'] = 'Commercial/Industrial'
    d.loc[d['sector'] == 'Transportation high stack', 'sector'] = 'Transportation'

    # REORGANIZES THE COLUMNS
    d = d[['county', 'pollutant', 'source', 'sector', 'elevated', 'emissions', 'damage']]

    # print(d['county'].unique())
    # print(d['pollutant'].unique())
    # print(d.loc[(d['county'] == 25017) & (d['pollutant'] == 'nh3')][['source', 'emissions']])

    electricity = pd.DataFrame(columns=list(d.columns) + ['perc'])
    industrial = pd.DataFrame(columns=list(d.columns) + ['perc'])
    residential = pd.DataFrame(columns=list(d.columns) + ['perc'])

    for pollutant in d['pollutant'].unique():
        for county in d['county'].unique():
            elec = d.loc[(d['source'] == "Electricity") & (d['sector'] != "Fuel Comb - Electric Generation - Other") &
                         (d['county'] == county) & (d['pollutant'] == pollutant)].copy()
            elec['perc'] = elec['emissions'] / elec['emissions'].sum()
            electricity = pd.concat([electricity, elec])

            ind = d.loc[(d['source'] == "Commercial/Industrial") & (d['elevated'] != "high stack") &
                        (d['sector'] != "Fuel Comb - Comm/Institutional - Other") & (d['county'] == county) &
                        (d['pollutant'] == pollutant)].copy()
            ind['perc'] = ind['emissions'] / ind['emissions'].sum()
            industrial = pd.concat([industrial, ind])

            res = d.loc[(d['source'] == "Residential") & (d['sector'] != "Fuel Comb - Residential - Other") &
                        (d['county'] == county) & (d['pollutant'] == pollutant)].copy()
            res['perc'] = res['emissions'] / res['emissions'].sum()
            residential = pd.concat([residential, res])

    source = pd.concat([electricity, industrial, residential])

    dd = pd.merge(d, source[["county", "pollutant", "sector", "emissions", "damage"]], on=["county", "pollutant", "sector"])
    print(source.columns)
    print(d.columns)
    dd.to_csv('./')

    for pollutant in d['pollutant'].unique():
        for county in d['county'].unique():
            d.loc[(d['source'] == "Commercial/Industrial") & (d['pollutant'] == pollutant) & (d['county'] == county) & (
                        d['elevated'] != "high stack"), 'emissions_other'] = \
                d.loc[(d['sector'] == "Fuel Comb - Comm/Institutional - Other") & (d['pollutant'] == pollutant) & (
                            d['county'] == county)]['emissions']
            d.loc[(d['source'] == "Residential") & (d['pollutant'] == pollutant) & (d['county'] == county) & (
                        d['elevated'] != "high stack"), 'emissions_other'] = \
                d.loc[(d['sector'] == "Fuel Comb - Comm/Institutional - Other") & (d['pollutant'] == pollutant) & (
                            d['county'] == county)]['emissions']


if __name__ == "__main__":
    main()
