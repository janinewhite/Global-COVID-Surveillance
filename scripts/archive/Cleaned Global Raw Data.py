#!/usr/bin/env python
# coding: utf-8

# In[1]:


# Dependencies
get_ipython().run_line_magic('matplotlib', 'inline')
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import requests
import io
import time
from datetime import datetime, timedelta
import json
import pprint
pp = pprint.PrettyPrinter(indent=4)
pd.set_option('display.max_columns', None)


# In[2]:


#Folders and Files

# Folders
repositoryFolder = "D:/Repositories/Global-COVID-Surveillance/data/"
localDownloadFolder = "C:/Users/janin/Downloads/"
demographicsFolder = repositoryFolder + "raw/demographics/"
configuredFolder = repositoryFolder + "configured/"
cleanedFolder = repositoryFolder + "cleaned/"
regionsFolder = repositoryFolder + "raw/regions/"
locationsFolder = repositoryFolder + "raw/locations/"

# Population Input Files
global_population_input_file = demographicsFolder + "Country Populations 2020.xlsx"
us_codes = demographicsFolder + "US State Codes.xlsx"

# Population Output Files
canada_population_file = demographicsFolder + "Canada Population.xlsx"
us_population_file = demographicsFolder + "US Population.xlsx"
all_populations_file = cleanedFolder + "Populations_cleaned.xlsx"

# Location Input
locations_file = demographicsFolder + "Country Geo.xlsx"

# R Files
south_africa_r = regionsFolder + "SSA-Temp.xlsx"
south_asia_r = regionsFolder + "SouthAsia excel updated 20201008.xlsx"
latin_america_r = regionsFolder + "LatinAmerica.xlsx"
central_asia_r = regionsFolder + "CentralAsia-Results.xlsx"
europe_r = regionsFolder + "Europe-Results-Updated.xlsx"
middle_east_r = regionsFolder + "Middle East Output.xlsx"
east_asia_pacific_r = regionsFolder + "East Asia and Pacific output.xlsx"
canada_r = regionsFolder + "_Canada Ouput.xlsx"
us_r = regionsFolder + "USState-Results.xlsx"


# In[3]:


#Sources
population_source_url ="https://www.worldometers.info/world-population/population-by-country/"
canada_population_source = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901" # Statistics Canada Quarterly Population
us_population_source = "https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-agesex-civ.csv"

github_url="https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/processed/data_all.csv?raw=true"
country_codes_coordinates = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/countries_codes_and_coordinates.csv"
countries_geo = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/countries.geo.json"
us_states_geo = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/us-states.geo.json"

kaggle_locations = "https://www.kaggle.com/paultimothymooney/latitude-and-longitude-for-every-country-and-state"

canada_source_csv = "https://opendata.arcgis.com/datasets/3afa9ce11b8842cb889714611e6f3076_0.csv"
us_source_csv = "https://covidtracking.com/data/download/all-states-history.csv"


# In[4]:


# Countries and Regions

european_countries = [
    'Albania','Andorra','Austria','Belarus','Belgium','Bosnia & Herzegovina','Bulgaria',
    'Croatia','Czech Republic','Denmark','Estonia','Finland','France',
    'Germany','Greece','Greenland','Hungary','Iceland','Ireland','Isle of Man','Italy',
    'Latvia','Liechtenstein','Lithuania','Luxembourg','Malta','Moldova','Monaco','Montenegro',
    'Netherlands','Norway','Poland','Portugal','Romania',
    'San Marino','Serbia','Slovakia','Slovenia','Spain','Sweden','Switzerland',
    'Ukraine','United Kingdom','Vatican City'
]

north_american_countries = ["Canada","United States"]

carribean_countries = [
    "Antigua & Barbuda","Aruba","Bahamas","Barbados","Bermuda","British Virgin Islands",
    "Cayman Islands","Cuba","Curacao","Dominica","Dominican Republic","Grenada",
    "Haiti","Jamaica","Puerto Rico","St. Kitts & Nevis","St. Lucia","St. Vincent & Grenadines",
    "Sint Maarten","Trinidad & Tobago","Turks and Caicos Islands","United States Virgin Islands"
]
central_south_america_countries = [
    'Argentina','Belize','Bolivia','Brazil','Chile','Colombia','Costa Rica',
    'Ecuador','El Salvador','Guatemala','Guyana','Honduras',
    'Mexico','Nicaragua','Panama','Paraguay','Peru','Suriname','Uruguay','Venezuela'
]
latin_american_countries = carribean_countries + central_south_america_countries
american_countries = north_american_countries + latin_american_countries

south_asia_countries = [
    "Afghanistan","Bangladesh","Bhutan","India","Maldives","Nepal","Pakistan","Sri Lanka"
]
central_asian_countries = [
    'Armenia','Azerbaijan','Cyprus','Faeroe Islands','Georgia','Gibraltar','Kazakhstan','Kosovo','Kyrgyzstan',
    'North Macedonia','Russia','Tajikistan','Turkey','Turkmenistan','Uzbekistan'
]
east_asian_countries = [
    "Brunei","Cambodia","China","Indonesia","Japan","Laos","Malaysia","Mongolia","Myanmar","Niue","North Korea","Philippines",
    "Singapore","South Korea","Taiwan","Thailand","Timor","Vietnam"
]
pacific_countries = [
    "Australia","Cook Islands","Fiji","French Polynesia","Guam","Kiribati",
    "Marshall Islands","Micronesia","Nauru","New Caledonia","New Zealand",
    "Northern Mariana Islands","Palau","Papua New Guinea","Samoa","Solomon Islands","Tonga","Tuvalu","Vanuatu"
]
east_asia_and_pacific_countries = east_asian_countries + pacific_countries

middle_eastern_countries = [
    "Bahrain","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Qatar","Saudi Arabia","Syria",
    "United Arab Emirates","Yemen"
]
north_african_countries = [
    "Algeria","Djibouti","Egypt","Libya","Morocco","Tunisia","Western Sahara"
]
middle_east_and_north_africa_countries = middle_eastern_countries + north_african_countries

sub_saharan_african_countries = [
    "Angola","Benin","Botswana","Burkina Faso","Burundi",
    "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros","Côte d’Ivoire",
    "Democratic Republic of Congo","Equatorial Guinea","Eritrea","Ethiopia",
    "Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","Lesotho","Liberia",
    "Madagascar","Malawi","Mali","Mauritania","Mauritius","Mozambique",
    "Namibia","Niger","Nigeria","Republic of the Congo","Rwanda",
    "São Tomé and Príncipe","Senegal","Seychelles","Sierra Leone",
    "Somalia","South Africa","South Sudan","Sudan","Swaziland",
    "Tanzania","Togo","Uganda","Zambia","Zimbabwe"
]

unincorporated_disputed_territories = [
    "American Samoa", "Anguilla","Caribbean Netherlands","Channel Islands","Curaçao",
    "Falkland Islands","French Guiana","Guadeloupe","Hong Kong"
]

country_lists = [
    central_asian_countries,
    east_asia_and_pacific_countries,
    european_countries,
    latin_american_countries,
    middle_east_and_north_africa_countries,
    north_american_countries,
    sub_saharan_african_countries,
    south_asia_countries
]

all_countries = []
for country_list in country_lists:
    all_countries = all_countries + country_list
all_countries.sort()

regions = [
    'Central Asia',
    'East Asia and Pacific',
    'Europe',
    'Latin America',
    'Middle East and North Africa',
    'North America',
    'South Asia',
    'Sub-Saharan Africa',
    'Territory'
]

countries_by_region = {
    'Central Asia': central_asian_countries,
    'Europe': european_countries,
    'Latin America': latin_american_countries,
    'South Asia': south_asia_countries,
    'Sub-Saharan Africa': sub_saharan_african_countries,
    'Middle East and North Africa': middle_east_and_north_africa_countries,
    'East Asia and Pacific': east_asia_and_pacific_countries,
    'North America': north_american_countries,
    'Territory': unincorporated_disputed_territories
}

country_conversions = {
    "Antigua & Barbuda": ["Antigua and Barbuda"],
    "Bahamas": ["Bahamas, The"],
    "Bosnia & Herzegovina": ["Bosnia and Herzegovina"],
    "Brunei": ["Brunei Darussalam"],
    "Cabo Verde": ["Cape Verde"],
    "Côte d’Ivoire": ["Cote d'Ivoire","Cote dIvoire"],
    "Czech Republic": ["Czechia","Czech Republic (Czechia)"],
    "Democratic Republic of Congo": ["Congo - Kinshasa"],
    "Egypt": ["Egypt, Arab Rep."],
    "Faeroe Islands": ["Faroe Islands"],
    "Gambia": ["Gambia, The"],
    "Hong Kong" : ["Hong Kong SAR China"],
    "Iran": ["Iran, Islamic Rep."],
    "Kyrgyzstan": ["Kyrgyz Republic"],
    "Laos": ["Lao PDR"],
    "Micronesia": ["Micronesia, Fed. Sts."],
    "Myanmar": ["Myanmar (Burma)","Burma"],
    "North Macedonia": ["Macedonia"],
    "State of Palestine": ["Palestinian Territories"],
    "Republic of the Congo": ["Congo - Brazzaville"],
    "Russia": ["Russian Federation"],
    "São Tomé and Príncipe": ["Sao Tome and Principe","Sao Tome & Príncipe","São Tomé & Príncipe"],
    "Sint Maarten": ["Sint Maarten (Dutch part)"],
    "Slovakia": ["Slovak Republic"],
    "St. Kitts & Nevis": ["Saint Kitts and Nevis"],
    "St. Lucia": ["Saint Lucia"],
    "St. Vincent & Grenadines": ["Saint Vincent and the Grenadines"],
    "Swaziland": ["Eswatini"],
    "Syria": ["Syrian Arab Republic"],
    "Timor": ["Timor-Leste"],
    "Trinidad & Tobago": ["Trinidad and Tobago"],
    "Vatican City": ["Holy See"],
    "Yemen": ["Yemen, Rep."],
    "" : ["nan"]
}

census_regions = {
    0: {"name" : "United States",
        "states" : ["United States"]},
    1: {"name" : "Northeast",
        "states" :["Connecticut", "Maine", "New Hampshire", "Vermont", "Massachusetts", 
                   "Rhode Island", "New Jersey", "New York", "Pennsylvania"]},
    3: {"name" : "South",
        "states" : ["Maryland", "Delaware", "West Virginia", "Virginia", "Kentucky", 
                    "Tennessee", "North Carolina", "South Carolina", "Georgia", "Florida", 
                    "Alabama", "Mississippi", "Arkansas", "Louisiana", "Oklahoma", "Texas", 
                    "District of Columbia", "Puerto Rico"]},
    2: {"name" : "Midwest",
        "states" : ["North Dakota", "South Dakota", "Nebraska", "Kansas", "Missouri", "Iowa", 
                    "Minnesota", "Wisconsin", "Illinois", "Michigan", "Indiana", "Ohio"]},
    4: {"name" : "West",
        "states" : ["Washington", "Idaho", "Montana", "Wyoming", "Oregon", "California", "Nevada", 
                    "Utah", "Colorado", "Arizona", "New Mexico", "Alaska", "Hawaii"]}
}

us_states = [
    'Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','District of Columbia',
    'Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana',
    'Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska',
    'Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota',
    'Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas',
    'Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming'
]
canada_provinces = [
    'Alberta','British Columbia','Manitoba','New Brunswick','Newfoundland and Labrador','Northwest Territories',
    'Nova Scotia','Ontario','Prince Edward Island','Quebec','Saskatchewan','Yukon'
]
states_and_provinces = us_states + canada_provinces


# In[5]:


#Functions
def titleCase(words):
    if len(words) > 3:
        titlecased = ""
        wordsArray = words.lower().split(" ")
        for word in wordsArray:
            if len(titlecased) > 0 :
                titlecased = titlecased + " "
            if word == "and":
                titlecased = titlecased + "and"
            else:
                titlecased = titlecased + word.capitalize()
        return titlecased
    else:
        return words.upper()

def fixCensusRegion(code):
    region_name = ""
    for region in census_regions:
        if region["number"] == code:
            region_name = region["name"]
            break
    if region_name == "":
        region_name = "Other"
        print(str(code) + " not found")
    return region_name

# CDC Standard age ranges 0-17, 18-29, 30-49, and 50-64
# CDC COVID Reporting Age Ranges https://www.cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm
def getAgeRange(age):
    age_range = ""
    if age == 0:
        age_range = "< 1"
    elif age == 999:
        age_range = "Total"
    elif age < 5:
        age_range = "1-4"
    elif age < 15:
        age_range = "5-14"
    elif age < 25:
        age_range = "15-24"
    elif age < 35:
        age_range = "25-34"
    elif age < 45:
        age_range = "35-44"
    elif age < 55:
        age_range = "45-54"
    elif age < 65:
        age_range = "55-64"
    elif age < 75:
        age_range = "65-74"
    elif age < 85:
        age_range = "75-84"
    elif age == 85:
        age_range = "85+"
    return age_range

def fixSex(code):
    sex = ""
    if code == 0:
        sex = "Population 2019"
    elif code == 1:
        sex = "Male"
    elif code == 2:
        sex = "Female"
    else:
        print(str(code) + " is not a sex")
    return sex

def us_date(x):
    month = x[5:7]
    day = x[8:11]
    year = x[0:4]
    conversion = month + "/" + day +"/"+ year
    return conversion

def removeDecimal(data):
    strData = str(data)
    decimalLocation = strData.find(".")
    if decimalLocation > -1:
        return strData[0:decimalLocation]
    else:
        return strData

def emptyNan(value):
    if (value == "nan"):
        return ""
    else:
        return value

def printColumns(df, label):
    print(label)
    print(df.columns)

def print_column_unique(column):
    print("Column Values:")
    values = column.sort_values(ascending = True).unique()
    print(values)
    return values

def print_column_missing(column, comparison):
    values = print_column_unique(column)
    print("Comparison:")
    print(comparison)
    missing_values = []
    for value in values:
        if not value in comparison:
            missing_values.append(value)
    if len(missing_values) > 0:
        print("Column values not in comparison:")
        print(missing_values)
    else:
        print("No missing values")
    missing_values = []
    for value in comparison:
        if not value in values:
            missing_values.append(value)
    if len(missing_values) > 0:
        print("Comparison values not in column:")
        print(missing_values)
    else:
        print("No missing values")
    return values

def key_from_value(value, dictionary, default):
    return_value = default
    for key, values in dictionary.items():
        if value.strip() in values:
            return_value = key
            break
    return return_value.strip()

def fixRegion(country):
    return key_from_value(country, countries_by_region, "Territory")

def checkRegions(regionColumn, countryColumn):
    fixed = []
    for i in range(0,len(regionColumn)):
        region = regionColumn[i]
        country = countryColumn[i]
        if (not (region in regions)) and (not (country in fixed)):
            fixed.append(country)
            print(f"{country} = {region}")

def fixCountry(value):
    return key_from_value(value, country_conversions, value)

def checkCountries(column):
    fixed = []
    for value in column:
        fixedValue = fixCountry(value)
        if (not (fixedValue == value)) and (not (value in fixed)):
            fixed.append(value)
            print(f"{value} => {fixedValue}")

def fixLevel(country, state):
    level = ""
    if state in states_and_provinces:
        level = "State/Province"
    elif country in all_countries:
        level = "Country"
    elif country in ["",None,np.nan]:
        level = "Region"
    else:
        level = "Territory"
    return level

def fixCountries(countries_column, configuredCountries):
    countries_conversion = countries_column.astype(str)
    countries_conversion = countries_conversion.apply(lambda x: fixCountry(x))
    print(conversions)
    countries = print_column_missing(countries_conversion,configuredCountries)
    return countries_conversion

def testConversion(title, test_array, conversion):
    print(title)
    no_conversions = []
    for value in test_array:
        return_value = ""
        if conversion == "country":
            return_value = fixCountry(value)
        elif conversion == "region":
            return_value = region_from_country(fixCountry(value))
        if return_value != value.strip():
            print(value.strip() + "," + return_value)
        if return_value == "":
            no_conversions.append(value)
    if len(no_conversions) > 0:
        print("Missing Conversions")
        print(no_conversions)
    print("")
    
def division(a,b):
    if not (b == 0):
        return a/b
    else:
        return np.nan
    
def fixProvince(value):
    province_map = {
        'BC': 'British Columbia',
        'NL': 'Newfoundland and Labrador',
        'NWT': 'Northwest Territories',
        'PEI': 'Prince Edward Island',
        'Repatriated': 'Repatriated Canada',
        'Repatriated Cdn': 'Repatriated Canada'
    }
    value = titleCase(value)
    if value in province_map.keys():
        new_province = province_map[value]
        return new_province
    else:
        return value

def censusRegionByState(state):
    for key in census_regions.keys():
        if (state in census_regions[key]["states"]):
            return census_regions[key]["name"]
        
def fixUSRegion(code):
    region = census_regions[code]
    region_name = region["name"]
    return region_name


# In[6]:


#Global Populations
country_populations = pd.read_excel(global_population_input_file)
country_populations["Population 100K"] = country_populations["Population"]/100000
country_populations["Population Source"] = population_source_url
conversions = {}
country_populations["Country"] = country_populations["Country"].astype(str)
country_populations["Country"] = country_populations["Country"].apply(lambda x: fixCountry(x))
country_populations_order = [
    'Country', 'Population', 'Population 100K', 'Fertility Rate', 'Median Age',
    'World Share (%)', 'Urban Population (%)', 'Annual Change (%)', 'Net Change', 'Migrants (net)',
    'Density (P/Km²)', 'Land Area (Km²)']
country_populations = country_populations[country_populations_order]
country_populations.head()


# In[7]:


# Canada Population
population_cn = pd.read_excel(canada_population_file)
population_cn.rename(columns = {"GEO": "State/Province",
                                "VALUE": "Population",
                                "REF_DATE": "Quarter"}, inplace = True)
population_cn = population_cn[["Quarter","State/Province","Population"]]
last_quarter = population_cn["Quarter"].max()
print("Canada Populations " + last_quarter)
canada_last_population = population_cn.loc[population_cn["Quarter"]==last_quarter].copy()[["State/Province","Population"]]
canada_last_population["Population 100K"] = canada_last_population["Population"]/100000
canada_last_population.reset_index(drop=True,inplace=True)
canada_last_population["Country"] = "Canada"
canada_last_population["State/Province"] = canada_last_population["State/Province"].apply(lambda x: "" if x=="Canada" else x)
canada_last_population.head(20)


# In[8]:


us_state_codes = pd.read_excel(us_codes)
us_state_codes["Census Region"] = us_state_codes["State Name"].apply(lambda x: censusRegionByState(x))
us_state_codes.rename(columns = {
    'State Name':'State/Province',
    "State Abbreviation": "Abbreviation"
}, inplace = True)
us_state_codes.head(70)


# In[9]:


# US Population
us_states_census_demographics_request = requests.get(us_population_source).content
us_demographics = pd.read_csv(io.StringIO(us_states_census_demographics_request.decode('utf-8')))
currentTime = datetime.now()
us_demographics["Downloaded"] = currentTime
us_demographics["Country"] = "United States"
us_demographics["REGION"] = us_demographics["REGION"].apply(lambda x: fixUSRegion(x))
us_demographics["SEX"] = us_demographics["SEX"].apply(lambda x: fixSex(x))
us_demographics["Age Range"] = us_demographics["AGE"].apply(lambda x: getAgeRange(x))
keep_columns = ["REGION","STATE","NAME","SEX","AGE","POPEST2019_CIV","Downloaded","Country", "Age Range"]
us_demographics = us_demographics[keep_columns]
us_demographics.rename(columns = {'REGION': 'Census Region',
                                  'NAME' : 'State Name',
                                  'STATE' : 'FIPS',
                                  'POPEST2019_CIV' : 'Population 2019',
                                  'SEX' : 'Sex',
                                  'AGE' : 'Age'}, 
                       inplace = True)

us_sex = us_demographics.drop(columns=["Age Range"]).loc[us_demographics["Age"]==999].copy()
us_sex = us_sex.pivot_table(
    index=["Downloaded","Country","Census Region","State Name","FIPS","Sex"],
    columns='Age',
    values = 'Population 2019',
    aggfunc='first'
).reset_index().rename_axis(None, axis=1)
us_sex["Total Population"] = us_sex[999]
us_sex = us_sex.sort_values(["FIPS", "Sex"])
us_sex = us_sex.drop(columns=[999])
us_sex = us_sex.pivot_table(
    index=["Downloaded","Country","Census Region","State Name","FIPS"],
    columns='Sex',
    values = 'Total Population',
    aggfunc='first'
).reset_index().rename_axis(None, axis=1)
print(us_sex.columns)
us_sex["Pct Male"] = us_sex["Male"]/us_sex["Population 2019"]
us_sex["Pct Female"] = us_sex["Female"]/us_sex["Population 2019"]
us_sex = us_sex.sort_values(["FIPS"])

us_age = us_demographics[["Census Region","FIPS","State Name","Age", "Age Range", "Population 2019"]].copy()
us_age = us_age.pivot_table(index=["Census Region","FIPS","State Name"], 
                      columns='Age', 
                      values='Population 2019', 
                      aggfunc='first').reset_index().rename_axis(None, axis=1)
us_age["Total Population"] = us_age[999]
us_age["< 1"] = us_age[0]
us_age["1-4"] = us_age[1]+us_age[2]+us_age[3]+us_age[4]
us_age["5-14"] = us_age[5]+us_age[6]+us_age[7]+us_age[8]+us_age[9]+us_age[10]+us_age[11]+us_age[12]+us_age[13]+us_age[14]
us_age["15-24"] = us_age[15]+us_age[16]+us_age[17]+us_age[18]+us_age[19]+us_age[20]+us_age[21]+us_age[22]+us_age[23]+us_age[24]
us_age["25-34"] = us_age[25]+us_age[26]+us_age[27]+us_age[28]+us_age[29]+us_age[30]+us_age[31]+us_age[32]+us_age[33]+us_age[34]
us_age["35-44"] = us_age[35]+us_age[36]+us_age[37]+us_age[38]+us_age[39]+us_age[40]+us_age[41]+us_age[42]+us_age[43]+us_age[44]
us_age["45-54"] = us_age[45]+us_age[46]+us_age[47]+us_age[48]+us_age[49]+us_age[50]+us_age[51]+us_age[52]+us_age[53]+us_age[54]
us_age["55-64"] = us_age[55]+us_age[56]+us_age[57]+us_age[58]+us_age[59]+us_age[60]+us_age[61]+us_age[62]+us_age[63]+us_age[64]
us_age["65-74"] = us_age[65]+us_age[66]+us_age[67]+us_age[68]+us_age[69]+us_age[70]+us_age[71]+us_age[72]+us_age[73]+us_age[74]
us_age["75-84"] = us_age[75]+us_age[76]+us_age[77]+us_age[78]+us_age[79]+us_age[80]+us_age[81]+us_age[82]+us_age[83]+us_age[84]
us_age["85+"] = us_age[85]
us_age["Pct < 1"] = us_age["< 1"]/us_age["Total Population"]
us_age["Pct 1-4"] = us_age["1-4"]/us_age["Total Population"]
us_age["Pct 5-14"] = us_age["5-14"]/us_age["Total Population"]
us_age["Pct 15-24"] = us_age["15-24"]/us_age["Total Population"]
us_age["Pct 25-34"] = us_age["25-34"]/us_age["Total Population"]
us_age["Pct 35-44"] = us_age["35-44"]/us_age["Total Population"]
us_age["Pct 45-54"] = us_age["45-54"]/us_age["Total Population"]
us_age["Pct 55-64"] = us_age["55-64"]/us_age["Total Population"]
us_age["Pct 65-74"] = us_age["65-74"]/us_age["Total Population"]
us_age["Pct 75-84"] = us_age["75-84"]/us_age["Total Population"]
us_age["Pct 85+"] = us_age["85+"]/us_age["Total Population"]
us_age = us_age.drop(columns=["Census Region","State Name",0,999])
age_order = [
    'FIPS', 'Total Population',
    '< 1', 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
    30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
    70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, '85+',
    '1-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84',
    'Pct < 1', 'Pct 1-4', 'Pct 5-14', 'Pct 15-24', 'Pct 25-34', 'Pct 35-44', 'Pct 45-54', 'Pct 55-64', 'Pct 65-74', 'Pct 75-84', 'Pct 85+'
]
us_age = us_age[age_order]
us_age = us_age.sort_values(["FIPS"])

us_state_demographics = pd.merge(us_sex, us_age, how="left", on="FIPS")
us_state_demographics.drop('Census Region', axis=1, inplace=True)
us_state_demographics = pd.merge(us_state_demographics, us_state_codes, how="left", on="FIPS")
us_state_demographics = us_state_demographics.drop(columns=["FIPS", "Status"])
us_state_demographics["Country"] = "United States"
us_state_demographics.rename(
    columns = {"Population 2019": "Population"},
    inplace = True)
us_state_demographics["Population 100K"] = us_state_demographics["Population"]/100000
us_state_demographics["Population Source"] = us_population_source
demographics_order = [
    'Country', 'State/Province', 'Census Region',
    'Population', 'Population 100K', 
    'Female', 'Male', 'Pct Male', 'Pct Female',
    '< 1', 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
    30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
    70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, '85+',
    '1-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84',
    'Pct < 1', 'Pct 1-4', 'Pct 5-14', 'Pct 15-24', 'Pct 25-34', 'Pct 35-44', 'Pct 45-54', 'Pct 55-64', 'Pct 65-74', 'Pct 75-84', 'Pct 85+',
    'Population Source'
]
printColumns(us_state_demographics,"US State Demographics Columns")
us_state_demographics = us_state_demographics[demographics_order]
us_state_demographics = us_state_demographics.sort_values(["State/Province"])
us_state_demographics.to_excel(us_population_file, index = False)
us_state_demographics.head()


# In[10]:


# Global input data
github_request=requests.get(github_url).content
c=pd.read_csv(io.StringIO(github_request.decode('utf-8')))
currentTime = datetime.now()

print("Columns")
print(c.columns)

print("Sets")
c["set"] = c["set"].astype(str)
sets = print_column_unique(c["set"])

print("Names")
c["name"] = c["name"].astype(str)
c["name"] = c["name"].apply(lambda x: "" if x=="nan" else x)
c["name"] = c["name"].apply(lambda x: fixCountry(x))
checkCountries(c["name"])
print_column_missing(c["name"],all_countries)
names = print_column_unique(c["name"])

print("Units")
c["unit"] = c["unit"].astype(str)
c["unit"] = c["unit"].apply(lambda x: "" if x=="unit" else x)
units = print_column_unique(c["unit"])

conversions = {}
c["Region"] = c["name"].apply(lambda x: fixRegion(x))
c["Level"] = c.apply(lambda x: fixLevel(x["name"],""),axis=1)

# Format text date and add datetime for date
c["time"] = c["time"].astype(str)
c["time"] = c["time"].apply(lambda x: us_date(x))
c["Date"] = c["time"]
c["time"] = c.apply(lambda x: pd.to_datetime(x["Date"], format="%m/%d/%Y"), axis=1)
minmax_dates = c.groupby(["name"]).agg({"Date": [np.min,np.max]})
min_date = c["Date"].min()
max_date = c["Date"].max()
print("Min: " + min_date)
print("Max: " + max_date)
c.sort_values(by=['set','name','Date'], inplace=True)

# Format numeric columns
numeric_columns = ['pop_100k',
                   'new_cases_orig','new_deaths_orig','new_tests_orig',
                   'cap_cum_cases','cap_new_cases',
                   'cap_cum_deaths','cap_new_deaths',
                   'cap_cum_tests','cap_new_tests',
                   'all_cum_cases','all_new_cases','all_cum_deaths','all_new_deaths',
                   'all_cum_tests','all_new_tests',
                   'pos'
                  ]
float_columns = ['pop_100k',
                 'cap_cum_cases','cap_new_cases','cap_cum_deaths',
                 'cap_new_deaths','cap_cum_tests','cap_new_tests'
                ]
integer_columns = ['new_cases_orig','new_deaths_orig','new_tests_orig',
                   'all_cum_cases','all_cum_deaths','all_cum_tests','all_new_tests'
                  ]
c[float_columns] = c[float_columns].apply(pd.to_numeric)
c[integer_columns] = c[integer_columns].apply(lambda x: pd.to_numeric(x, errors='coerce', downcast='integer'))
has_data = c.all_cum_cases > 0
c = c[has_data]
c = c.where(c.notnull(), None)

# Add missing columns to match Google sheet
c["State/Province"] = ""
c["Accessed"] = str(currentTime.month) + '/' + str(currentTime.day) + '/' + str(currentTime.year)

c = c.rename({'time': "Time",
              'name': "Country",
              'unit': "Abbreviation",
              'cum_tests_orig': "Total Tests",
              'new_tests_orig': "Tests Daily",
              'pop_100k': "Population 100K",
              'new_cases_orig': "Cases Daily",
              'new_deaths_orig': "Deaths Daily",
              'cap_cum_cases': "Total Cases Per Capita",
              'cap_new_cases': "Cases Daily per Capita (7 day rolling average)",
              'cap_cum_deaths': "Total Deaths Per Capita",
              'cap_new_deaths': "Death Daily Per capita (7 day rolling average)",
              'cap_cum_tests': "Total Tests Per Capita (7 day rolling average)",
              'cap_new_tests': "Tests Daily Per Capita (7 day rolling average)",
              'all_cum_cases': "Total Cases",
              'all_new_cases': "Cases Daily (7 day rolling average)",
              'all_cum_deaths': "Total Deaths",
              'all_new_deaths': "Death Daily (7 day rolling average)",
              'all_cum_tests': "Total Tests (7 day rolling average)",
              'all_new_tests': "Tests Daily (7 day rolling average)",
              'pos': "Positivity Rate (7 day rolling average)"
             },axis=1)
c_data_order = [
    'Level', 'Region', 'Country', 'State/Province', 'Abbreviation', 'Time', 'Date', 'Population 100K',
    'Cases Daily', 'Cases Daily (7 day rolling average)', 'Cases Daily per Capita (7 day rolling average)',
    'Total Cases', 'Total Cases Per Capita', 
    'Tests Daily', 'Tests Daily (7 day rolling average)', 'Tests Daily Per Capita (7 day rolling average)',
    'Total Tests', 'Total Tests (7 day rolling average)', 'Total Tests Per Capita (7 day rolling average)',
    'Positivity Rate (7 day rolling average)',  
    'Deaths Daily', 'Death Daily (7 day rolling average)', 'Death Daily Per capita (7 day rolling average)',
    'Total Deaths','Total Deaths Per Capita',
    'Accessed'
]
c = c.reset_index()
c = c.loc[c["set"]=="country"].copy()
c = c[c_data_order].copy()
c.head()


# In[11]:


# Generate country raw data
group_cols = ['Level', 'Region', 'Country', 'State/Province', 'Abbreviation', 'Time', 'Date','Accessed']
sum_cols = ['Population 100K', 'Cases Daily', 'Tests Daily','Deaths Daily']
raw_data_order = group_cols + sum_cols
raw_data = c[raw_data_order].copy()
raw_data.head()


# In[12]:


# Generate raw region data
checkRegions(raw_data["Region"], raw_data["Country"])
region_group_cols = ['Level', 'Region', 'Time', 'Date','Accessed']
raw_data_regions = raw_data.groupby(region_group_cols)[sum_cols].sum()
raw_data_regions = raw_data_regions.reset_index()
raw_data_regions["Level"] = "Region"
raw_data_regions = raw_data_regions[region_group_cols + sum_cols].copy()
raw_data_regions.head()


# In[13]:


# Canada raw data
canada_source_request = requests.get(canada_source_csv).content
canada_df = pd.read_csv(io.StringIO(canada_source_request.decode('utf-8')))
currentTime = datetime.now()

print("Original Canada Columns")
print(canada_df.columns)
canada_df.rename(columns = {
    'Province': 'State/Province',
    'SummaryDate': 'Time',
    'TotalCases': 'Cases Total','DailyTotals': 'Cases Daily',
    'TotalRecovered' : 'Recovered Total','DailyRecovered': 'Recovered Daily',
    'TotalDeaths': 'Deaths Total','DailyDeaths': 'Deaths Daily',
    'TotalTested': 'Tests Total','DailyTested': 'Tests Daily',
    'TotalActive': 'Active Total','DailyActive': 'Active Daily',
    'TotalHospitalized': 'Hospitalized Total','DailyHospitalized': 'Hospitalized Daily',
    'TotalICU': 'ICU Total', 'DailyICU': 'ICU Daily'
}, inplace = True)
print("Renamed Canada Columns")
print(canada_df.columns)

canada_df.drop(columns=["OBJECTID"], inplace = True)
canada_df["Accessed"] = str(currentTime.month) + '/' + str(currentTime.day) + '/' + str(currentTime.year)
canada_df["Country"] = "Canada"
canada_df["Region"] = "North America"
canada_df["State/Province"] = canada_df["State/Province"].apply(lambda x: fixProvince(x))
canada_df["Date"] = canada_df["Time"].apply(lambda x: us_date(x).replace(" ",""))
canada_df["Level"] = canada_df["State/Province"].apply(lambda x: "Country" if x == "Canada" else "State/Province")
canada_df["State/Province"] = canada_df["State/Province"].apply(lambda x: "" if x=="Canada" else x)
string_columns = ["State/Province","Abbreviation","Country","Region"]
canada_df = canada_df.sort_values(by=["Level","Country","State/Province","Date"])
canada_df = canada_df.reset_index()
canada_df = canada_df.drop(columns=["index"])
canada_df = canada_df.merge(canada_last_population, how='left', on=["Country","State/Province"])
canada_order = [
    'Level', 'Region', 'Country', 'State/Province', 'Abbreviation', 'Time', 'Date', 'Accessed',
    "Population", "Population 100K",
    'Cases Daily', 'Cases Total', 'Tests Daily', 'Tests Total', 
    'Deaths Total', 'Deaths Daily',
    'Recovered Total', 'Recovered Daily', 'Active Daily', 'Active Total',
    'Hospitalized Total', 'Hospitalized Daily', 'ICU Total', 'ICU Daily'
]
canada_df = canada_df[canada_order].copy()
canada_df.loc[canada_df["Level"]=="State/Province"].head()


# In[14]:


# US States raw data
us_states_request = requests.get(us_source_csv).content
states=pd.read_csv(io.StringIO(us_states_request.decode('utf-8')))
currentTime = datetime.now()
states["Accessed"] = currentTime
states["Country"] = "United States"
printColumns(states, "Pre Rename Columns")
states = states.drop(
    columns = [
        'deathConfirmed', 'deathProbable',
        'hospitalized',
        'negativeTestsAntibody', 'negativeTestsPeopleAntibody', 'negativeTestsViral',
        'positiveScore', 'positiveTestsAntibody', 'positiveTestsAntigen',
        'positiveTestsPeopleAntibody', 'positiveTestsPeopleAntigen',
        'positiveTestsViral', 'positiveCasesViral',
        'totalTestEncountersViral', 'totalTestEncountersViralIncrease',
        'totalTestsAntibody', 'totalTestsAntigen',
        'totalTestsPeopleAntibody', 'totalTestsPeopleAntigen',
        'totalTestsPeopleViral', 'totalTestsPeopleViralIncrease',
        'totalTestsViral', 'totalTestsViralIncrease'
    ])
states.rename(
    columns = {
        'date': 'Date', 'state' : 'Abbreviation', 'dataQualityGrade': 'Data Quality',
        'totalTestResults' : 'Total Tests', 'totalTestResultsIncrease' : 'Tests Daily',
        'negative' : 'Total Negative', 'negativeIncrease' : 'Negative Daily',
        'positive' : 'Total Cases', 'positiveIncrease' : 'Cases Daily',
        'recovered' : 'Total Recovered',
        'death' : 'Total Deaths', 'deathIncrease' : 'Deaths Daily',
        'hospitalizedCumulative' : 'Total Hospitalized', 'hospitalizedIncrease' : 'Hospitalized Daily', 'hospitalizedCurrently' : 'Currently Hospitalized',
        'inIcuCumulative' : 'Total In ICU', 'inIcuCurrently' : 'Currently In ICU',
        'onVentilatorCumulative' : 'Total On Ventilator', 'onVentilatorCurrently' : 'Currently On Ventilator'
    }, inplace = True)
states["Date"] = states["Date"].astype(str)
states["Date"] = states["Date"].apply(lambda x: us_date(x))
states["Time"] = pd.to_datetime(states["Date"], format="%m/%d/%Y")
states["Level"] = "State/Province"
states["Region"] = "North America"
printColumns(states, "Post Rename Columns")
states_order = [
    'Level', 'Region', 'Country', 'Abbreviation', 'Data Quality', "Time", "Date",
    'Cases Daily', 'Total Cases',
    'Tests Daily', 'Total Tests', 'Negative Daily', 'Total Negative',
    'Deaths Daily', 'Total Deaths',
    'Hospitalized Daily', 'Currently Hospitalized', 'Total Hospitalized',
    'Currently In ICU', 'Total In ICU', 'Currently On Ventilator', 'Total On Ventilator',
    'Total Recovered'
]
states.head()

us_raw_demographics = us_state_demographics.drop(columns=["Country"])
all_us_demographics = us_raw_demographics.columns.tolist()
print(len(all_us_demographics))
all_us_demographics.pop(len(all_us_demographics)-1)
all_us_demographics.pop(0)
print(all_us_demographics)
us_raw_demographics.head()

# Combine US States and Demographics
states_input = pd.merge(states, us_state_codes, how="left", on="Abbreviation")
states_input = states_input.drop(columns=["Census Region"])
us_raw_demographics = us_state_demographics.drop(columns=["Country"])
states_input = pd.merge(states_input, us_raw_demographics, how="left", on="State/Province")
print(states_input.columns.tolist())
states_input = states_input.sort_values(["State/Province","Time"])
states_input = states_input.reset_index()

characteristics_order = [
    "Time", 'Date', 'Level', 'Region', 'Country', 'Abbreviation', 'State/Province', 'FIPS', 
    'Status', 'Data Quality', 'Accessed'
]
stats_order = [
    'Cases Daily', 'Total Cases',
    'Tests Daily', 'Total Tests', 'Negative Daily', 'Total Negative',
    'Deaths Daily', 'Total Deaths',
    'Hospitalized Daily', 'Currently Hospitalized', 'Total Hospitalized',
    'Currently In ICU', 'Total In ICU', 'Currently On Ventilator', 'Total On Ventilator', "Total Recovered"
]
merge_order = characteristics_order + all_us_demographics + stats_order
states_input = states_input[merge_order].copy()
states_input.head()


# In[16]:


# Create Daily Input
canada_raw_data = canada_df[raw_data_order].copy()
us_raw_data = states_input[raw_data_order].copy()
all_raw_data = pd.concat([raw_data_regions,us_raw_data,canada_raw_data,raw_data], sort=False)

all_raw_data["Level"] = all_raw_data["Level"].fillna("").astype(str)
all_raw_data["Region"] = all_raw_data["Region"].fillna("").astype(str)
all_raw_data["Country"] = all_raw_data["Country"].fillna("").astype(str)
all_raw_data["State/Province"] = all_raw_data["State/Province"].fillna("").astype(str)
all_raw_data["Abbreviation"] = all_raw_data["Abbreviation"].fillna("").astype(str)

all_raw_data["Date"] = all_raw_data["Date"].astype(str)
all_raw_data["Week"] = all_raw_data["Date"].apply(lambda x: datetime.strptime(x, '%m/%d/%Y').strftime('%Y%V'))
def firstDay(dayString):
    dt = datetime.strptime(dayString, '%m/%d/%Y')
    firstDate = dt - timedelta(days=dt.weekday())
    return firstDate.strftime('%m/%d/%Y')
all_raw_data["First Day of Week"] = all_raw_data["Date"].apply(lambda x: firstDay(x))

grouping_cols = ["Region","Country","State/Province"]
base_cols = ["Cases","Tests","Deaths"]
daily_data_order = ['Level', 'Region', 'Country', 'State/Province', 'Abbreviation', 
                    'Time', 'Date', 'Week', 'First Day of Week', 'Accessed','Population 100K']
for col in base_cols:
    all_raw_data[col + " Daily"] = all_raw_data[col + " Daily"].fillna(0).astype(int)
    all_raw_data[col + " Daily Rate"] = all_raw_data[col + " Daily"]/all_raw_data["Population 100K"]
    all_raw_data["Total " + col] = all_raw_data.groupby(grouping_cols)[col + " Daily"].cumsum().reset_index(drop=True)
    all_raw_data[col + " Daily 7D Rolling"] = all_raw_data.groupby(grouping_cols, as_index=False)[col + " Daily"].rolling(7,min_periods=7).mean().reset_index(drop=True)
    all_raw_data[col + " Daily 2W Rolling"] = all_raw_data.groupby(grouping_cols, as_index=False)[col + " Daily"].rolling(14,min_periods=14).mean().reset_index(drop=True)
    all_raw_data[col + " Daily Rate 7D Rolling"] = all_raw_data.groupby(grouping_cols, as_index=False)[col + " Daily Rate"].rolling(7,min_periods=7)[col + " Daily Rate"].mean().reset_index(drop=True)
    all_raw_data[col + " Daily Rate 2W Rolling"] = all_raw_data.groupby(grouping_cols, as_index=False)[col + " Daily Rate"].rolling(14,min_periods=14)[col + " Daily Rate"].mean().reset_index(drop=True)
    all_raw_data["Total " + col + " Rate"] = all_raw_data["Total " + col]/all_raw_data["Population 100K"]
    base_order = [
        col + ' Daily', col + ' Daily 7D Rolling', col + ' Daily 2W Rolling',
        col + ' Daily Rate', col + ' Daily Rate 7D Rolling', col + ' Daily Rate 2W Rolling'
    ]
    daily_data_order = daily_data_order + base_order

all_raw_data["Positivity 7D Rolling"] = all_raw_data["Cases Daily 7D Rolling"]/all_raw_data["Tests Daily 7D Rolling"]
all_raw_data["Positivity 7D Rolling"] = all_raw_data["Positivity 7D Rolling"].apply(lambda x: np.nan if x == np.inf else x)
all_raw_data["Positivity 2W Rolling"] = all_raw_data["Cases Daily 2W Rolling"]/all_raw_data["Tests Daily 2W Rolling"]
all_raw_data["Positivity 2W Rolling"] = all_raw_data["Positivity 2W Rolling"].apply(lambda x: np.nan if x == np.inf else x)
daily_data_order = daily_data_order + ["Positivity 7D Rolling", "Positivity 2W Rolling"]

print(daily_data_order)
all_raw_data = all_raw_data[daily_data_order].copy()
all_raw_data.to_excel(cleanedFolder + "daily_raw_input.xlsx", index=False)
all_raw_data.to_csv(cleanedFolder + "daily_raw_input.csv", index=False)
all_raw_data.head(28)


# In[18]:


# Create Weekly Input Data

canada_raw_data = canada_df[raw_data_order].copy()
us_raw_data = states_input[raw_data_order].copy()
to_concat = [raw_data_regions,us_raw_data,canada_raw_data,raw_data]
weekly_raw_data = pd.concat([s.reset_index(drop=True) for s in to_concat], sort=False)
weekly_raw_data = weekly_raw_data.reset_index(drop=True)
weekly_raw_data["Level"] = weekly_raw_data["Level"].fillna("").astype(str)
weekly_raw_data["Region"] = weekly_raw_data["Region"].fillna("").astype(str)
weekly_raw_data["Country"] = weekly_raw_data["Country"].fillna("").astype(str)
weekly_raw_data["State/Province"] = weekly_raw_data["State/Province"].fillna("").astype(str)
weekly_raw_data["Abbreviation"] = weekly_raw_data["Abbreviation"].fillna("").astype(str)
weekly_raw_data["Date"] = weekly_raw_data["Date"].astype(str)
weekly_raw_data["Week"] = weekly_raw_data["Date"].apply(lambda x: datetime.strptime(x, '%m/%d/%Y').strftime('%Y%V'))
weekly_raw_data["Week"] = weekly_raw_data["Week"].dropna().astype(int)
weekly_raw_data["First Day of Week"] = weekly_raw_data["Date"].apply(lambda x: firstDay(x))
weekly_raw_data["Cases Daily"] = weekly_raw_data["Cases Daily"].fillna(0)
weekly_raw_data["Tests Daily"] = weekly_raw_data["Tests Daily"].fillna(0)
weekly_raw_data["Deaths Daily"] = weekly_raw_data["Deaths Daily"].fillna(0)
weekly_raw_data = weekly_raw_data.sort_values(['Level', 'Region', 'Country', 'State/Province', 'Abbreviation','Time']).reset_index(drop=True)
weekly_raw_data.head()

grouped_data_order = [
    'Level', 'Region', 'Country', 'State/Province', 'Abbreviation',
    'Week', 'First Day of Week', 'Accessed','Population 100K'
]
weekly_grouped_data = weekly_raw_data.groupby(grouped_data_order,as_index=False).sum().reset_index(drop=True)
weekly_grouped_data.rename(columns = {
    'Cases Daily': 'Cases Weekly',
    'Tests Daily': 'Tests Weekly',
    'Deaths Daily': 'Deaths Weekly'
}, inplace = True)
weekly_grouped_data["Region"] = weekly_grouped_data["Region"].fillna("").astype(str)
weekly_grouped_data["Country"] = weekly_grouped_data["Country"].fillna("").astype(str)
weekly_grouped_data["State/Province"] = weekly_raw_data["State/Province"].fillna("").astype(str)
weekly_grouped_data = weekly_grouped_data.sort_values(['Region', 'Country', 'State/Province', 'Week']).reset_index(drop=True)

base_cols = ["Cases","Tests","Deaths"]
for col in base_cols:
    weekly_grouped_data[col + " Weekly"] = weekly_grouped_data[col + " Weekly"].astype(int)
    weekly_grouped_data[col + " Weekly Rate"] = weekly_grouped_data[col + " Weekly"]/weekly_grouped_data["Population 100K"]
    weekly_grouped_data["Weekly Total " + col] = weekly_grouped_data.groupby(grouping_cols)[col + " Weekly"].cumsum().reset_index(drop=True)
    weekly_grouped_data["Weekly Total " + col + " Rate"] = weekly_grouped_data["Weekly Total " + col]/weekly_grouped_data["Population 100K"]
    weekly_grouped_data[col + " 2W Rolling"] = weekly_grouped_data.groupby(grouping_cols, as_index=False)[col + " Weekly"].rolling(2,min_periods=2).sum().reset_index(drop=True)
    weekly_grouped_data[col + " 2W Rolling"] = weekly_grouped_data[col + " 2W Rolling"].fillna(0).astype(int)
    weekly_grouped_data[col + " 2W Rolling Rate"] = weekly_grouped_data[col + " 2W Rolling"]/weekly_grouped_data["Population 100K"]

weekly_grouped_data["Positivity Weekly"] = weekly_grouped_data["Cases Weekly"]/weekly_grouped_data["Tests Weekly"]
weekly_grouped_data["Positivity Weekly"] = weekly_grouped_data["Positivity Weekly"].apply(lambda x: np.nan if x == np.inf else x)
weekly_grouped_data["Positivity 2W Rolling"] = weekly_grouped_data["Cases 2W Rolling"]/weekly_grouped_data["Tests 2W Rolling"]
weekly_grouped_data["Positivity 2W Rolling"] = weekly_grouped_data["Positivity 2W Rolling"].apply(lambda x: np.nan if x == np.inf else x)

print(weekly_grouped_data.columns)
weekly_order = [
    'Level', 'Region', 'Country', 'State/Province', 'Abbreviation', 'Week', 'First Day of Week', 'Accessed', 'Population 100K', 
    'Cases Weekly','Cases Weekly Rate','Weekly Total Cases', 'Weekly Total Cases Rate', 
    'Cases 2W Rolling', 'Cases 2W Rolling Rate', 
    'Tests Weekly', 'Tests Weekly Rate', 'Weekly Total Tests', 'Weekly Total Tests Rate',
    'Tests 2W Rolling', 'Tests 2W Rolling Rate',
    'Deaths Weekly', 'Deaths Weekly Rate', 'Weekly Total Deaths', 'Weekly Total Deaths Rate',
    'Deaths 2W Rolling', 'Deaths 2W Rolling Rate', 
    'Positivity Weekly', 'Positivity 2W Rolling'
]
weekly_grouped_data = weekly_grouped_data[weekly_order]
weekly_grouped_data.to_excel(cleanedFolder + "weekly_raw_input.xlsx", index=False)
weekly_grouped_data.to_csv(cleanedFolder + "weekly_raw_input.csv", index=False)
weekly_grouped_data.head(21)


# In[ ]:




