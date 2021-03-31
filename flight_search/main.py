# This is a quite simple script that gets the current cheapest flights from Budapest to destinations
# provided in the "Flight Deals.csv" in the next half year. 
# (It is  possible to modify the destinations by changing them in the aforementioned csv).
# In order to run the code you need an API key for Tequila's meta search which is accessible by 
# registering on https://tequila.kiwi.com/portal/login or asking me to give one to you via email.
# (My email: witens11@gmail.com)

API_KEY = ""

import pandas as pd
import requests


df = pd.DataFrame(pd.read_csv("Flight Deals.csv"))
df.iloc[:,1] = pd.NA
df.loc[df["City"]=="Paris","IATA Code"]

LOCATIONS_ENDPOINT = "https://tequila-api.kiwi.com/locations/query"
for city in df.iloc[:,0]:
    apikey = API_KEY
    params = {
        "apikey": apikey,
        "term": city
    }
    request = requests.get(LOCATIONS_ENDPOINT, params=params)
    df.loc[df["City"]==city,"IATA Code"] = request.json()["locations"][0]["code"]
df

import datetime as dt
today = dt.date.today().strftime("%d/%m/%Y")
end_date = (dt.date.today() + dt.timedelta(180)).strftime("%d/%m/%Y")

SEARCH_ENDPOINT = "https://tequila-api.kiwi.com/v2/search"
from_city = "BUD"
to_cities = df["IATA Code"].to_string(index=False).replace("\n", ",").strip()

params = {
    "apikey": API_KEY,
    "fly_from": from_city,
    "fly_to": to_cities,
    "date_from": today,
    "date_to": end_date,
    "nights_in_dst_from": 7,
    "nights_in_dst_to": 30,
    "flight_type": "round",
    "one_for_city": 1,
    "curr": "HUF"
}
request = requests.get(SEARCH_ENDPOINT, params=params)
flight_data = request.json()["data"]

df.loc[:,["Lowest Price", "From", "Till"]] = pd.NA
for req in flight_data:
    df['Lowest Price'][df['City'] == req['cityTo']] = req['price']
    df['From'][df['City'] == req['cityTo']] = pd.to_datetime(req['local_arrival']).date().strftime("%Y-%m-%d")
    df['Till'][df['City'] == req['cityTo']] = pd.to_datetime(req['route'][len(req['route'])-1]['local_arrival']).date().strftime("%Y-%m-%d")
df2 = df.rename(columns={"Lowest Price":"Lowest Price (HUF)"})

#import openpyxl
#df2.to_excel("Flight Deals_filled.xlsx", index=False)
print(df2)
