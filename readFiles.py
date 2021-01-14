import os, glob
from zipfile import ZipFile
import pandas as pd
import sqlite3
import geopandas as gpd
import pathlib

path = os.path.split(os.path.abspath(__file__))[0]

rawFolder = os.path.join(path, 'Data', 'raw')
dataFolder = os.path.join(path, 'Data')
zipFiles = glob.glob1(dataFolder, '*.zip')

# for f in glob.glob1(rawFolder,'*'):
#     os.remove(os.path.join(rawFolder, f))

columns = ['Istasyon_No', 'Istasyon_Adi', 'YIL', 'AY']

df_all = pd.DataFrame()

for file in zipFiles:
    print(file)
    try:
        # [os.remove(os.path.join(rawFolder, i)) for i in glob.glob1(rawFolder, '*')]
        code = " rm -rf /mnt/s/Drought_App/Data/raw/*"
        os.system(code)
    except:
        pass

    with ZipFile(os.path.join(dataFolder, file), 'r') as zipObj:
        # Extract all the contents of zip file in current directory
        zipObj.extractall(rawFolder)

    dataFiles = glob.glob1(rawFolder, '*.xlsx')
    istFiles = glob.glob1(rawFolder, '*.txt')

    for f_ in dataFiles:
        print(f_)
        df = pd.read_excel(os.path.join(rawFolder, f_), skipfooter=1)
        df_ist = pd.read_csv(os.path.join(rawFolder, istFiles[0]), sep='|')
        df_ist = df_ist.rename(columns={'?stasyon Numaras?': 'Istasyon_No', 'Rak?m': 'Rakim'})
        df_ist = df_ist.drop(columns=['?stasyon Ad?', '?l', '?l?e'])
        col = list(set(list(df.columns)) - set(columns))[0]
        df = df.rename(columns={col: 'value'})
        df = pd.merge(df, df_ist, how='inner', on=['Istasyon_No'])
        df['var'] = col
        df_all = df_all.append(df)

stations = df_all['Istasyon_No'].unique()

ll = []

df_all = df_all.loc[df_all['AY'] != 13]
df_new = pd.DataFrame()
for station in stations:
    tt_m = df_all.loc[(df_all['var'] == 'AYLIK_TOPLAM_YAGIS_mm_Manuel') & (df_all['Istasyon_No'] == station)]
    tt_ogm = df_all.loc[(df_all['var'] == 'AYLIK_TOPLAM_YAGIS_mm') & (df_all['Istasyon_No'] == station)]
    con = tt_m.YIL.max() == tt_ogm.YIL.max()
    if con == True:
        ll.append(station)
        df_new = df_new.append(tt_m)
    else:
        dd = tt_m.append(tt_ogm)
        dd = dd.groupby(['YIL', 'AY']).first()
        dd = dd.reset_index()
        df_new = df_new.append(dd)

df_all = df_all.loc[(df_all['var'] != 'AYLIK_TOPLAM_YAGIS_mm_Manuel') & (df_all['var'] != 'AYLIK_TOPLAM_YAGIS_mm')]
df_all = df_all.dropna()
conn = sqlite3.connect(os.path.join(dataFolder, 'data.db'))
df_all.to_sql('data', conn, if_exists='replace', index=False)
df_new.to_sql('data', conn, if_exists='append', index=False)

code = """SELECT DISTINCT(d.Istasyon_No ),d.Boylam,d.Enlem,d.Istasyon_Adi,d.Rakim from "data" d;"""
df_st = pd.read_sql(code, conn)
# df_st = gpd.GeoDataFrame(df_st)
crs = {'init': 'epsg:4326'}

df_st = gpd.GeoDataFrame(df_st, geometry=gpd.points_from_xy(df_st.Boylam, df_st.Enlem), crs=crs)
df_st.to_file(os.path.join(dataFolder, 'st.geojson'), driver='GeoJSON')
