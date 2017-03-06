#This code scrapes dca site to create dataframe of local government authorities
from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
import html5lib as h5
import bs4


profile = webdriver.FirefoxProfile()
profile.set_preference("general.useragent.override","Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/40.0.2214.111 Safari/537.36")
driver = webdriver.Firefox(profile)


bases = [
#4/2011
'https://web.archive.org/web/20110402011728/http://www.gmanet.com/Cities.aspx?',
#1/2012
'https://web.archive.org/web/20120111121556/http://www.gmanet.com/cities.aspx?',
#1/2013
'https://web.archive.org/web/20130126202452/http://www.gmanet.com/Cities.aspx?',
#2/2014        
'https://web.archive.org/web/20140216232911/http://www.gmanet.com/Cities.aspx',
#1/2015
'https://web.archive.org/web/20150112061115/http://www.gmanet.com/Cities.aspx'] 

emptyfill = pd.DataFrame(columns=('Position','Name','City','Date','Link'))

for base in bases:
    driver.get(base)
    linkvec = []
    letterlist = ['A-C','D-J','K-N','O-R','S-Z']

    for letters in letterlist:
        driver.find_element_by_partial_link_text(letters).click()
        html = driver.page_source
        ListlinkerHref = driver.find_elements_by_css_selector('.CityListLink')
        for i in range(0,len(ListlinkerHref)):
            linkvec.append(ListlinkerHref[i].get_attribute('href'))

    import requests
    import numpy as np
    import time
    for link in linkvec:
        f = requests.get(link,timeout=20)
        citysoup = BeautifulSoup(f.text,'html.parser')
        citytable = citysoup.select('.TableCell')
        citytext = []
        for i in range(0,len(citytable)):
            citytext.append(citytable[i].text)
        cityarray = np.asarray(citytext)
        citymatrix = np.reshape(cityarray,(len(cityarray)/2, 2))
        citydf = pd.DataFrame(citymatrix,columns=['Position','Name'])
        citydf['City'] = citysoup.select('#_ctl0_main__ctl0_txtCityName')[0].text
        citydf['Date'] = base
        citydf['Link'] = link
        emptyfill = emptyfill.append(citydf)  
citydata = emptyfill
citydata.to_csv(path_or_buf = '../Input/ga.city.officials.2011.2015.csv')