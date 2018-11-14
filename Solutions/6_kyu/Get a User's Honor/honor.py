from urllib.request import urlopen
from bs4 import BeautifulSoup
import re

def get_honor(name):
    bsObj = BeautifulSoup(urlopen("https://www.codewars.com/users/"+name).read())
    h = bsObj.findAll(text = re.compile("Honor:"))[0].parent.parent.get_text()
    return int("".join([c for c in h[6:] if c != ',']))
