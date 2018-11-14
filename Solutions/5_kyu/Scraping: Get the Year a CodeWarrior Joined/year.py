from urllib.request import urlopen
from bs4 import BeautifulSoup
import re

def get_member_since(name):
    bsObj = BeautifulSoup(urlopen("https://www.codewars.com/users/"+name).read())
    year = bsObj.findAll(text = re.compile("Member Since:"))[0].parent.parent.get_text()
    return year[13:]