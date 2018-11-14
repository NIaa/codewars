import urllib as u
from bs4 import BeautifulSoup
import re

def get_leaderboard_honor():
    ret = []
    bsObj = BeautifulSoup(u.urlopen("https://www.codewars.com/users/leaderboard").read())
    leaders = bsObj.find_all("td", {"class" : "is-big"})
    for ld in leaders:
        ret.append(int(''.join([c for c in ld.parent.get_text()[-7:] if c>= '0' and c <= '9'])))
    return ret