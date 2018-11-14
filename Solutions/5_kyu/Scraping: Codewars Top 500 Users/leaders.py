from urllib.request import urlopen
from bs4 import BeautifulSoup

URL = 'https://www.codewars.com/users/leaderboard'

class Leaderboard:
    def __init__(self, l):
        self.position = l
        
class User:
    def __init__(self, n, c, h):
        self.name, self.clan, self.honor = n, c, h

def solution():
    bsObj = BeautifulSoup(urlopen("https://www.codewars.com/users/leaderboard").read())
    users = bsObj.find_all("td", {"class" : "is-big"})
    d = {}
    for i, user in enumerate(users):
        td = user.parent.find_all("td")
        name = td[1].get_text()[5:]
        clan = td[-2].get_text()
        honor = int(''.join([c for c in td[-1].get_text() if c != ',']))  
        d[i+1] = User(name, clan, honor)
    return Leaderboard(d)