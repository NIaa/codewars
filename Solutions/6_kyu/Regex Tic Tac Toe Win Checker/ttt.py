import re
def regex_tic_tac_toe_win_checker(b):
    r = r'XXX.{6}|OOO.{6}|...XXX...|...OOO...|.{6}XXX|.{6}OOO|X..X..X|O..O..O|X...X...X|O...O...O|..X.X.X..|..O.O.O..' 
    print(len(r))
    # 107 chars, first try
    rgx = re.compile(r)
    return rgx.search(b) != None