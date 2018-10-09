import re
def validate_pin(pin):
    rgx = re.compile(r'^(\d\d\d\d|\d\d\d\d\d\d)$')
    return rgx.match(pin) != None