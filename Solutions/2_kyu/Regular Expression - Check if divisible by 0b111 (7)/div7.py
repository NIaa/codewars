# FSM https://en.wikipedia.org/wiki/Finite-state_machine
p00 = '0|111' 
p44 = '(1|00)0'
p55 = '001*0'
p04 = '100'
p40 = '011'
p05 = '1101*0|101'
p50 = '01'
p45 = '(1|00)1|0101*0'
p54 = '1'

pattern = '((({})({})*({}))|{})'
p00      = pattern.format(p04,p44,p40,p00)
p55      = pattern.format(p54,p44,p45,p55)
p05      = pattern.format(p04,p44,p45,p05)
p50      = pattern.format(p54,p44,p40,p50)

solution = '^(' + pattern.format(p05,p55,p50,p00) +')+$'