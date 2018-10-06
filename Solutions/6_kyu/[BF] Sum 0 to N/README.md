<https://www.codewars.com/kata/bf-sum-0-to-n/train/bf>   
<https://stackoverflow.com/questions/46056821/how-to-write-if-else-statements-in-brainfuck>   
<https://esolangs.org/wiki/Brainfuck_algorithms#Divmod_algorithm>  
<b><b>
Division fucks my brain so hard :(
```
[-]> cell1 = 0
[-]+ cell2 = 1
|manipulations that change cell1 value
|goto to cell1
[                          // if cell1 != 0
    [-]>[-]                // both cell1 and cell2 now equal 0 
                              if you want the loop to end and want to prevent the 
                              next loop from starting
    |do something
    |goto cell1            // Make sure you point again to cell1 to exit the loop
]>[                        // else
    |do something else
    |goto cell1            // Make sure you point again to cell1 to exit the loop
]
```