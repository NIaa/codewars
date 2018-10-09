<https://www.codewars.com/kata/last-digit-of-a-huge-number/train/python>
<b><b>
```python
def last_digit(lst):
    n = 1
    for x in reversed(lst):
        n = x ** (n if n < 4 else n % 4 + 4)
    return n % 10
```