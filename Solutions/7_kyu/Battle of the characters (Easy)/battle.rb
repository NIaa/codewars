def power(x)
    eval x.split("").map{|c| c.ord}.join('+')
  end
  def battle(x, y)
    return x if power(x)>power(y)
    return "Tie!" if power(x)==power(y)
    return y if power(x)<power(y)
  end