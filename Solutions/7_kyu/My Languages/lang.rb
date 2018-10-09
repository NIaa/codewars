def my_languages(d)
    d.select{|i| d[i]>=60}.sort_by{|k, v| -v}.map{|l| l[0]}
end