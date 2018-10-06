int evaporator(double content, double evap_per_day, double threshold) {
  double percentage = 1;
  int day = 0;
  while(percentage>threshold*0.01)
  {
    percentage*=(1-evap_per_day*0.01);
    ++day;
  }
  return day;
}