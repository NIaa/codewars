fn power_of_two(x: u64) -> bool {
   x != 0 && x & (x - 1) == 0
}