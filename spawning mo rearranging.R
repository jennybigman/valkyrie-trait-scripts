# Define the vector of months
mo <- c("Jan", "Feb", "Mar", "Apr", "Nov", "Dec")

# Define the full list of months for comparison
all_months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Find the position of the months in the `all_months` vector
mo_positions <- match(mo, all_months)

# Check where the break occurs (i.e., where the months are not consecutive)
break_index <- which(diff(mo_positions) > 1)

# If there is a break, rearrange the vector by moving months after the break to the front
if (length(break_index) > 0) {
  # Split the months at the first break and rearrange
  mo_rearranged <- c(mo[(break_index[1] + 1):length(mo)], mo[1:break_index[1]])
} else {
  # If all months are consecutive, no rearrangement is needed
  mo_rearranged <- mo
}

# View the rearranged vector
print(mo_rearranged)
