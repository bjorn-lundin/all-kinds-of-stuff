# aoc 2022 dec 1a


list_of_calories=[]
sum=0
with open("data_01a.dat","r") as f:
  for line in f:
    if line.strip() != "":
      sum = sum + int(line.strip())
    else:
      list_of_calories.append(sum)
      sum = 0

idx = 0
cnt = 0
highest = 0

for item in list_of_calories:
  cnt = cnt +1
  if item > highest :
    highest = item
    idx = cnt
    
print('Highest', highest, idx)


list_of_calories_sorted = list_of_calories.copy()
list_of_calories_sorted.sort(reverse=True)

idx = 0

num1=0
num2=0
num3=0

for item in list_of_calories_sorted:
  idx = idx +1
  print('top3', item, idx)
  if idx == 1 : item1 = item
  if idx == 2 : item2 = item
  if idx == 3 : item3 = item
  if idx >= 3 : break

idx = 0

idx1=0
idx2=0
idx3=0

for item in list_of_calories:
  idx = idx +1
  print('top', item, idx)
  if item == item1 : idx1 = idx
  if item == item2 : idx2 = idx
  if item == item3 : idx3 = idx

print('1', item1, idx1)
print('2', item2, idx2)
print('3', item3, idx3)

print('sum top 3', item1 + item2 + item3)






