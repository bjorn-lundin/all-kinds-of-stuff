

f = open("input_day_01_task_01.dat", "r")

prev = 0
cnt = 0
first = True
for row in f:
#  print(row)
  x=int(row)
  if x > prev and not first:
    cnt=cnt+1
    print(x, 'increased',cnt)
  elif x < prev :
    print(x, 'decreased',cnt)
  else:
    print(x, 'same',cnt)
  
  prev=x  
  first=False
  
print('Antal', cnt)
f.close()
