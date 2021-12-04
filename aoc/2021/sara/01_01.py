
cur_row=0
prev_row=0
cnt=0
first=True

f=open('input_01_01.dat','r')
for x in f:
  print(x)
  cur_row=int(x)
  if cur_row > prev_row and not first:
      cnt=cnt+1
  
  prev_row=cur_row
  first=False
  
print('count',cnt)