y=0
x=0
cmd=''
n=0
aim=0

f=open('input_02_01.dat','r')
for row in f:
  #print("'" + row[1:3] + "'")
  result=row.find(' ')
  #print("'" + row[result:].strip() + "'")
  n=int(row[result:].strip())
  result=row.find('forward')
  if result == 0:
      cmd='f'
  result=row.find('up')
  if result == 0:
      cmd='u'
  result=row.find('down')
  if result == 0:
      cmd='d'
      
      
  if cmd=='f':
      x=x+n
      y=y+aim*n
  elif cmd=='u':
      #y=y-n
      aim=aim-n
  elif cmd=='d':
      #y=y+n
      aim=aim+n
  else:
      print('helvete '+ cmd)   
  print(cmd,'n=',n,'x=',x,'y=',y,'aim=',aim)