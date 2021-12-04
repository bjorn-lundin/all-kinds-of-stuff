
row_num=0
a=0
b=0
c=0

f = open("input_day_01_task_02.dat", "r")

for row in f:
#  print(row)
    x=int(row)
    row_num=row_num+1
    
    if row_num == 1 :
        a=x
        if b > 0 :
            b=b+x
            print(b)
        if c > 0 :
            c=c+x
    elif row_num == 2 :
        if a > 0 :
            a=a+x
        b=x
        if c > 0 :
            c=c+x
            print(c)
    elif row_num == 3 :
        if a > 0 :
            a=a+x
            print(a)
        if b > 0 :
            b=b+x
        c=x
        row_num=0

    #print('x=',x,'row_num=',row_num,'a=',a,'b=',b,'c=',c)

f.close()
