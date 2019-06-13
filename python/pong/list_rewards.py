import os


path = os.environ.get('BOT_HISTORY') + '/data/ai/rewards'
arr = os.listdir(path)
cnt=0
cnt2=0
for filename in arr:
    bd = {}
    print(filename)
    cnt = cnt + 1
    with open(path + '/' + filename) as fh:
        first = True
        for line in fh:
            cnt2 = cnt2 + 1
            if first:
                #print('first')
                linearray=line.split('|')
                ts = '00:00:00.000'
                bd[ts] = {}
                for j in range(1,17):
                    bd[ts][j] = float(linearray[j])

                first = False

            else:
                #print('else')
                linearray=line.split('|')
                ts = linearray[0]
                bd[ts] = {}
                for j in range(1,17):
                    bd[ts][j] = linearray[j]


    print(cnt,cnt2)

#print(bd['00:00:00.000'][4])
#print(bd['19:05:45.339'][4])


#{'1.123647159.dat': {'19:05:45.339': {1: '-100.000', 2: '-100.000', 3: '-100.000', 4: '-100.000', 5: '-100.000', 6: '-100.000', 7: ' 342.000', 8: '-100.000', 9: #'-100.000', 10: '-100.000', 11: ' 0.000', 12: ' 0.000', 13: ' 0.000', 14: ' 0.000', 15: ' 0.000', 16: ' 0.000\n'}, '00:00:00.000': {1: 4099615.0, 2: 5031719.0, 3: #5441937.0, 4: 5662976.0, 5: 6447118.0, 6: 7399145.0, 7: 7430136.0, 8: 8106159.0, 9: 8221099.0, 10: 8760180.0, 11: 0.0, 12: 0.0, 13: 0.0, 14: 0.0, 15: 0.0, 16: #0.0}}}
