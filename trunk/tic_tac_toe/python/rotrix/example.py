from pydexarm import Dexarm

'''windows'''
#dexarm = Dexarm("COM67")
'''mac & linux'''
#arm = Dexarm("/dev/tty.usbmodem1421")
arm = Dexarm("/dev/ttyACM0")
arm.go_home()


def X(n):
  return -80+n*45

def Y(n):
  return 220+n*43


for x in [0,1,2,3] : 
  for y in [0,1,2,3] : 
    print("x=",str(x),"y=",str(y))
    arm.move_to(X(x), Y(y), 0)
    arm.move_to(X(x), Y(y), -40)
    if y==0 or y==2:
      arm.air_picker_pick()

    if y==1 or y==3:
      arm.air_picker_nature()
      arm.delay_s(2)

    arm.move_to(X(x), Y(y), 0)
#arm.go_home()
arm.air_picker_stop()
