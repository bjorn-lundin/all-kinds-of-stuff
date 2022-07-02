from pydexarm import Dexarm
import sys


def deposit():
  dexarm.move_to(0,360,0)
  dexarm.move_to(0,360,-60)
  dexarm.soft_gripper_open()
  dexarm.delay_s(1)
  dexarm.soft_gripper_stop()
  dexarm.move_to(0,360,0)


dexarm = Dexarm("/dev/ttyACM3")


coords = []
c=(45,255,0)
coords.append((45,255,0))
coords.append((15,255,0))
coords.append((-15,255,0))
coords.append((-45,255,0))
coords.append((45,285,0))
coords.append((15,285,0))
coords.append((-15,285,0))
coords.append((-45,285,0))
coords.append((45,315,0))
coords.append((15,315,0))
coords.append((-15,315,0))
coords.append((-45,315,0))
coords.append((45,345,0))
coords.append((15,345,0))
coords.append((-15,345,0))
coords.append((-45,345,0))

dexarm.go_home()

kind = dexarm.get_module_kind()
print('kind',kind)
if kind != "PUMP":
  dexarm.set_module_kind("2")

print('curr',dexarm.get_current_position())

for c in coords:
  print (c)
  dexarm.move_to(c[0], c[1], c[2])
  dexarm.soft_gripper_neutral()
  dexarm.move_to(c[0], c[1], -60)
  dexarm.soft_gripper_close()
  dexarm.move_to(c[0], c[1], c[2])
  deposit()



dexarm.go_home()

dexarm.soft_gripper_stop()
sys.exit(0)

dexarm.move_to(50, 300, 0)

#dexarm.soft_gripper_open()
dexarm.soft_gripper_neutral()

dexarm.move_to(50, 300, -60)

dexarm.soft_gripper_close()

dexarm.move_to(50, 300, -20)

dexarm.move_to(-50, 300, -20)

dexarm.move_to(-50, 300, -60)

dexarm.soft_gripper_open()
dexarm.delay_s(2)

dexarm.soft_gripper_stop()

dexarm.move_to(-50, 300, -20)

dexarm.go_home()





