from pydexarm import Dexarm

#'''windows'''
#dexarm = Dexarm("COM67")
#'''mac & linux'''
dexarm = Dexarm("/dev/ttyACM3")


kind = dexarm.get_module_kind()
print('kind',kind)
if kind != "PUMP":
  dexarm.set_module_kind("2")

print('curr',dexarm.get_current_position())

dexarm.soft_gripper_open()
dexarm.delay_s(2)

dexarm.soft_gripper_close()
dexarm.delay_s(2)

dexarm.soft_gripper_neutral()
dexarm.delay_s(2)

dexarm.soft_gripper_stop()
dexarm.delay_s(2)


dexarm.go_home()
#print('curr',dexarm.get_current_position())

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





