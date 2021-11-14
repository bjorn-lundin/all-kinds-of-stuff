import mcpi.minecraft as minecraft
import cv2

mc = minecraft.Minecraft.create()
for i in range(10) :
    mc.postToChat(" HEJ SARA " + str(i))

