MCU=atmega328p
AVRDUDE_CONF=C:/bnl/tools/WinAVR/20100110/bin/avrdude.conf
COM_PORT=COM7
PROJECT.NAME=ada_interrupts
all:
	avr-gnatmake -g -XMCU=$(MCU) -P$(PROJECT.NAME).gpr
	avr-objcopy -O ihex $(PROJECT.NAME).elf $(PROJECT.NAME).hex
	rm -f b~* *.o *.ali

clean:
	rm -f *.o *.ali *.hex *.elf

burn:
	avrdude -C $(AVRDUDE_CONF) -v -v -p "m328p" \
    -c "arduino" -P $(COM_PORT) -b 57600 -F -U flash:w:$(PROJECT.NAME).hex

dump:
	avr-objdump -mavr:5 -d $(PROJECT.NAME).elf
    
test:
	putty -load "arduino"

