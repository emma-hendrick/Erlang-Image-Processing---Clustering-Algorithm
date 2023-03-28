import serial
import sys

# configure the serial connection
ser = serial.Serial(
    port='COM3', # replace with your serial port
    baudrate=115200, # replace with your baud rate
    bytesize=serial.EIGHTBITS,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_ONE,
    timeout=None
)

for arg in sys.argv[1:]:
    # define the bytes to send
    # This is our RGB value at the moment
    data = int(arg).to_bytes(1, 'little')
    print(data)
    # send the bytes over the serial connection
    ser.write(data)

# close the serial connection
ser.close()