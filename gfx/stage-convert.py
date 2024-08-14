import sys
import json

source = open(sys.argv[1],"r")
map = json.loads(source.read())
source.close()

tileLength = map['screenSize'][0] * map['screenSize'][1]
metaLength = tileLength >> 5
stageHeight = map['screenSize'][1] * 8 * len(map['screens'])

header = bytearray(4)
header[0] = stageHeight & 0xFF
header[1] = (stageHeight >> 8) & 0xFF
header[2] = tileLength & 0xFF
header[3] = (tileLength >> 8) & 0xFF

tileData = bytearray(tileLength * len(map['screens']))
collisionData = bytearray(metaLength * len(map['screens']))
paletteData = [bytearray(32),bytearray(32),bytearray(32),bytearray(32)]

for j in range(0, len(map['screens'])):
  screen = map['screens'][j]
  meta = screen['metaValues']
  offset = j * tileLength
  metaOffset = j * metaLength
  
  for i in range(0, tileLength):
    tileData[offset + i] =  screen['tiles'][i]

  for i in range(0, metaLength):
    tileIndex = i * 8
    value = 0
    for j in range(0,8):
      value |= (1 << (7-j)) if (meta[tileIndex + j] > 0) else 0
    collisionData[metaOffset + i] = value

for j in range(0,4):
  palette = map['colorPalettes'][j]
  for i in range(0, len(palette)):
    r = (palette[i][0] >> 3) & 0x1F
    g = (palette[i][1] >> 3) & 0x1F
    b = (palette[i][2] >> 3) & 0x1F
    color = r | (g << 5) | (b << 10)
    paletteData[j][i * 2] = color & 0xFF
    paletteData[j][i * 2 + 1] = (color >> 8) & 0xFF

dest = open(sys.argv[2],"wb")
dest.write(header)
dest.write(tileData)
dest.write(collisionData)
dest.close()

dest = open(sys.argv[2] + ".0.pal","wb")
dest.write(paletteData[0])
dest.close()

dest = open(sys.argv[2] + ".1.pal","wb")
dest.write(paletteData[1])
dest.close()

dest = open(sys.argv[2] + ".2.pal","wb")
dest.write(paletteData[2])
dest.close()

dest = open(sys.argv[2] + ".3.pal","wb")
dest.write(paletteData[3])
dest.close()
