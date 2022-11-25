# coding: utf-8
import os
files=filter(lambda x: x[-5:]==".json", os.listdir("."))
files.sort()
example="0303000003030303000003030000000000000000000000000303000003030303000003034000a90876100f1803ff01060301020601ff020104"
import json

def findoval(n,level):
    for y in ['-1','6']:
        for x in range(6):
            tmp=level['grid'][y][str(x)]
            if tmp == n:
                return (x,y)
    for x in ['-1','6']:
        for y in range(6):
            tmp=level['grid'][str(y)][x]
            if tmp==n:
                return (x,y)
    return False

def electron(n, level):
    tmp = findoval(n,level)
    if not tmp:
        return '\x00'+chr(int('100'+bin(n)[2:].zfill(2)[:2]+'000',2))
    if tmp[0] == '-1':
        return chr(int('000'+bin(tmp[1])[2:].zfill(3)[:3]+'11',2))+chr(int('000'+bin(n)[2:].zfill(2)[:2]+'000',2))
    if tmp[0] == '6':
        return chr(int('101'+bin(tmp[1])[2:].zfill(3)[:3]+'01',2))+chr(int('000'+bin(n)[2:].zfill(2)[:2]+'000',2))
    if tmp[1]=='-1':
        return chr(int(bin(tmp[0])[2:].zfill(3)[:3]+'00000',2))+chr(int('000'+bin(n)[2:].zfill(2)[:2]+'000',2))
    if tmp[1]=='6':
        return chr(int(bin(tmp[0])[2:].zfill(3)[:3]+'10110',2))+chr(int('000'+bin(n)[2:].zfill(2)[:2]+'000',2))
    return '\x00'+chr(int('100'+bin(n)[2:].zfill(2)[:2]+'000',2))

def target(n, level):
    tmp = findoval(n+4, level)
    if not tmp:
        return '\xff\xff\x00'
    if tmp[0] == '-1':
        return '\xff'+chr(tmp[1])+'\x01'
    if tmp[1] == '-1':
        return chr(tmp[0])+'\xff\x01'
    return chr(int(tmp[0]))+chr(int(tmp[1]))+'\x01'

def makelevel(fname):
    level=json.load(open(fname,'r'))
    blocks=[]
    for i in range(6):
        for j in range(6):
            blocks.append(chr(level['grid'][str(i)][str(j)]))
    result = "".join(blocks)
    result += "".join([electron(i,level) for i in range(4)])
    result += "".join([target(n,level) for n in range(4)])
    result += chr(int(level['moves']))
    return result

leveldata = map(makelevel, files)
if leveldata[0].encode('hex') == example:
    print("all is well")
    rawbytes = "".join(leveldata)
    f=file("levels.dat",'w')
    f.write(rawbytes)
    f.close()
else:
    print("what went wrong?", leveldata[0].encode('hex'), example)
