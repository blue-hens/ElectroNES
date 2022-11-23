curve=[(0,0),(2,0),(4,0),(6,0),(8,1),(9,2),(10,3),(11,4),(11,4),(12,5),(13,6),(14,7),(15,8),(15,10),(15,12),(15,14)]
line=[(i,0) for i in range(0, 32, 2)]
def swapxy(thecurve):
    return map(lambda x: (x[1],x[0]), thecurve)
def reversexy(thecurve):
    return map(lambda x: (-x[0], -x[1]), thecurve)
def reversex(thecurve):
    return map(lambda x: (-x[0], x[1]), thecurve)
def reversey(thecurve):
    return map(lambda x: (x[0], -x[1]), thecurve)
def makebyte(ival):
    if ival < 0:
        return chr(256+ival)
    return chr(ival)

def makepaths(line, curve):
    paths={}
    paths[0]=swapxy(line)
    paths[1]=reversex(line)
    paths[2]=reversey(swapxy(line))
    paths[3]=line
    paths[4]=swapxy(curve)
    paths[5]=reversexy(curve)
    paths[6]=reversexy(swapxy(curve))
    paths[7]=curve
    paths[8]=reversex(swapxy(curve))
    paths[9]=reversex(curve)
    paths[10]=reversey(swapxy(curve))
    paths[11]=reversey(curve)
    return paths

paths=makepaths(line,curve)
#I now have paths[0] through paths[11] as [(deltax, deltay), ...]
#I'm going to store them in two files pathsx.dat, pathsy.dat
def makefiles(paths):
    pathsx=""
    pathsy=""
    for i in range(12):
        for j in range(16):
            xbyte=makebyte(paths[i][j][0])
            ybyte=makebyte(paths[i][j][1])
            pathsx += xbyte
            pathsy += ybyte
    f=file("pathsx.dat","w")
    f.write(pathsx)
    f.close()
    f=file("pathsy.dat","w")
    f.write(pathsy)
    f.close()

makefiles(paths)
