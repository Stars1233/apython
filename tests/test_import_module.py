import myhelper
print(myhelper.GREETING)
print(myhelper.add(3, 4))
print(myhelper.double(5))
from myhelper import GREETING, add
print(GREETING)
print(add(10, 20))
