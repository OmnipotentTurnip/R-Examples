import time

start_time = time.time()

example1 = '0,3,6'.split(',')
example2 = '1,3,2'.split(',')
example3 = '2,1,3'.split(',')
example4 = '1,2,3'.split(',')
example5 = '2,3,1'.split(',')
example6 = '3,2,1'.split(',')
example7 = '3,1,2'.split(',')
input_numbers = '2,0,6,12,1,3'.split(',')

numbers = input_numbers

seen = {}
l = len(numbers)
current = numbers[l-1]
f = 30000000

def addtodict(key, val):
    key = str(key)
    seen[key] = { "last": val }

for i in range(0, l - 1):
    addtodict(numbers[i], i + 1)

for j in range(l, f):
    if current in seen:
        new = str(j - seen[current]['last'])
    else:
        new = "0"
    addtodict(current, j)
    current = new

print('Answer is ' + current)
print("--- %s seconds ---" % (time.time() - start_time))
