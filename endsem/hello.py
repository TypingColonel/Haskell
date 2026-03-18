from functools import partial, reduce, lru_cache
from math import inf

@lru_cache(None)
def add(x, y):
    return x + y

# print(any())

x =  [1, 23, 5, 5]

print(list(enumerate(x, start = 1)))

for idx, _ in enumerate(x, start = 1):
    pass

a = reduce(lambda x, y: x + 6, [1, 23, 5, 5], 10)
print(a)

add5 = partial(add, 5)
print(add5(5))

def custom_generator(n):
    for i in range(n):
        yield i

a = custom_generator(5)
print(a)

for i in a:
    print(i)

# print(reversed(x))
y = reversed(sorted(x))
print([i for i in y])

for i in range(int(inf)):
    print(i)