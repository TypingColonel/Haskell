# import operator
# # class normal:
# #     def __init__(self):
# #         self.a = 0
    
# #     def __iter__(self):

# #computation does not occur here until next is called
# #same as in haskell this can also handle infinite lists
# m = map(round, [2.3])
# print(next(m))
# print(m)        

# #custom iterator
# class custom:
#     def __init__(self):
#         self.pos = 0
    
#     def __iter__(self):
#         return self
    
#     def __next__(self):
#         if self.pos < 2:
#             self.pos = self.pos - 1
#             return self.pos
#         else:
#             raise StopIteration
    
# a = custom()
# # for x in a:
# #     print(x)

# #zip function in python
# #to pass a operation as an argurment import argument module

# l = [1]
# z = [2]
# for t, q in zip(l, z):
#     print(t, q)
# #any and all functions

# from functools import reduce

# f = [2, 3, 4, 512]
# print(reduce(operator.add, f, 10))

#creating a custom generator 

def gen_function():
    for a in range(100):
        yield str(a)

a = gen_function()
print(a)
while True:
    try:
        print(next(a))
    except StopIteration:
        print("generator finished running")
        exit(0)