from functools import lru_cache

@lru_cache(None)
def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

raise RecursionError
#Tail optimization 
#python does not support that for some reason, unknown 
#clousure is when the inner function is using a parameter from the outer function and outer function returns the inner functions the 
#inner function can still use outer function's values
#you can return a function to map and make it work 
# eg : map (clouser(value), [2, 31])
# here clouser will return a function 