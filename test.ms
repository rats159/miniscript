var count = 0;
function fib(n) {
    print(count);
    count += 1;
    if n < 2 return n;
    return fib(n-2) + fib(n - 1);
};

print("Result:", fib(4));
print(count);