// test_ir.sl - Simple test for LLVM IR generation

// Simple arithmetic function
int32 add(a : int32, b : int32) {
    return a + b;
}

// Function with local variables  
int32 multiply(x : int32, y : int32) {
    var result = x * y;
    return result;
}

// Function with control flow
int32 abs(n : int32) {
    if (n < 0) {
        return 0 - n;
    }
    return n;
}

// Function with loop
int32 factorial(n : int32) {
    var result = 1;
    var i = 1;
    
    while (i <= n) {
        result = result * i;
        i = i + 1;
    }
    
    return result;
}

// Entry point
int32 main() {
    var x = add(5, 3);
    var y = multiply(x, 2);
    var z = abs(0 - 42);
    var f = factorial(5);
    
    return f;
}


