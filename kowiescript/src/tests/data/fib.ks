fn fibonacci(n) {
  if n <= 1 {
    ret n;
  } else {
    ret fibonacci(n - 1) + fibonacci(n - 2);
  }
}

let a = 10;  // Replace with the desired value of n
let fib_number = fibonacci(a);
print(fib_number);