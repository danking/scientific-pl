define fib<>[](x : number) : number
  match x with
    0 => 0
    1 => 1
    n : number => fib@<>[](n-1) + fib@<>[](n-2)
  end
end

fib@<>[](0)
fib@<>[](1)
fib@<>[](2)
fib@<>[](3)
fib@<>[](4)
fib@<>[](5)
fib@<>[](10)
