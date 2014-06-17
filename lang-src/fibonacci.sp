define fib<>[](x : number) : number
  match x with
    1 => 1
    2 => 2
    n : number => fib@<>[](n-1) + fib@<>[](n-2)
  end
end

fib@<>[](1)
fib@<>[](2)
fib@<>[](3)
fib@<>[](5)
fib@<>[](10)
