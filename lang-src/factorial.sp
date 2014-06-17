define fact<>[](x : number) : number
  match x = 1 with
    inl _ : () => 1
    inr _ : () => x * fact@<>[](x-1)
  end
end

fact@<>[](5)
fact@<>[](10)


