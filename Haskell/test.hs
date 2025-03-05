tthw=putStrLn "Hello, World!"

fact n=case n of 
    0->1 
    a->a*fact(a-1)


avg arr = sum arr `div` length arr