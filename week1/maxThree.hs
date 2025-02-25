main::IO()
main = do
    print(maxThree 4 5 3)
    print(maxThree 3 2 1)
    print(maxThree 7 2 10)

maxThree a b c = max a (max b c)