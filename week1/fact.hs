main::IO()
main = do
    print(fact 3)
    print(fact 5)
    print(fact 1)
    print(fact 0)

fact::Int->Int
fact x = if (x == 0) then 1 else x * fact (x - 1)