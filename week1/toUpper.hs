main::IO()
main = do
    print(toUpper 'a')
    print(toUpper 'A')
    print(toUpper 'z')
    print(toUpper 'Z')
    print(toUpper ' ')

firstSmallCode = ord 'a'
lastSmallCode = ord 'z'
offset = firstSmallCode - (ord 'A')

isSmallLetter ch = (ord ch) >= firstSmallCode && (ord ch) <= lastSmallCode

toUpper::Char->Char
toUpper ch = if (isSmallLetter ch) then chr ((ord ch) - offset) else ch