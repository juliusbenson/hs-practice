badExample xs =
    if length xs > 0
    then head xs
    else 'Z'

goodExample xs =
    if not (null xs)
    then head xs
    else 'Z'

otherExample (x:_) = x
otherExample [] = 'Z'