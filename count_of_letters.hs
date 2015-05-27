countOfLetters :: [Char] -> [(Char, Int)]
countOfLetters str = countOfLettersInner str []

countOfLettersInner :: [Char] -> [(Char, Int)] -> [(Char, Int)]
countOfLettersInner [] result = result
countOfLettersInner (x:xs) result = 
	if (containsChar result x) then
		countOfLettersInner xs (incrementValueForChar result x)
	else 
		countOfLettersInner xs ((x, 1):result)

containsChar :: [(Char, Int)] -> Char -> Bool
containsChar [] ch = False
containsChar (x:xs) ch = 
	if (fst x) == ch then
		True
	else if (length xs) == 0 then
		False
	else 
		(containsChar xs ch)

incrementValueForChar :: [(Char, Int)] -> Char -> [(Char, Int)]
incrementValueForChar (x:xs) ch = 
	if (fst x) == ch then
		(ch, (snd x)+1):xs
	else 
		x:(incrementValueForChar xs ch)

main = do
	print(countOfLetters "112345bbb")