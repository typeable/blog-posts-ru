# Что такое Property-Based Testing?

Property-based testing (PBT) - подход к тестированию ПО, подразумевающий автоматическую проверку свойств функций (предикатов), специфицируемых программистом-тестировщиком. Для проверки, т.е. поиска контрпримеров, используются автоматически сгенерированные входные данные.

PBT-библиотеки, как правило, [состоят](https://github.com/dubzzz/fast-check/blob/762cfd15e48014a67890f109ae31a122af16f5a8/documentation/Arbitraries.md#house-arbitraries) из двух частей:

- Runners - отвечают за запуск тестов и проверку истинности предикатов
- Arbitraries - отвечают за генерацию псевдо-случайных данных, при этом делая возможным сокращение (shrinking) - способ "упростить" найденный контрпример.

PBT-подход был популяризован библиотекой QuickCheck (Haskell), и в этой статье будет показано, как пользоваться этим инструментом эффективно.

По мнению автора этой статьи, мастерство PBT заключается в способности создать быстрый и эффективный генератор данных, позволяющий получить потенциально "проблематичные" значения. Для этого нужны как знания из предметной области, так и владение инструментами, предоставляемыми библиотекой QuickCheck.

Тип, отвечающий за генерацию данных - это обёртка над функцией, которая принимает генератор псевдо-случайности, и также Int-параметр, отвечающий за ограничение размера для генерируемых данных.


```
newtype Gen a = MkGen { unGen :: QCGen -> Int -> a }
```

Ограничение размера нужно для индуктивных типов, например, глубины дерева (чтобы дерево не было бесконечным).

Изменить размер генератора можно с помощью функций scale и resize, а получить параметр текущего генератора можно с помощью sized:

```
resize :: Int -> Gen a -> Gen a
scale :: (Int -> Int) -> Gen a -> Gen a
sized :: (Int -> Gen a) -> Gen a
```

В этой статье будет показано, как протестировать реальный код с помощью QuickCheck. В качестве примера, рассмотрим наивный парсер и сериалайзер для подмножества JSON, в котором нет типов Boolean и Null, а также запрещены пробелы, которые не обязательны в JSON.

Тип данных задаётся так:

```
data Json
  = Object [(String, Json)]
  | Array [Json]
  | String String
  | Number Double
  deriving (Show, Eq, Generic)
```


Чтобы установить лимит на размер дерева, мы должны недопустить создание новых ветвей, если параметр size равен нулю, а также обеспечить уменьшение параметра в рекурсивных вызовах.

Напишем правильный инстанс:

```
instance Arbitrary Json where
  arbitrary = sized arbitrary'
    where
      arbitrary' 0 = pure $ Array []
      arbitrary' n =
        oneof [ Object <$> resize (n `div` 2) arbitrary
              , Array <$> resize (n `div` 2) arbitrary
              , String <$> arbitrary
              , Number <$> arbitrary
              ]
```

Стоит обратить внимание на то, что здесь мы делим size на 2, а не вычитаем единицу. Инстанс Arbitrary для списка породит список длины, не превышающей size. Это позволяет нам сделать зависимость среднего размера дерева от size логарифмической, а не экспоненциальной. На практике нам не требуется линейная зависимость, нужно лишь, чтобы не было расхождения из-за того, что каждый из конструкторов Array или Object порождает бесконечное дерево (в силу того, что выход из рекурсии случается статистически реже, чем порождение новых конструкторов). Выбор константы "2" здесь произволен.

Протестируем наш генератор:

```
 V1> generate (arbitrary  :: Gen Json)
Object [("%\1003607*SF\STX\166973ti\59844B",Array [Number 3.575309217492902]),("",String "4\USO\DLE\1065483e\STX\FS}\146478"),("\DEL\59467AU\1020011\997210I\57595\EM\fDm\DEL",Object []),("sX%8\1083528D-r\146202{S",Array [Array [],Array [Object [],Array [],Array []]]),("",Number 4.890220146762664),("\158800m\1047365\&07",Array [String "\5524\1069330"])]

 V1> generate (arbitrary  :: Gen Json)
String "\ACK[Q\1038205\64353EFz|\159918\77959\&0\1013613-\12406\1042312"

 V1> generate (arbitrary  :: Gen Json)
Number (-6.706967599855459)
```

Как видно, Arbitrary-инстанс для типа String генерирует "потенциально проблематичные" строки, чтобы попытаться спровоцировать типичные ошибки в приложениях, использующих эти данные. Программисту стоит помнить о специальных символах, пустах строках, whitespace и т.п..

Теперь реализуем сериалайзер и парсер для нашего типа данных.

Сериализация реализуется достаточно прямолинейно:

```haskell
serialize :: Json -> String
serialize (Object props) =
  "{" ++ intercalate "," (map toKeyValue props) ++ "}"
  where
    toKeyValue (key, value) = serializeString key ++ ":" ++ serialize value
serialize (Array entries) =
  "[" ++ intercalate "," (map serialize entries) ++ "]"
serialize (String str) = show str
serialize (Number n) = show n
```

А для парсинга воспользуемся стандартным подходом, который используется в библиотеках комбинаторов парсеров - все функции для парсина будут иметь тип `String -> Maybe (a, String)`, где a - тип, который мы хотим получить в результате, а второй компонент пары (имеющий тип String) - оставшаяся непоглощённой парсером часть строки.

Удобство выбранного нами формата в том, что по первому символу мы можем сказать, с каким типом мы имеем дело, поэтому бэктрекинг не нужен.

Здесь мы не будем останавливаться на самом коде и перейдём к тестированию.

```
decode :: String -> Maybe (Json, String)
decode ('{' : rest)                = first Object <$> decodeProps rest
decode ('[' : rest)                = first Array <$> decodeArray rest
decode ('"' : rest)                = first String <$> decodeString rest
decode (c : rest)
  | isDigit c = first Number <$> decodeNumber (c:rest)
decode _ = Nothing

decodeProps :: String -> Maybe ([(String, Json)], String)
decodeProps ('}' : rest) = Just ([], rest)
decodeProps (',' : rest) = decodeProps rest
decodeProps ('"' : input) = do
  (key, ':' : input') <- decodeString input
  (value, input'') <- decode input'
  (restProps, input''') <- decodeProps input''
  return ((key, value) : restProps, input''')

decodeList :: String -> Maybe ([Json], String)
decodeList (']' : rest) = Just ([], rest)
decodeList (',' : rest) = decodeList rest
decodeList input = do
  (entry, rest) <- decode input
  first (entry :) <$> decodeList rest

decodeString :: String -> Maybe (String, String)
decodeString ('\\' : '"' : rest) = first ('"' :) <$> decodeString rest
decodeString ('"' : rest)        = Just ("", rest)
decodeString (c : rest)          = first (c :) <$> decodeString rest
decodeString _                   = Nothing

decodeNumber :: String -> Maybe (Double, String)
decodeNumber = listToMaybe . reads
```

Наконец, parse позволит получить результат только если строка успешно распарсилась с помощью decode:

```
parse :: String -> Maybe Json
parse input = case decode input of
  Just (json, "") -> Just json
  _               -> Nothing
```

Сформулируем свойство, которое мы хотим протестировать:

```
prop_serialize_parse :: Json -> Property
prop_serialize_parse json = parse (serialize json) === Just json
```

И запустим проверку данного свойства:

```
 V1> quickCheck prop_serialize_parse
*** Failed! Falsified (after 4 tests and 2 shrinks):
Array [String "\n"]
```

Очевидно, мы забыли про часть escape-последовательностей, которые используются в инстансе `Show String`.

Для простоты, откажемся от собственной реализации `decodeString` и переиспользуем `reads` из `Prelude`.

Предполагается, что открывающая кавычка была поглощена вызывающей функцией, поэтому ее надо вернуть:

```
decodeString :: String -> Maybe (String, String)
decodeString = listToMaybe . reads . ('"':)
```

Обнаруживаем также, что числа могут быть отрицательными:

```
*> verboseCheck prop_serialize_parse
Failed:
*** Failed! Falsified (after 5 tests and 5 shrinks):
Array [Object [("",Number (-1.0))]]
```

Что также легко учесть в нашем парсере:

```
decode (c : rest)
  | isDigit c || c == '-' = first Number <$> decodeNumber (c:rest)
```


```
> quickCheck prop_serialize_parse
*** Failed! Falsified (after 4 tests and 7 shrinks):
Object [("",Object [("",String "\n")])]
```

Попробуем протестировать нашу реализацию подмножества JSON относительно существующей (библиотека aeson). Мы хотим убедиться, что сериализация возвращает валидный JSON:


```
prop_serialize_returns_json :: Json -> Property
prop_serialize_returns_json json = Aeson.decode @Aeson.Value (BS.pack $ serialize json) =/= Nothing
```


Результат:

```
*** Failed! Falsified (after 4 tests):
String "\ETB\171675^\153309mX"
Nothing == Nothing
```

Конечно же, мы неправильно работаем с escape-последовательностями. Инстанс Show для String не обрабатывает их так же, как декодировщик из `aeson`.

Конечно, нам следовало бы реализовать сериализацию правильно, но т.к. этот пост о QuickCheck, интереснее будет показать, как заставить QuickCheck не проверять значения, которые заведомо нам "не интересны".

Допустим, мы решили ограничить строки только печатными символами из диапазона кодов 32-126.

Для этого можно использовать функцию `suchThat :: Gen a -> (a -> Bool) -> Gen a`:

```
instance Arbitrary Json where
  arbitrary = sized arbitrary'
    where
      arbitraryString =
        arbitrary `suchThat` all ((\code -> code >= 32 && code <= 126) . ord)
      arbitrary' 0 = pure $ Array []
      arbitrary' n =
        oneof [ Object <$> listOf
                ((,) <$> arbitraryString <*> resize (n `div` 2) arbitrary)
              , Array <$> resize (n `div` 2) arbitrary
              , String <$> arbitraryString
              , Number <$> arbitrary
              ]
```

Запустив такой тест, мы заметим, что время его работы существенно возросло, т.к. теперь мы отбрасываем те строки, в которых есть хотя бы один символ не из заданного интервала.

Следующий код даёт понять, что мы используем примерно 6% сгенерированных примеров:

```
 > quickCheck (\s -> classify (all ((\code -> code >= 32 && code <= 126) . ord) s) "useful" (s === s))
+++ OK, passed 100 tests (6% useful).
```

Поэтому такой способ генерации строк не годится. Чтобы тесты были быстрыми, необходимо всегда стараться сразу генерировать данные, удовлетворяющие нужным инвариантам, а не использовать suchThat и подобные функции.

Ситуация становится немного лучше, если поместить suchThat внутрь `listOf` (`String` в Haskell - это список символов (`[Char]`)):

```
arbitraryString =
  listOf (arbitrary `suchThat` ((\code -> code >= 32 && code <= 126) . ord))
```

Однако, ещё быстрее сразу генерировать символ из заданного интервала:

```
arbitraryString = listOf (chr <$> chooseInt (32, 126))
```

К сожалению, далеко не всегда легко написать генератор, выдающий только значения, удовлетворяющие определённому предикату, особенно если предикат требует каким-то образом связанных ограничений на различные части структуры.

## Shrinking

Shrinking -- способ "уменьшить" найденный пример до минимального возможного. Функция `shrink :: Arbitrary a => a -> [a]` вступает в дело, когда контрпример уже найден.

shrink должна возвращать конечный (и, возможно, пустой) список всех возможных "упрощений" значения с типом a. Пустой список означает, что минимальный контрпример найден.

Просмотреть результат работы shrink можно, запустив `verboseCheck`. Допустим, мы хотим проверить достаточно странное утверждение о том, что все строки не содержат ровно двух символов 'a'. После нахождения первого контрпримера мы видим, как shrink пытается последовательно уменьшить строку:

```
> verboseCheck (\str -> 2 /= length (filter (== 'a') str))

  ...

Failed:
"a8aL"

Passed:
""

Passed:
"aL"

Passed:
"a8"

Passed:
"8aL"

Failed:
"aaL"

Passed:
""

Passed:
"aL"

Passed:
"aL"

Failed:
"aa"

Passed:
""

Passed:
"a"

Passed:
"a"

*** Failed! Falsified (after 69 tests and 10 shrinks):
"aa"
```

Этот алгоритм поиска реализован в функции shrinkList:

```
shrinkList :: (a -> [a]) -> [a] -> [[a]]
shrinkList shr xs = concat [ removes k n xs | k <- takeWhile (>0) (iterate (`div`2) n) ]
                 ++ shrinkOne xs
 where
  n = length xs

  shrinkOne []     = []
  shrinkOne (x:xs) = [ x':xs | x'  <- shr x ]
                  ++ [ x:xs' | xs' <- shrinkOne xs ]

  removes k n xs
    | k > n     = []
    | null xs2  = [[]]
    | otherwise = xs2 : map (xs1 ++) (removes k (n-k) xs2)
   where
    xs1 = take k xs
    xs2 = drop k xs
```

shrinkList пытается:

- Удалить половину списка, четверть списка, восьмую часть и т.д. с конца и с начала
- Применить shrink к одному из элементов списка

Напишем shrink для Json (просто переиспользуем shrink для пары, списка, строки и числа):

```
shrink (Object props)  = Object <$> shrink props
shrink (Array entries) = Array <$> shrink entries
shrink (String str)    = String <$> shrink str
shrink (Number n)      = Number <$> shrink n
``

-- TODO: использовать generic-random


```
  arbitrary = genericArbitraryRec uniform `withBaseCase` return (Array [])
```
