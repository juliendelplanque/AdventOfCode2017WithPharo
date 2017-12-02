# Advent of Code 2017: Day 1
1. [Homepage of Day 1 puzzle](http://adventofcode.com/2017/day/1)
2. [Complete code of this solution](https://github.com/olekscode)

## Puzzle description
You're standing in a room with "digitization quarantine" written in LEDs along one wall. The only door is locked, but it includes a small interface. "Restricted Area - Strictly No Digitized Users Allowed."

It goes on to explain that you may only leave by solving a captcha to prove you're not a human. Apparently, you only get one millisecond to solve the captcha: too fast for a normal human, but it feels like hours to you.

The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.

For example:

* 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
* 1111 produces 4 because each digit (all 1) matches the next.
* 1234 produces 0 because no digit matches the next.
* 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

What is the solution to your captcha?

## Puzzle input
```
6592822488931338589815525425236818285229555616392928433262436847386544514648645288129834834862363847542262953164877694234514375164927616649264122487182321437459646851966649732474925353281699895326824852555747127547527163197544539468632369858413232684269835288817735678173986264554586412678364433327621627496939956645283712453265255261565511586373551439198276373843771249563722914847255524452675842558622845416218195374459386785618255129831539984559644185369543662821311686162137672168266152494656448824719791398797359326412235723234585539515385352426579831251943911197862994974133738196775618715739412713224837531544346114877971977411275354168752719858889347588136787894798476123335894514342411742111135337286449968879251481449757294167363867119927811513529711239534914119292833111624483472466781475951494348516125474142532923858941279569675445694654355314925386833175795464912974865287564866767924677333599828829875283753669783176288899797691713766199641716546284841387455733132519649365113182432238477673375234793394595435816924453585513973119548841577126141962776649294322189695375451743747581241922657947182232454611837512564776273929815169367899818698892234618847815155578736875295629917247977658723868641411493551796998791839776335793682643551875947346347344695869874564432566956882395424267187552799458352121248147371938943799995158617871393289534789214852747976587432857675156884837634687257363975437535621197887877326295229195663235129213398178282549432599455965759999159247295857366485345759516622427833518837458236123723353817444545271644684925297477149298484753858863551357266259935298184325926848958828192317538375317946457985874965434486829387647425222952585293626473351211161684297351932771462665621764392833122236577353669215833721772482863775629244619639234636853267934895783891823877845198326665728659328729472456175285229681244974389248235457688922179237895954959228638193933854787917647154837695422429184757725387589969781672596568421191236374563718951738499591454571728641951699981615249635314789251239677393251756396
```

## Solution
To solve this puzzle I create a simple class `ReverseCaptchaSolver` with class-side method `solve:` that finds a solution to a given captcha represented by a string of digits. Here is an example of how this class will be used:

```Smalltalk
ReverseCaptchaSolver solve: '1122'. "3"
```
I put this class inside  `Day1` tag of my `AdventOfCode2017` package

```Smalltalk
Object subclass: #ReverseCaptchaSolver
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'AdventOfCode2017-Day1'
```
Summing up all digits that match the next digit in the list is an easy task. The only tricky part is that the list must be circular. We should compare every digit except the last one to the digit right next to it and then compare the last digit to the first one. The most clean and easy solution that comes to my mind is adding the first digit to the end of the list. This way `1122` becomes `11221` and `91212129` becomes `912121299`.

```Smalltalk
makeCircular: aStringOfDigits
    "Makes captcha string circular by adding its first digit to the end of it"

    | firstDigit |

    firstDigit := aStringOfDigits first asString.
    ^ aStringOfDigits, firstDigit.
```

Now we just need to iterate through all the digits of the given capcha (except that last digit that we added) and compare each one of them to the next digit in the list.

```Smalltalk
solve: captchaString
    "Returns the sum of all digits that match the next digit in the circular list (last digit should match the first one)"

    | circularString |
    circularString := self makeCircular: captchaString.

    ^ (1 to: circularString size - 1) inject: 0 into: [ :sum :i |
        | firstDigit secondDigit |
        firstDigit := (circularString at: i) asString asNumber.
        secondDigit := (circularString at: i+1) asString asNumber.

        firstDigit = secondDigit
            ifTrue: [ sum + firstDigit ]
            ifFalse: [ sum ] ].
```

To test my solution I wrote 4 tests corresponding to the 4 examples provided in puzzle description. For instance, we expect captcha '1122' to be solved as 3.

```Smalltalk
testExample1

    | captchaString expectedSum actualSum |

    captchaString := '1122'.
    expectedSum := 3.
    actualSum := ReverseCaptchaSolver solve: captchaString.

    self assert: actualSum equals: expectedSum.
```

## Answer
The answer to this puzzle is **1029**.
