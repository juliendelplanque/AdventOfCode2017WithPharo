

```Smalltalk
DataFrame subclass: #CorruptedSpreadsheet
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'AdventOfCode2017-Day2'
```

```Smalltalk
TestCase subclass: #CorruptedSpreadsheetTests
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'AdventOfCode2017-Day2'
```

```Smalltalk
CorruptedSpreadsheet >> initializeRows: anArrayOfArrays
    "Overrides DataFrame>>initializeRows to automatically
    convert the values retrieved from CSV file with
    DataFrame class class >> fromCSV: to Number"
    
    super initializeRows: anArrayOfArrays.
    
    self
        toColumns: self columnNames
        applyElementwise: [ :cell | cell asNumber ].
        
    ^ self
```

## Part 1

```Smalltalk
CorruptedSpreadsheet >> checksum
    "Calculates the checksum of a spreadsheet by summing up
    the ranges (difference between the largest value and
    the smallest value) of all its rows."

    ^ (1 to: self numberOfRows) inject: 0 into: [ :sum :i |
            sum + (self rowAt: i) range ].
```

```Smalltalk
CorruptedSpreadsheetTests >> testExample1

    | spreadsheet expectedChecksum actualChecksum |
    
    spreadsheet := CorruptedSpreadsheet fromRows: #(
        (5 1 9 5)
        (7 5 3 4)
        (2 4 6 8)).
        
    expectedChecksum := 18.
    actualChecksum := spreadsheet checksum.
    
    self assert: actualChecksum equals: expectedChecksum.
```

```Smalltalk
CorruptedSpreadsheet class class >> exampleAnswerPart1
<gtExample>

    | spreadsheet answer |
    
    spreadsheet := self
        fromCSV: '/Users/oleks/Desktop/day2.csv'
        separator: Character tab.

    answer := spreadsheet checksum.
    ^ answer
```

## Part 2

```Smalltalk
CorruptedSpreadsheet >> sumOfEvenDivisions
    "Finds the only two numbers in each row where one evenly
    divides the other - that is, where the result of the
    division operation is a whole number. Returns the sum of
    these divisions across all rows"

    ^ ((1 to: self numberOfRows) inject: 0 into: [ :sum :k |
        | row div evenDiv |
      row :=self rowAt: k.
        evenDiv := 0.

        1 to: row size do: [ :i |
            1 to: row size do: [ :j |
                (i ~= j) ifTrue: [
                    div := (row at: i) / (row at: j).
                    div isInteger ifTrue: [
                        evenDiv := div ] ] ] ].
        sum + evenDiv ]).
```

```Smalltalk
CorruptedSpreadsheetTests >> testExample2

    | spreadsheet expectedSum actualSum |
    
    spreadsheet := CorruptedSpreadsheet fromRows: #(
        (5 9 2 8)
        (9 4 7 3)
        (3 8 6 5)).
        
    expectedSum := 9.
    actualSum := spreadsheet sumOfEvenDivisions.
    
    self assert: actualSum equals: expectedSum.
```

```Smalltalk
CorruptedSpreadsheet class class >> exampleAnswerPart2
<gtExample>

    | spreadsheet answer |
    
    spreadsheet := self
        fromCSV: '/Users/oleks/Desktop/day2.csv'
        separator: Character tab.

    answer := spreadsheet sumOfEvenDivisions.
    ^ answer
```

