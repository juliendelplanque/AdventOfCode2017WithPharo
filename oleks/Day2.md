
# Advent of Code 2017: Day 2

This is my solution to [Day 2: Corruption Checksum](http://adventofcode.com/2017/day/2) puzzles from [Advent of Code 2017](http://adventofcode.com/2017). All code blocks were automatically genarated from my [Pharo](https://pharo.org) image with [Epicea Markdown Exporter](https://medium.com/@i.oleks/epicea-markdown-exporter-2d594dd62cbd). The solution is based on [DataFrame](https://github.com/PolyMathOrg/DataFrame) - a new data structure for spreadsheet-like datasets.

## Table of Contents
1. [Setting up the environment](#setting-up-the-environment)
2. [Part 1](#part-1)
    1. [Puzzle](#puzzle)
    2. [Solution](#solution)
    3. [Tests and results](#tests-and-results)
    4. [DataFrame issues](#dataframe-issues)
3. [Part 2](#part-2)
    1. [Puzzle](#puzzle-1)
    2. [Solution](#solution-1)
    3. [Tests and results](#tests-and-results-1)
    4. [DataFrame issues](#dataframe-issues-1)

## Setting up the environment

Both puzzles of this day can be solved with [DataFrame](https://github.com/PolyMathOrg/DataFrame). So I create `CorruptedSpreadsheet` - a subclass of `DataFrame` that will implement additional methods for calculating the sums required by these puzzles.

```Smalltalk
DataFrame subclass: #CorruptedSpreadsheet
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'AdventOfCode2017-Day2'
```

I also create a `TestCase` to test my solutions on the examples provided in the [puzzle descriptions](http://adventofcode.com/2017/day/2). This way I will verify that my solutions are correct.

```Smalltalk
TestCase subclass: #CorruptedSpreadsheetTests
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'AdventOfCode2017-Day2'
```

The given puzzle input is rather big, so I will put it into a CSV file '/Users/oleks/Desktop/day2.csv' and read it from there. I know that all the values are integers, but [NeoCSVReader](https://ci.inria.fr/pharo-contribution/job/EnterprisePharoBook/lastSuccessfulBuild/artifact/book-result/NeoCSV/NeoCSV.html) used by DataFrame to read CSV files returns does not automatically parse strings to numbers whenever it's possible (I thing that this step should be done by DataFrame, so I'm creating an [issue](https://github.com/PolyMathOrg/DataFrame/issues/20) for it).

Until the desired functionality is added, I can modify my subclass to simply convert all values to integers every time a `CorruptedSpreadsheet` is initialized. This can be done by overriding the `DataFrame >> initializeRows:` method which is called by `DataFrame >> fromCSV:`.

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

### Puzzle
As you walk through the door, a glowing humanoid shape yells in your direction. "You there! Your state appears to be idle. Come help us repair the corruption in this spreadsheet - if we take another millisecond, we'll have to display an hourglass cursor!"

The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.

### Solution

```Smalltalk
CorruptedSpreadsheet >> checksum
    "Calculates the checksum of a spreadsheet by summing up
    the ranges (difference between the largest value and
    the smallest value) of all its rows."

    ^ (1 to: self numberOfRows) inject: 0 into: [ :sum :i |
            sum + (self rowAt: i) range ].
```

### Tests and results

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

### DataFrame issues

## Part 2

### Puzzle
"Great work; looks like we're on the right track after all. Here's a star for your effort." However, the program seems a little worried. Can programs be worried?

"Based on what we're seeing, it looks like all the User wanted is some information about the evenly divisible values in the spreadsheet. Unfortunately, none of us are equipped for that kind of calculation - most of us specialize in bitwise operations."

It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.

### Solution

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

### Tests and results

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

### DataFrame issues
