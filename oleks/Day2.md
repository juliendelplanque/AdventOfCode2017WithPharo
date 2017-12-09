```Smalltalk
spreadsheet := DataFrame
    fromCSV: '/Users/oleks/Desktop/day2.csv'
    separator: Character tab.

spreadsheet
    toColumns: spreadsheet columnNames
    applyElementwise: [ :cell | cell asNumber ].

spreadsheet columnTypes.

divs:= DataSeries fromArray:
    ((1 to: spreadsheet numberOfRows) collect: [ :k |
        | row result div |
        row :=spreadsheet rowAt: k.
        result = 0.

        1 to: row size do: [ :i |
            1 to: row size do: [ :j |
                (i ~= j) ifTrue: [
                    div := (row at: i) / (row at: j).

                    div isInteger ifTrue: [
                        result := div ] ] ] ].

        result ]).

divs sum.
```


