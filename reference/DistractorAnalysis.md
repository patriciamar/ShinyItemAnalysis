# Distractor analysis

Performs distractor analysis for each item and optional number of
groups.

## Usage

``` r
DistractorAnalysis(
  Data,
  key,
  item = "all",
  p.table = FALSE,
  num.groups = 3,
  criterion = NULL,
  crit.discrete = FALSE,
  cut.points,
  data,
  matching,
  match.discrete
)
```

## Arguments

- Data:

  character: data matrix or data.frame with rows representing unscored
  item responses from a multiple-choice test and columns corresponding
  to the items.

- key:

  character: answer key for the items. The `key` must be a vector of the
  same length as `ncol(Data)`. In case it is not provided, `criterion`
  needs to be specified.

- item:

  numeric or character: either character `"all"` to apply for all items
  (default), or a vector of item names (column names of `Data`), or item
  identifiers (integers specifying the column number).

- p.table:

  logical: should the function return the proportions? If `FALSE`
  (default), the counts are returned.

- num.groups:

  numeric: number of groups to which are the respondents split.

- criterion:

  numeric: numeric vector. If not provided, total score is calculated
  and distractor analysis is performed based on it.

- crit.discrete:

  logical: is `criterion` discrete? Default value is `FALSE`. See
  details.

- cut.points:

  numeric: numeric vector specifying cut points of `criterion`. See
  details.

- data:

  deprecated. Use argument `Data` instead.

- matching:

  deprecated. Use argument `criterion` instead.

- match.discrete:

  deprecated. Use argument `crit.discrete` instead.

## Details

This function is an adapted version of the `distractor.analysis()`
function from CTT package. In case that no `criterion` is provided, the
scores are calculated using the item `Data` and `key`. The respondents
are by default split into the `num.groups`-quantiles and the number (or
proportion) of respondents in each quantile is reported with respect to
their answers. In case that `criterion` is discrete
(`crit.discrete = TRUE`), `criterion` is split based on its unique
levels. Other cut points can be specified via `cut.points` argument.

## Author

Adela Hladka  
Institute of Computer Science of the Czech Academy of Sciences  
<hladka@cs.cas.cz>  

Patricia Martinkova  
Institute of Computer Science of the Czech Academy of Sciences  
<martinkova@cs.cas.cz>  

## Examples

``` r
Data <- dataMedicaltest[, 1:100]
Databin <- dataMedical[, 1:100]
key <- dataMedicalkey

# distractor analysis for all items
DistractorAnalysis(Data, key)
#> $X2001
#>         score.level
#> response Group1 Group2 Group3
#>     A       197    250    314
#>     AB       38     38     35
#>     ABC       4      4      4
#>     ABCD     10      9      6
#>     ABD      12     11      3
#>     AC       11      8      2
#>     ACD       4     10     14
#>     AD       10     17     11
#>     B       279    259    230
#>     BC       17      5      1
#>     BCD       8      4      5
#>     BD       22      7      4
#>     C        93     39     16
#>     CD        5      4      2
#>     D       117    120    123
#> 
#> $X2002
#>         score.level
#> response Group1 Group2 Group3
#>     A       132     63     32
#>     AB       87     31     16
#>     ABC       9      6      4
#>     ABCD     18     40      6
#>     ABD      67     59     43
#>     AC       61     51     11
#>     ACD      36     58     42
#>     AD      167    214    179
#>     B        26     11      1
#>     BC       20     11      4
#>     BCD      17     29     38
#>     BD       41     46    122
#>     C        35     27      9
#>     CD       59     61     57
#>     D        41     78    204
#> 
#> $X2003
#>         score.level
#> response Group1 Group2 Group3
#>     A        37      3      0
#>     AB       48      7      0
#>     ABC      19      7      0
#>     ABCD     37     25     10
#>     ABD      28      8      0
#>     AC       56     25      8
#>     ACD      71     74     40
#>     AD       36     14      7
#>     B        53      7      2
#>     BC       70     59     24
#>     BCD      95    165    117
#>     BD       51     17      5
#>     C        68     63     55
#>     CD      128    300    500
#>     D        24     13      3
#> 
#> $X2004
#>         score.level
#> response Group1 Group2 Group3
#>     A       210    144     79
#>     AB       58     28     13
#>     ABC      11     12      6
#>     ABCD     10     17     16
#>     ABD       8     10      6
#>     AC       28     15     13
#>     ACD      13     14     13
#>     AD       30     28     11
#>     B        95     72     29
#>     BC       23      4      2
#>     BCD       9      5      5
#>     BD       23      6      3
#>     C        90     93     80
#>     CD       21     46     27
#>     D       183    289    467
#> 
#> $X2005
#>         score.level
#> response Group1 Group2 Group3
#>     A       275    404    510
#>     AB       64     49     15
#>     ABC     132    106     56
#>     ABCD     15     19     12
#>     ABD       4      2      0
#>     AC       92     65     60
#>     ACD       3      2      4
#>     AD       26     48     58
#>     B        43     29      8
#>     BC       57     33     31
#>     BCD       5      6      1
#>     BD        3      3      0
#>     C        80     20     13
#>     CD        5      0      1
#>     D        16      1      1
#> 
#> $X2006
#>         score.level
#> response Group1 Group2 Group3
#>      A      131     70     22
#>      AB       1      0      0
#>      ABD      1      0      0
#>      AD       1      0      1
#>      B      371    523    659
#>      BC       0      2      0
#>      BD       0      1      0
#>      C      142     65     35
#>      D      167    118     50
#> 
#> $X2007
#>         score.level
#> response Group1 Group2 Group3
#>       A     137     57     19
#>       AB      1      0      0
#>       B     485    659    728
#>       BC      1      1      0
#>       BD      1      0      0
#>       C      88     26     16
#>       CD      1      0      0
#>       D     100     44      6
#> 
#> $X2008
#>         score.level
#> response Group1 Group2 Group3
#>      A      178    116     80
#>      ABD      1      0      0
#>      AC       0      1      0
#>      AD       1      0      0
#>      B      150    117    100
#>      C      249    193     94
#>      D      233    352    493
#> 
#> $X2009
#>         score.level
#> response Group1 Group2 Group3
#>     A       256    391    395
#>     AB        1      1      0
#>     ABCD      1      0      0
#>     AC        1      1      0
#>     ACD       0      0      1
#>     AD        1      1      0
#>     B       165    131    213
#>     BC        3      0      1
#>     C       278    156     91
#>     D       104     94     61
#> 
#> $X2010
#>         score.level
#> response Group1 Group2 Group3
#>       A     223    171     93
#>       AB      4      0      1
#>       AD      3      0      0
#>       B     231     97     41
#>       BC      1      1      0
#>       BD      2      0      0
#>       C     264    429    560
#>       CD      2      0      0
#>       D      88     85     72
#> 
#> $X2011
#>         score.level
#> response Group1 Group2 Group3
#>     A       104     31      9
#>     AB        7      2      1
#>     ABC       2      0      0
#>     ABCD      1      0      0
#>     ABD       1      0      0
#>     AC        5      2      0
#>     ACD       0      1      0
#>     AD       13      1      3
#>     B       217    282    234
#>     BC       13      9      3
#>     BCD       4      5      2
#>     BD       42    141    290
#>     C       169    113     94
#>     CD       30     19     11
#>     D       218    182    123
#> 
#> $X2012
#>         score.level
#> response Group1 Group2 Group3
#>     A       155    104     64
#>     AB       62     35     23
#>     ABC      18     17      5
#>     ABCD      2      0      2
#>     ABD       3      5      3
#>     AC       31     18     10
#>     ACD       3      0      0
#>     AD       33      8      1
#>     B       105     60     30
#>     BC       29     16      6
#>     BCD       1      2      1
#>     BD       10      4      1
#>     C       252    449    589
#>     CD       13     14     10
#>     D       108     52     25
#> 
#> $X2013
#>         score.level
#> response Group1 Group2 Group3
#>     A       343    587    725
#>     AB        4      1      0
#>     ABC       6      3      0
#>     ABCD      1      0      0
#>     AC        8      6      2
#>     B       289    167     40
#>     BC       11      1      0
#>     BD        2      0      0
#>     C        89     16      3
#>     CD        4      0      0
#>     D        74      9      1
#> 
#> $X2014
#>         score.level
#> response Group1 Group2 Group3
#>     A       152    180    151
#>     AB      138    288    435
#>     ABC      50     42     42
#>     ABCD     10      7      2
#>     ABD      30     35     23
#>     AC      141     86     49
#>     ACD      44      8      4
#>     AD       66     57     26
#>     B        31     29     18
#>     BC       25     20     10
#>     BCD      18      5      0
#>     BD       27     10      4
#>     C        37      5      1
#>     CD       30      5      3
#>     D        29     10      2
#> 
#> $X2015
#>         score.level
#> response Group1 Group2 Group3
#>      A       49     40     31
#>      AB       1      0      1
#>      ABD      3      0      0
#>      AC      10      7      2
#>      AD       3      2      0
#>      B      177     74     35
#>      BC      32     27      6
#>      BCD      1      0      1
#>      BD       4      1      0
#>      C       96     74     36
#>      CD     139    334    455
#>      D      312    230    203
#> 
#> $X2016
#>         score.level
#> response Group1 Group2 Group3
#>     A        36     23     13
#>     AB       48     22      4
#>     ABC       2      1      3
#>     ABCD      5      4      1
#>     ABD       9      2      0
#>     AC       21     10      8
#>     ACD       4      3      2
#>     AD       25     22     22
#>     B       248    218     85
#>     BC       68     18      4
#>     BCD      25     18      7
#>     BD       99     93     44
#>     C        60      8      6
#>     CD       43     19     14
#>     D       131    324    557
#> 
#> $X2017
#>         score.level
#> response Group1 Group2 Group3
#>     A        69     14      0
#>     AB      194    153     59
#>     ABC      50     76     73
#>     ABCD     28     19      7
#>     ABD      32     29     12
#>     AC       26     13      7
#>     ACD      10      4      2
#>     AD       14      8      1
#>     B       124    147     85
#>     BC       87    185    446
#>     BCD      28     57     44
#>     BD       71     59     27
#>     C        35     17      4
#>     CD       35      5      1
#>     D        23      2      1
#> 
#> $X2018
#>         score.level
#> response Group1 Group2 Group3
#>     A        75     37     16
#>     AB       21     15      8
#>     ABC       5      3      0
#>     ABCD      7      0      0
#>     ABD       5      4      0
#>     AC       63    238    534
#>     ACD      20     61     38
#>     AD       49     31      1
#>     B        82     20      1
#>     BC       19      3      1
#>     BCD       3      3      0
#>     BD       46     29      3
#>     C       197    176    121
#>     CD      117    132     44
#>     D       116     37      4
#> 
#> $X2019
#>         score.level
#> response Group1 Group2 Group3
#>     A        90     64     52
#>     AB       34     15      7
#>     ABC       1      0      0
#>     ABCD      6     10      3
#>     ABD       1      0      0
#>     AC       55    146    272
#>     ACD       1      1      3
#>     AD       49     29     13
#>     B       111     64     43
#>     BC      111     75     31
#>     BCD       2      0      1
#>     BD       29     23     21
#>     C       155    172    147
#>     CD       52     23     17
#>     D       123    162    158
#> 
#> $X2020
#>         score.level
#> response Group1 Group2 Group3
#>     A        72     80     41
#>     AB       88     62     36
#>     ABC      29     33     20
#>     ABCD     53     26      8
#>     ABD      19      9      6
#>     AC       53     28     15
#>     ACD      15      9      6
#>     AD       69     33     17
#>     B        59     54     70
#>     BC      113    115    102
#>     BCD      49    119    290
#>     BD       64     70     70
#>     C        36     23     14
#>     CD       53     57     31
#>     D        43     64     42
#> 
#> $X2021
#>         score.level
#> response Group1 Group2 Group3
#>     A        49     41     19
#>     AB       22     10      7
#>     ABC       5      8      1
#>     ABCD      2      7      4
#>     ABD       0      1      2
#>     AC       92     67     39
#>     ACD      22     37     41
#>     AD       39     47     32
#>     B        82     53     27
#>     BC       60     37     18
#>     BCD      11     12     16
#>     BD       18     29      7
#>     C       187    117     89
#>     CD      133    208    320
#>     D        94    113    144
#> 
#> $X2022
#>         score.level
#> response Group1 Group2 Group3
#>     A        90     47     36
#>     AB       51     22      5
#>     ABC      37     30      3
#>     ABCD     11      4      2
#>     ABD       6      3      0
#>     AC      156    108     41
#>     ACD       6      5      2
#>     AD       18      6      3
#>     B        62     39     23
#>     BC       82     90     70
#>     BCD       9      4      6
#>     BD        5      6      0
#>     C       276    405    560
#>     CD       14     17     12
#>     D         3      1      5
#> 
#> $X2023
#>         score.level
#> response Group1 Group2 Group3
#>     A        51     28     14
#>     AB       64     38     16
#>     ABC      36     39     30
#>     ABCD     34     33     17
#>     ABD      21      4      5
#>     AC      112    137    293
#>     ACD      26     24     20
#>     AD       76     41     17
#>     B        66     78     78
#>     BC       70     62     43
#>     BCD      16     19      2
#>     BD       72     87     45
#>     C        63     92    142
#>     CD       59     44     20
#>     D        52     54     23
#> 
#> $X2024
#>         score.level
#> response Group1 Group2 Group3
#>      A       65     30      7
#>      AB      22     15      5
#>      ABC      2      0      0
#>      ABD      8     16     12
#>      AC       4      1      0
#>      ACD      1      0      0
#>      AD       7      3      1
#>      B      297    181    124
#>      BC       2      1      0
#>      BD     263    462    583
#>      C       66     39     23
#>      CD      16      6      2
#>      D       74     36     14
#> 
#> $X2025
#>         score.level
#> response Group1 Group2 Group3
#>     A        29      4      2
#>     AB       10      0      0
#>     ABC      47     12      2
#>     ABCD     30      3      1
#>     ABD      22      4      1
#>     AC       13      3      0
#>     ACD     481    685    731
#>     AD        7      3      0
#>     B        25      3      3
#>     BC        5      0      0
#>     BCD      39     13     10
#>     BD        7      1      0
#>     C         5      1      0
#>     CD       19      4      0
#>     D        91     53     21
#> 
#> $X2026
#>         score.level
#> response Group1 Group2 Group3
#>     A        69     20     11
#>     AB      227    133     53
#>     ABC       4      1      1
#>     ABCD      0      1      0
#>     ABD      32     12      0
#>     AC        6      1      0
#>     ACD       3      0      0
#>     AD       34     13      0
#>     B        91    169    162
#>     BC       81    195    444
#>     BCD      43     47     19
#>     BD       94     72     29
#>     C        59     44     29
#>     CD       30     46      6
#>     D        55     35     16
#> 
#> $X2027
#>         score.level
#> response Group1 Group2 Group3
#>       A      63     12      5
#>       AB      1      0      0
#>       AD      1      0      0
#>       B     429    581    680
#>       BC      1      0      0
#>       C     207    172     79
#>       CD      1      0      0
#>       D     123     25      7
#> 
#> $X2028
#>         score.level
#> response Group1 Group2 Group3
#>     A       169    319    331
#>     AB       35     37     23
#>     ABC       1      3      2
#>     ABCD     11      7      4
#>     ABD      14      5      2
#>     AC       27     36    150
#>     ACD      58     36     24
#>     AD       66     55     39
#>     B        62     50     29
#>     BC       37      8      3
#>     BCD      26     10      0
#>     BD       88     42     20
#>     C        66     50     64
#>     CD       92     73     41
#>     D        70     56     37
#> 
#> $X2029
#>         score.level
#> response Group1 Group2 Group3
#>      A      126     67     12
#>      AB      18      4      1
#>      ABC      2      0      0
#>      ABD      0      1      0
#>      AC      54     21      2
#>      ACD      2      3      0
#>      AD      73     70     41
#>      B      203    229    175
#>      BC      76     36     14
#>      BCD      2      2      1
#>      BD      97    259    511
#>      C       93     20      1
#>      CD       8      8      1
#>      D       68     67     12
#> 
#> $X2030
#>         score.level
#> response Group1 Group2 Group3
#>      A      128     48     29
#>      AB      16      9      4
#>      ABC      2      2      0
#>      AC      18      4      1
#>      ACD      1      0      0
#>      AD       4      2      0
#>      B      253    125     57
#>      BC      16     13      3
#>      BCD      5      0      1
#>      BD      36     25     16
#>      C      121    411    582
#>      CD      24     14      6
#>      D      196    135     71
#> 
#> $X2031
#>         score.level
#> response Group1 Group2 Group3
#>      A      227    109     51
#>      AB       8      7      2
#>      ABC      0      1      0
#>      AC       1      2      1
#>      ACD      1      0      0
#>      AD       1      2      0
#>      B      306    223    103
#>      BC       3      5      6
#>      BD       4      0      1
#>      C      169    379    557
#>      CD       2      3      1
#>      D      102     57     48
#> 
#> $X2032
#>         score.level
#> response Group1 Group2 Group3
#>     A       189    100     97
#>     AB        4      4      1
#>     ABC      10      8      4
#>     ABCD      0      1      1
#>     AC       58     58     62
#>     ACD       3      6      4
#>     AD        4      1      0
#>     B       160    222    168
#>     BC       10      0      2
#>     BCD       1      0      0
#>     BD        8      5      2
#>     C       325    344    404
#>     CD       23     29     17
#>     D        33     11      8
#> 
#> $X2033
#>         score.level
#> response Group1 Group2 Group3
#>     A        95     92     25
#>     AB       41     44     11
#>     ABC      18      8      5
#>     ABCD     11      7      1
#>     ABD      11      5      5
#>     AC       39     37     16
#>     ACD      11     20     10
#>     AD       44     48     62
#>     B       101     46     32
#>     BC       56     27     16
#>     BCD      18      9      9
#>     BD       59     62     50
#>     C       137     87     28
#>     CD       54     54     37
#>     D       112    235    460
#> 
#> $X2034
#>         score.level
#> response Group1 Group2 Group3
#>     A       250    352    512
#>     AB      145    228    168
#>     ABC       5      8      2
#>     ABCD      2      1      0
#>     ABD      23      5      1
#>     AC       58     73     38
#>     ACD       7      2      0
#>     AD      127     44     16
#>     B        51     31     19
#>     BC        2      0      1
#>     BCD       2      0      0
#>     BD       21      6      0
#>     C        27     14      5
#>     CD        5      0      1
#>     D       101     26      7
#> 
#> $X2035
#>         score.level
#> response Group1 Group2 Group3
#>     A       147     70     43
#>     AB        1      1      0
#>     ABCD      1      0      0
#>     ABD       0      0      1
#>     AC        2      0      0
#>     AD        0      1      0
#>     B       464    639    715
#>     BD        1      0      0
#>     C       153     67     11
#>     CD        2      0      0
#>     D        54     11      1
#> 
#> $X2036
#>         score.level
#> response Group1 Group2 Group3
#>     A       197    138     77
#>     AB       80    208    343
#>     ABC      14     25     24
#>     ABCD      1      1      0
#>     ABD       3      4      1
#>     AC       53     25     16
#>     ACD       3      0      0
#>     AD        8      4      1
#>     B       235    259    222
#>     BC      109     71     68
#>     BCD       8      6      5
#>     BD       12      6      2
#>     C        83     34      5
#>     CD        7      0      3
#>     D        13      6      2
#> 
#> $X2037
#>         score.level
#> response Group1 Group2 Group3
#>     A        30     40     29
#>     AB        1      1      0
#>     ABC       1      0      1
#>     ABCD      3      0      0
#>     ABD       1      0      0
#>     AC      251    506    672
#>     AD       68     33      9
#>     B        35      7      6
#>     BC      297    181     45
#>     BCD       1      0      0
#>     BD       77     12      4
#>     C        38     10      3
#>     CD        5      0      2
#>     D        22      0      0
#> 
#> $X2038
#>         score.level
#> response Group1 Group2 Group3
#>      A      280    120     41
#>      AB       4      1      0
#>      ABC      1      1      0
#>      AC       1      0      0
#>      B      447    629    680
#>      BC       1      0      0
#>      BD       0      1      1
#>      C       31      9      2
#>      CD       1      0      0
#>      D       61     28     46
#> 
#> $X2039
#>         score.level
#> response Group1 Group2 Group3
#>      A      301    520    661
#>      AB       1      0      0
#>      ABC      0      1      0
#>      ACD      1      0      0
#>      B       77     66     39
#>      BD       4      0      0
#>      C      138     73     36
#>      D      301    125     33
#> 
#> $X2040
#>         score.level
#> response Group1 Group2 Group3
#>       A      77     33     17
#>       AB      2      0      0
#>       AC      1      0      0
#>       AD      2      0      0
#>       B      90     33      7
#>       BD      1      0      0
#>       C     546    686    738
#>       CD      0      2      0
#>       D     103     34      8
#> 
#> $X2041
#>         score.level
#> response Group1 Group2 Group3
#>     A        28     21     42
#>     AB       11     14     19
#>     ABC      24     14     17
#>     ABCD    108    144    121
#>     ABD      27     83    150
#>     AC      178    148     91
#>     ACD     105    105     59
#>     AD       24     34     30
#>     B        21     13     15
#>     BC       17     16      3
#>     BCD      24     22     13
#>     BD       29     32     74
#>     C       107     75     89
#>     CD       91     41     23
#>     D        36     26     22
#> 
#> $X2042
#>         score.level
#> response Group1 Group2 Group3
#>     A       152    131    128
#>     AB      165    111     41
#>     ABC      55     61     81
#>     ABCD     16      9      7
#>     ABD      46     15      5
#>     AC      106    220    358
#>     ACD      26     44     50
#>     AD       95     67     37
#>     B        43     14      4
#>     BC       24     26      8
#>     BCD       8      7      1
#>     BD       16      4      0
#>     C        30     39     26
#>     CD       15     23     13
#>     D        31     18     10
#> 
#> $X2043
#>         score.level
#> response Group1 Group2 Group3
#>     A       295    207    143
#>     AB       53     40     19
#>     ABC       3      1      0
#>     ABCD      1      4      0
#>     ABD      25     28     23
#>     AC       31     17     11
#>     ACD       5      6      8
#>     AD      115    325    490
#>     B       119     35     14
#>     BC       10      5      0
#>     BCD       1      1      0
#>     BD       20     14      6
#>     C        67     31      7
#>     CD        9      6      6
#>     D        70     70     43
#> 
#> $X2044
#>         score.level
#> response Group1 Group2 Group3
#>     A        17     10      4
#>     AB        3      1      3
#>     ABC       3      1      0
#>     ABCD     55     40     29
#>     ABD      25     10      8
#>     AC       15     18     13
#>     ACD     175    267    354
#>     AD      121     96     94
#>     B         2      1      0
#>     BC        5      1      0
#>     BCD      56     30     17
#>     BD       45     27     12
#>     C         6      6      3
#>     CD      209    177    161
#>     D        89    102     72
#> 
#> $X2045
#>         score.level
#> response Group1 Group2 Group3
#>      A      217    304    452
#>      ABC      1      1      0
#>      ACD      1      1      0
#>      B      285    170    101
#>      BD       1      0      0
#>      C      110    130     87
#>      CD       4      0      0
#>      D      191    164    129
#> 
#> $X2046
#>         score.level
#> response Group1 Group2 Group3
#>     A        43     45     43
#>     AB       14      7      3
#>     ABC      17      4      3
#>     ABCD     28      2     13
#>     ABD      11     12     20
#>     AC       54     54     33
#>     ACD     105    180    252
#>     AD      155    252    323
#>     B         9      1      0
#>     BC       15      1      1
#>     BCD      12     11      0
#>     BD       33      5      3
#>     C        26     14      1
#>     CD      168    117     40
#>     D       138     84     36
#> 
#> $X2047
#>         score.level
#> response Group1 Group2 Group3
#>      A       43      6      0
#>      AB       1      0      0
#>      ABC      3      0      0
#>      ABD      0      1      0
#>      AC       1      0      0
#>      B      506    690    754
#>      BC       9      3      1
#>      BD       0      0      1
#>      C      243     84     15
#>      CD       2      1      0
#>      D       21      5      0
#> 
#> $X2048
#>         score.level
#> response Group1 Group2 Group3
#>     A       221    111     36
#>     AB       17      7      2
#>     ABC       6      9      5
#>     ABCD     20     19     37
#>     ABD      12      6      6
#>     AC       71    111    108
#>     ACD      53    149    249
#>     AD      111     97     68
#>     B        13      3      1
#>     BC       25     27     11
#>     BCD      29     34     23
#>     BD       27      4      3
#>     C        88    101     91
#>     CD       98     87    118
#>     D        38     25     13
#> 
#> $X2049
#>         score.level
#> response Group1 Group2 Group3
#>      A       63     19      4
#>      AB       3      0      0
#>      ABC      0      1      0
#>      AC       3      0      0
#>      ACD      1      2      0
#>      AD       5      4      4
#>      B       34     10      2
#>      BC       3      1      0
#>      BCD      3      3      0
#>      BD       4      0      1
#>      C       50      9      2
#>      CD      14      7      3
#>      D      647    734    755
#> 
#> $X2050
#>         score.level
#> response Group1 Group2 Group3
#>     A       137     92    108
#>     AB       11     27     27
#>     ABC       0      1      1
#>     ABCD      1      0      4
#>     ABD       2      7      1
#>     AC       32     21     11
#>     ACD       9      8      5
#>     AD       36     25     26
#>     B       218    331    433
#>     BC       13     11     10
#>     BCD       1      0      0
#>     BD       14     20      5
#>     C       137     97     63
#>     CD       32     16      5
#>     D       174    131     67
#> 
#> $X1
#>         score.level
#> response Group1 Group2 Group3
#>      A       30      4      2
#>      AC       0      0      1
#>      ACD      4      0      0
#>      AD       1      2      1
#>      B       47      3      5
#>      BC       0      1      0
#>      BCD      0      0      1
#>      BD       3      1      0
#>      C      328    165     77
#>      CD     401    609    680
#>      D       12      4      4
#> 
#> $X2
#>         score.level
#> response Group1 Group2 Group3
#>      A      649    697    718
#>      AB       1      1      1
#>      AC      16      7      4
#>      ACD      1      1      0
#>      AD       3      0      0
#>      B       35     34     29
#>      BC       1      0      0
#>      BD       1      0      0
#>      C       86     46     18
#>      CD       1      0      0
#>      D       34      4      1
#> 
#> $X10
#>         score.level
#> response Group1 Group2 Group3
#>     A       277    128     25
#>     AB        2      0      0
#>     ABCD      1      0      0
#>     ABD       1      0      0
#>     AC        6      4      0
#>     ACD       4      1      0
#>     AD        6      0      2
#>     B        69     13      0
#>     BC        0      4      1
#>     BCD       2      2      0
#>     BD        7      0      0
#>     C       105    109     72
#>     CD      257    488    651
#>     D        90     41     20
#> 
#> $X17
#>         score.level
#> response Group1 Group2 Group3
#>     A        75     31      4
#>     AB       29     10      6
#>     ABC      17     11      8
#>     ABCD      9      5      2
#>     ABD       8      5      1
#>     AC       22      7      0
#>     ACD       2      1      1
#>     AD       93     49      1
#>     B        75     62     28
#>     BC      350    548    709
#>     BCD      14      7      0
#>     BD       23     12      4
#>     C        45     11      4
#>     CD       15      4      0
#>     D        47     22      2
#> 
#> $X30
#>         score.level
#> response Group1 Group2 Group3
#>     A       143     75     23
#>     AB        6      1      1
#>     ABC       2      0      0
#>     ABCD      2      0      0
#>     ABD       5      2      0
#>     AC        6      1      0
#>     ACD      10      9      5
#>     AD      511    664    722
#>     B        29      2      0
#>     BC        8      0      0
#>     BCD       3      0      0
#>     BD       71     20      1
#>     C         2      1      0
#>     CD        3      1      0
#>     D        28     13     19
#> 
#> $X38
#>         score.level
#> response Group1 Group2 Group3
#>     A        31      3      0
#>     AB       20      2      0
#>     ABC      38     11      2
#>     ABCD     33     16      5
#>     ABD       9      3      0
#>     AC       66      3      1
#>     ACD      38     14      1
#>     AD       16      0      1
#>     B        37      3      1
#>     BC      113     91     36
#>     BCD     143    404    646
#>     BD       13      6      2
#>     C        99     37      4
#>     CD      145    194     72
#>     D        26      3      0
#> 
#> $X48
#>         score.level
#> response Group1 Group2 Group3
#>     A       104     44      5
#>     AB       78     92     43
#>     ABC     116    377    631
#>     ABCD     19     35     14
#>     ABD      17     21      9
#>     AC      148    100     32
#>     ACD      49     18      2
#>     AD       54     11      2
#>     B        18     10      3
#>     BC       34     40     20
#>     BCD      13      3      0
#>     BD        8      9      0
#>     C        69     19      4
#>     CD       43      7      4
#>     D        53      2      1
#> 
#> $X63
#>         score.level
#> response Group1 Group2 Group3
#>     A        32     17      9
#>     AB        1      1      2
#>     ABC       2      3      1
#>     ABCD      0      0      1
#>     ABD       9      6      8
#>     AC       41     25      7
#>     ACD       1      1      0
#>     AD      166     93     31
#>     B        82     32     13
#>     BC       69     52     31
#>     BCD       3      2      3
#>     BD      356    508    634
#>     C        12      3      2
#>     CD        2      1      0
#>     D        51     45     29
#> 
#> $X64
#>         score.level
#> response Group1 Group2 Group3
#>     A        59     49     17
#>     AB       33     20      1
#>     ABC       2      2      0
#>     ABCD      2      2      3
#>     ABD      10     16      5
#>     AC       27     22     11
#>     ACD       9     18     14
#>     AD       24     35     18
#>     B       127     60     16
#>     BC       15      4      3
#>     BCD      10     11      5
#>     BD      149    143     71
#>     C        99     67     65
#>     CD      176    257    488
#>     D        81     81     54
#> 
#> $X86
#>         score.level
#> response Group1 Group2 Group3
#>       A     274    315    421
#>       AC      2      1      0
#>       AD      0      1      0
#>       B     192    165    127
#>       C     188    155     98
#>       CD      0      1      0
#>       D     159    133    111
#> 
#> $X104
#>         score.level
#> response Group1 Group2 Group3
#>       A      92     64     23
#>       AB      1      0      0
#>       AC      1      1      0
#>       AD      1      0      0
#>       B     244    153     62
#>       BC      2      0      0
#>       C     272    433    632
#>       CD      3      0      0
#>       D     194    122     48
#> 
#> $X123
#>         score.level
#> response Group1 Group2 Group3
#>      A      132     45     13
#>      AB       2      1      0
#>      ABD      4      0      0
#>      AC       5      1      0
#>      AD       1      0      0
#>      B      240    441    626
#>      BC       9      7      2
#>      BCD      1      0      0
#>      BD       1      2      0
#>      C      223    122     39
#>      CD       1      2      0
#>      D      199    166     91
#> 
#> $X133
#>         score.level
#> response Group1 Group2 Group3
#>     A        83     21      8
#>     AB       55     21      4
#>     ABC      15     11      1
#>     ABCD     22     37     19
#>     ABD      36     50     27
#>     AC       49     12      6
#>     ACD      46     79     59
#>     AD      141    263    468
#>     B        63     18      3
#>     BC       24     11      4
#>     BCD      18     21     14
#>     BD       91     70     20
#>     C        32     10      2
#>     CD       53     75     43
#>     D        96     88     93
#> 
#> $X151
#>         score.level
#> response Group1 Group2 Group3
#>       A     153    194    201
#>       AB      1      0      0
#>       AC      3      0      0
#>       AD      1      0      0
#>       B     171    270    415
#>       BC      2      0      0
#>       BD      2      1      0
#>       C     211    137     78
#>       CD      1      0      0
#>       D     270    180     74
#> 
#> $X164
#>         score.level
#> response Group1 Group2 Group3
#>     A       346    558    697
#>     AB        0      1      0
#>     ABC       1      0      0
#>     ABCD      5      0      0
#>     AC        2      2      0
#>     AD        5      2      1
#>     B       109     17      2
#>     BC        1      1      1
#>     C       158     57     14
#>     CD        2      1      0
#>     D       199    150     55
#> 
#> $X177
#>         score.level
#> response Group1 Group2 Group3
#>     A       129     47     13
#>     AB      492    604    702
#>     ABC       5      4      0
#>     ABCD      8      3      0
#>     ABD      14      2      0
#>     AC        2      0      0
#>     ACD       1      1      1
#>     AD        5      2      1
#>     B       116    108     44
#>     BC        4      0      0
#>     BCD       4      1      0
#>     BD        3      2      0
#>     C        10      2      0
#>     CD       24     11     10
#>     D         8      3      0
#> 
#> $X181
#>         score.level
#> response Group1 Group2 Group3
#>      A       25      6      3
#>      AB      26     17      3
#>      ABC    128    267    456
#>      ABD     68     58     42
#>      AC     145    127     47
#>      ACD      5      0      0
#>      AD     105     59     13
#>      B       24      8      3
#>      BC     123    166    170
#>      BCD      0      2      2
#>      BD      78     38     19
#>      C       48     24      8
#>      CD       1      1      0
#>      D       51     16      4
#> 
#> $X182
#>         score.level
#> response Group1 Group2 Group3
#>     A       104     63     30
#>     AB       13      8      1
#>     ABC       7      2      0
#>     ABCD      1      1      0
#>     ABD       1      1      0
#>     AC      202     76     14
#>     ACD       3      0      0
#>     AD        2      0      1
#>     B        18      5      2
#>     BC        5      1      0
#>     BCD       9      5      2
#>     BD       37     31     21
#>     C        90     37     13
#>     CD       91    100     58
#>     D       242    459    627
#> 
#> $X191
#>         score.level
#> response Group1 Group2 Group3
#>     A        87     63     25
#>     AB      108     66     19
#>     ABC      22     14      6
#>     ABCD     24     11     11
#>     ABD      20     11      7
#>     AC      145    324    552
#>     ACD      24     32     42
#>     AD       29     11     10
#>     B        33     17      5
#>     BC       20     25      8
#>     BCD       5      4      1
#>     BD       12     12      2
#>     C       162    130     59
#>     CD       82     47     16
#>     D        49     17      6
#> 
#> $X193
#>         score.level
#> response Group1 Group2 Group3
#>     A        99    134    108
#>     AB      121    101     61
#>     ABC      23     35     35
#>     ABCD     11     14     14
#>     ABD       8      2      0
#>     AC       78    204    459
#>     ACD      12     21      9
#>     AD       50     31      6
#>     B        55     62     11
#>     BC       72     42     21
#>     BCD      13      3      2
#>     BD       25      8      3
#>     C        87     73     35
#>     CD      102     43      5
#>     D        66     11      2
#> 
#> $X205
#>         score.level
#> response Group1 Group2 Group3
#>      A      226    158    116
#>      AB     118    211    121
#>      ABC      4      1      0
#>      ABD      1      0      0
#>      AC       7      3      0
#>      AD       3      0      0
#>      B      291    365    522
#>      BC      15      4      1
#>      BCD      1      0      0
#>      BD       7      3      1
#>      C      117     34      6
#>      CD       3      2      3
#>      D       32      9      0
#> 
#> $X209
#>         score.level
#> response Group1 Group2 Group3
#>     A        69     44      6
#>     AB       21     22     10
#>     ABC      19     21      4
#>     ABCD     21     30     16
#>     ABD      18     46     21
#>     AC       57    128    145
#>     ACD      52    214    500
#>     AD       29     17      4
#>     B        65     32      6
#>     BC      101     23      1
#>     BCD      87     65     12
#>     BD       45     20      4
#>     C        99     69     18
#>     CD       64     39     22
#>     D        79     17      2
#> 
#> $X217
#>         score.level
#> response Group1 Group2 Group3
#>     A        69     26      6
#>     AB       27      6      0
#>     ABC       7      5      1
#>     ABCD      7      0      0
#>     ABD      12      4      0
#>     AC       64     49     13
#>     ACD      11      9      1
#>     AD       55      9      1
#>     B       110     67     24
#>     BC       72     67     43
#>     BCD      10     13      7
#>     BD      122     28      5
#>     C       132    382    647
#>     CD       53     65     10
#>     D        76     59     13
#> 
#> $X218
#>         score.level
#> response Group1 Group2 Group3
#>     A        70     20      1
#>     AB       65     23      3
#>     ABC     282    618    751
#>     ABCD      6      9      1
#>     ABD       9      2      1
#>     AC       70     26      4
#>     ACD       4      2      1
#>     AD        9      2      0
#>     B        51     10      0
#>     BC      100     50      7
#>     BCD       5      3      0
#>     BD        5      2      0
#>     C        58     10      0
#>     CD       20      3      0
#>     D        67      8      2
#> 
#> $X248
#>         score.level
#> response Group1 Group2 Group3
#>     A       152     92     47
#>     AB       89     29      7
#>     ABC      13      6      4
#>     ABCD     13      5      1
#>     ABD      18      6      5
#>     AC       81     76     52
#>     ACD      11     29     17
#>     AD       43     72     37
#>     B        66     57     32
#>     BC       17     22     17
#>     BCD       5      3      3
#>     BD       16     17     10
#>     C       136    179    127
#>     CD       64    136    380
#>     D       101     58     31
#> 
#> $X272
#>         score.level
#> response Group1 Group2 Group3
#>     A        75     14      2
#>     AB       60     46     14
#>     ABC      48     57     27
#>     ABCD     43    167    147
#>     ABD      56    218    506
#>     AC       47     34      3
#>     ACD      35     50     32
#>     AD       67     68     16
#>     B        43     11      1
#>     BC       60     12      0
#>     BCD      37     43     11
#>     BD       31     41     10
#>     C        98      8      0
#>     CD       55     16      1
#>     D        70      4      1
#> 
#> $X293
#>         score.level
#> response Group1 Group2 Group3
#>      A      342    503    657
#>      AB       3      1      0
#>      AC       1      1      0
#>      ACD      1      0      0
#>      AD       0      1      1
#>      B      319    188     89
#>      BD       1      0      0
#>      C       71     25      7
#>      CD       1      0      0
#>      D       80     70     15
#> 
#> $X304
#>         score.level
#> response Group1 Group2 Group3
#>      A      171     76     26
#>      AB      69     21      1
#>      ABC     34     42     12
#>      AC      47     59     31
#>      ACD      1      0      0
#>      AD       1      0      0
#>      B       88     26      8
#>      BC      17     11      5
#>      BD       2      0      0
#>      C      120    339    607
#>      CD       1      0      0
#>      D      271    215     80
#> 
#> $X316
#>         score.level
#> response Group1 Group2 Group3
#>     A       174     52      9
#>     AB        8      7      1
#>     ABC       1      1      0
#>     ABCD      0      0      1
#>     ABD       3      2      3
#>     AC       17      4      0
#>     ACD       1      4      0
#>     AD       17     13      6
#>     B       156     85     43
#>     BC       21     21     14
#>     BCD       7     24     20
#>     BD       57    153    407
#>     C       176    144     39
#>     CD        3      1      6
#>     D       185    279    222
#> 
#> $X331
#>         score.level
#> response Group1 Group2 Group3
#>     A       255     77     19
#>     AB        1      1      0
#>     ABC       2      0      0
#>     ABCD     11      3      0
#>     AC        5      0      0
#>     ACD       2      0      0
#>     AD        3      0      1
#>     B        89     70     25
#>     BC        0      1      0
#>     BD        2      1      0
#>     C       165     76     18
#>     CD        3      1      0
#>     D       288    558    708
#> 
#> $X365
#>         score.level
#> response Group1 Group2 Group3
#>     A        74     26      1
#>     AB       34      5      3
#>     ABC       9      3      1
#>     ABCD      1      9      1
#>     ABD       7      5      3
#>     AC        3      1      1
#>     ACD       7      9      2
#>     AD       18      5      1
#>     B       125     28      7
#>     BC       57     68     36
#>     BCD      74    405    636
#>     BD       83     54     23
#>     C       129     50      9
#>     CD      135    104     46
#>     D        67     17      1
#> 
#> $X394
#>         score.level
#> response Group1 Group2 Group3
#>      A      265    198     49
#>      AB      17      6      3
#>      ABC      5      0      0
#>      AC      11      4      0
#>      AD       5      0      0
#>      B      145    386    665
#>      BC      10     17     12
#>      BD       3      1      0
#>      C      176    106     28
#>      CD       5      1      0
#>      D      175     65     13
#> 
#> $X395
#>         score.level
#> response Group1 Group2 Group3
#>      A       36      2      0
#>      AB      14      3      1
#>      ABC      0      0      1
#>      ABD     15     28     33
#>      AC       4      0      0
#>      AD      15      8      2
#>      B      240     34      5
#>      BC      28      6      0
#>      BCD      1      2      0
#>      BD     380    695    727
#>      C       13      0      0
#>      CD       1      1      1
#>      D       81     11      1
#> 
#> $X410
#>         score.level
#> response Group1 Group2 Group3
#>      A      330    524    674
#>      AB      46     30     16
#>      ABC      9      0      0
#>      AC      42     16      4
#>      ACD      1      0      0
#>      AD       1      2      0
#>      B      181    100     38
#>      BC      14     10      2
#>      BCD      2      0      0
#>      BD       0      1      0
#>      C       72     25      8
#>      CD       1      0      0
#>      D      121     79     25
#> 
#> $X411
#>         score.level
#> response Group1 Group2 Group3
#>     A       226    326    264
#>     AB       58     52     28
#>     ABC      12     15      5
#>     ABCD     23     15      2
#>     ABD      10      8      1
#>     AC       48    146    356
#>     ACD       5     17      7
#>     AD       22     12     13
#>     B        43     11     17
#>     BC       12      2      2
#>     BD        2      0      0
#>     C       178     88     47
#>     CD       47     33     14
#>     D       134     56     15
#> 
#> $X432
#>         score.level
#> response Group1 Group2 Group3
#>      A       44     10      3
#>      AB       3      1      0
#>      ABC      0      0      1
#>      ABD      1      1      0
#>      AC       5      1      0
#>      ACD      1      4      0
#>      AD      15      7      0
#>      B       45     15      3
#>      BC      12      6      2
#>      BCD      2      0      0
#>      C      176     82     28
#>      CD     396    599    706
#>      D      127     63     28
#> 
#> $X435
#>         score.level
#> response Group1 Group2 Group3
#>       A     216     87     31
#>       AC     14      9      3
#>       B       5      1      0
#>       BC      1      0      0
#>       C     584    687    736
#>       CD      1      1      0
#>       D       8      5      1
#> 
#> $X451
#>         score.level
#> response Group1 Group2 Group3
#>     A        68     14      3
#>     AB       81     22      7
#>     ABC      47     36     23
#>     ABCD     90    128     93
#>     ABD      49     27     13
#>     AC       28     13      6
#>     ACD      25     29     36
#>     AD       46     19      6
#>     B        87     24     11
#>     BC       99     70     37
#>     BCD      87    261    422
#>     BD       38     39     32
#>     C        31     28      8
#>     CD       44     73     70
#>     D         8      6      4
#> 
#> $X461
#>         score.level
#> response Group1 Group2 Group3
#>     A        39      8      6
#>     AB       53     43     34
#>     ABC       0      0      2
#>     ABCD      3      0      0
#>     ABD      56     69     39
#>     AC       76    154    460
#>     ACD      14     13     12
#>     AD       60     54      9
#>     B        56     42     26
#>     BC        4      2      1
#>     BCD       4      3      0
#>     BD      133    164     78
#>     C        66     64     44
#>     CD       44     35     18
#>     D       206    127     37
#> 
#> $X462
#>         score.level
#> response Group1 Group2 Group3
#>     A       188    205     76
#>     AB      169    371    633
#>     ABC      44     38     15
#>     ABCD     20      2      0
#>     ABD      36     26     14
#>     AC      123     66     17
#>     ACD      19      8      0
#>     AD       83     50     11
#>     B        18      5      0
#>     BC       35      6      2
#>     BCD       6      2      0
#>     BD        9      0      0
#>     C        37      5      2
#>     CD       20      3      0
#>     D        16      0      1
#> 
#> $X468
#>         score.level
#> response Group1 Group2 Group3
#>     A        71     37      3
#>     AB       72     39     18
#>     ABC      14     12      2
#>     ABCD      7      2      1
#>     ABD      23     27     23
#>     AC       46     31     28
#>     ACD       8      7      1
#>     AD       88     42     12
#>     B       106    147     95
#>     BC       47     22      4
#>     BCD       6      7      2
#>     BD      117    283    515
#>     C        36     13      2
#>     CD       41     13      4
#>     D       143    107     61
#> 
#> $X474
#>         score.level
#> response Group1 Group2 Group3
#>     A        65     29     13
#>     AB       41     25      7
#>     ABC      37     25     12
#>     ABCD     20     11      3
#>     ABD       2      1      0
#>     AC       84     54     17
#>     ACD       9      0      2
#>     AD        5      3      0
#>     B        76    115     70
#>     BC      187    372    580
#>     BCD      16     19     11
#>     BD       10     14      3
#>     C       195     89     41
#>     CD       33     20     10
#>     D        43     12      2
#> 
#> $X479
#>         score.level
#> response Group1 Group2 Group3
#>               1      1      0
#>     A       197     78     37
#>     AB       50     39     27
#>     ABCD      0      2      1
#>     ABD      45    113     97
#>     AC        4      2      0
#>     ACD       3      3      2
#>     AD      436    522    599
#>     B        20      5      2
#>     BC       11      6      1
#>     BCD       1      0      0
#>     BD       20      7      2
#>     C         8      1      0
#>     CD        5      4      0
#>     D        30      7      3
#> 
#> $X483
#>         score.level
#> response Group1 Group2 Group3
#>     A        20      2      1
#>     AB       27     20      7
#>     ABC       4      2      0
#>     ABCD      2      0      0
#>     ABD       5      6      1
#>     AC       12      5      2
#>     AD        2      3      1
#>     B       379    542    693
#>     BC      106     52     19
#>     BCD      29      6      4
#>     BD      102     90     19
#>     C        71     24      9
#>     CD       18     10      1
#>     D        48     26     12
#> 
#> $X519
#>         score.level
#> response Group1 Group2 Group3
#>     A        72     11      1
#>     AB        2      0      0
#>     ABCD      1      0      0
#>     ABD       1      0      0
#>     AC        1      0      0
#>     B       116     23      7
#>     BC        2      0      0
#>     BD        1      0      0
#>     C       484    703    748
#>     CD        2      0      1
#>     D       146     52     14
#> 
#> $X521
#>         score.level
#> response Group1 Group2 Group3
#>      A      400    627    717
#>      AB       5      2      0
#>      ABC      1      0      0
#>      AC       1      0      0
#>      AD       9      1      0
#>      B      160    106     45
#>      BC       6      0      0
#>      BCD      0      1      0
#>      C      151     41      7
#>      CD       2      0      0
#>      D       90      9      2
#> 
#> $X552
#>         score.level
#> response Group1 Group2 Group3
#>      A       71     35     12
#>      AB       4      0      0
#>      ABD      1      0      0
#>      AC       4      0      1
#>      ACD      1      0      0
#>      AD      23      6      3
#>      B      125     44     11
#>      BC       2      1      0
#>      BCD      1      0      0
#>      BD      14      9      2
#>      C      373    632    730
#>      CD       1      1      0
#>      D      201     61     11
#> 
#> $X567
#>         score.level
#> response Group1 Group2 Group3
#>      A      180    100     61
#>      AB      11      1      0
#>      ABC      4      1      0
#>      B      377    568    665
#>      BC       4      2      1
#>      BD       1      0      0
#>      C       28     19      8
#>      CD       1      0      0
#>      D      220     92     36
#> 
#> $X592
#>         score.level
#> response Group1 Group2 Group3
#>      A       77     98     87
#>      ABC      1      0      0
#>      ABD      1      0      0
#>      AD       1      0      0
#>      B      500    594    639
#>      BC       1      1      0
#>      C       78     20     16
#>      D      158     71     28
#> 
#> $X611
#>         score.level
#> response Group1 Group2 Group3
#>     A        30     22      1
#>     AB       15      5      0
#>     ABC       9      3      0
#>     ABCD     24      1      0
#>     ABD      29      8      7
#>     AC       16      3      1
#>     ACD      13      8      3
#>     AD      459    627    719
#>     B         4      2      0
#>     BC       98     47     28
#>     BCD      21     13      6
#>     BD       35     14      0
#>     C         6      3      1
#>     CD       31      7      1
#>     D        37     24      4
#> 

# distractor analysis for item 1
DistractorAnalysis(Data, key, item = 1)
#> $X2001
#>         score.level
#> response Group1 Group2 Group3
#>     A       197    250    314
#>     AB       38     38     35
#>     ABC       4      4      4
#>     ABCD     10      9      6
#>     ABD      12     11      3
#>     AC       11      8      2
#>     ACD       4     10     14
#>     AD       10     17     11
#>     B       279    259    230
#>     BC       17      5      1
#>     BCD       8      4      5
#>     BD       22      7      4
#>     C        93     39     16
#>     CD        5      4      2
#>     D       117    120    123
#> 
if (FALSE) { # \dontrun{
# distractor analysis with proportions
DistractorAnalysis(Data, key, p.table = TRUE)

# distractor analysis for 6 groups
DistractorAnalysis(Data, key, num.group = 6)

# distractor analysis using specified criterion
criterion <- round(rowSums(Databin), -1)
DistractorAnalysis(Data, key, criterion = criterion)

# distractor analysis using discrete criterion
DistractorAnalysis(Data, key, criterion = criterion, crit.discrete = TRUE)

# distractor analysis using groups specified by cut.points
DistractorAnalysis(Data, key, cut.points = seq(10, 96, 10))
} # }
```
