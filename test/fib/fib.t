
Run the fib executable to generate some logs
  $ ./fib.exe 5 &> fib.logf

Check that the logs are parseable
  $ logf -- fib.logf
  [1]<>:
  [2]<5>:
    [3]<3>:
      [4]<1>:
      [5]<2>:
        [6]<0>:
        [7]<1>:
    [8]<4>:
      [9]<2>:
        [10]<0>:
        [11]<1>:
      [12]<3>:
        [13]<1>:
        [14]<2>:
          [15]<0>:
          [16]<1>:
  [17]<>:

Try out some queries on nodes when we know the id
  $ logf query fib.logf 1
  <1> :
    n: 5

  $ logf query fib.logf 8
  <8> 5:4:
    n: 4

  $ logf query fib.logf 12
  <12> 5:4:3:
    n: 3

Try and find some paths based on some regexps
  $ logf find fib.logf "**:3:**"
  No paths match the given regular expression
