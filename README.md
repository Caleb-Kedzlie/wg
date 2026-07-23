Compile Parser in `wg` directory  
```
javac -cp java/ java/nz/mwh/wg/Start.java
```

Run Grace script (make sure to use 'LF' line endings)  
```
java -cp java/ nz.mwh.wg.Start TypeChecker.grace
```

Print longform AST  
```
java -cp java/ nz.mwh.wg.Start -p TypeChecker.grace
```

Print concise AST (first move the script into test.grace in outer directory)  
```
java -cp java/ nz.mwh.wg.Start wg.grace
```

TypeChecker.grace is the main file, it imports collections.grace.  
The script sample.grace is for testing if all AST nodes are implemented but more extensive tests are used.  
TEMPLATE and TESTING are not run.