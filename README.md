## Main commands  
Compile Parser. Stay in `wg` directory for all commands, and use 'LF' line endings for all files.
```
cd wg
javac -cp java/ java/nz/mwh/wg/Start.java
```

Run Grace TypeChecker script 
```
java -cp java/ nz.mwh.wg.Start TypeChecker.grace
```


Print longform AST (use 'LF' line endings, and replace test.grace with desired script)  
```
java -cp java/ nz.mwh.wg.Start -p test.grace
```

Print concise AST (first move the script contents into test.grace in outer `wg` directory)  
```
java -cp java/ nz.mwh.wg.Start wg.grace
```

## Info  
TypeChecker.grace is the main file, it imports collections.grace. The TEMPLATE is unused.  
The script sample.grace is for testing if all AST nodes are implemented but more extensive tests are used.  
There is some random testing in test.grace that could be deleted.  

## Typechecking any Grace program  
With the current commands, any program written in the file test.grace can be converted into concise and longform AST.  
The Typechecker uses concise AST, so convert it into that form and then copy and paste it into the end of TypeChecker.grace.  
For example with a script consisting of just the constant "3" can either be standalone:  
`o0C(o1N(n0M(3)),nil).checkType(Environment(BaseEnvironment), unknownType)`  
Or as a test that prints the result and catches errors:  
`assertPasses(o0C(o1N(n0M(3)),nil))`  