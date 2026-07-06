(Compile Interpreter) Reset: `Get-ChildItem -Recurse -Filter *.class | Remove-Item`
```
javac -cp wg/java/ wg/java/nz/mwh/wg/Start.java
```

(Run Grace script)
```
java -cp wg/java/ nz.mwh.wg.Start wg/TypeChecker.grace
```

(Print longform AST)
```
java -cp wg/java/ nz.mwh.wg.Start -p wg/TypeChecker.grace
```

(Print concise AST, put script in test.grace in wg folder using `getFileContents "wg/test.grace"` in wg.grace)
```
java -cp wg/java/ nz.mwh.wg.Start wg/wg.grace
```

TypeChecker.grace is the main file, it imports collections.grace.  
The script sample.grace is for testing if all AST nodes are implemented but more extensive tests are needed.  