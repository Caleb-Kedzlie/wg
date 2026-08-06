import "ast" as ast

// This file makes use of many AST nodes

def x = object {
    var y : Number := 1
    method foo(arg : Action) bar(n) -> String {
        self.y := arg.apply + n
        return "y @ {y}!"
    }
}

print(x.foo { 2 } bar 3)