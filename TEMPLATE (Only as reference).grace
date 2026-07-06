class cons(h, t) {
    def kind is public = "cons"
    def head = h
    def tail = t
    def size = t.size + 1

    method at(index : Number) {
        if (index == 0) then {
            return head
        }
        return tail.at(index - 1)
    }

    method each(b) {
        var c := self
        while { c.kind == "cons" } do {
            b.apply(c.head)
            c := c.tail
        }
    }

    method map(b) {
        cons(b.apply(h), tail.map(b))
    }

    method flatMap(b) {
        def items = b.apply(head)
        
        def rest = tail.flatMap(b)
        rest.prefix(items)
    }

    method join(s) {
        if (size == 1) then {
            return head.asString
        }
        return head.asString ++ s ++ tail.join(s)
    }

    method prefix(other) {
        if (other.kind == "nil") then {
            return self
        }
        if (size == 1) then {
            cons(other.head, self)
        } else {
            cons(other.head, self.prefix(other.tail))
        }
    }

    method without(pred) {
        if (pred.apply(head)) then {
            tail.without(pred)
        } else {
            cons(head, tail.without(pred))
        }
    }
}

def nil = object {
    def end is public = true
    def kind is public = "nil"
    def size = 0

    method at(index) {
        // error
        nilHasNoItems
    }

    method each(b) {

    }

    method each_inner(b) {

    }
    
    method map(b) {
        self
    }

    method flatMap(b) {
        self
    }

    method without(pred) {
        self
    }

    method prefix(other) {
        other
    }

    method join(s) { "" }
}

method o1N(v) { cons(v, nil) }
method c2N(a, b) { cons(a, cons(b, nil)) }
method c0N(h, t) { cons(h, t) }


def TypeError = Exception.refine "Type Error"

class aType {
    def kind is public = "basic"
    var body := nil
    var shortName := ""

    method accept(subtype) {
        if (subtype.kind == "unknown") then {
            return true
        }
        if (self == subtype) then {
            return true
        }
        var a := body
        a.each { x ->
            if (!subtype.hasMethod(x.name)) then {
                return false
            }
            def meth = subtype.getMethod(x.name)
            var superParam := x.parameters
            var subArg := meth.parameters
            while { superParam.kind != "nil" } do {
                if (!subArg.head.accept(superParam.head)) then {
                    return false
                }
                superParam := superParam.tail
                subArg := subArg.tail
            }
            // print "checking method {x.name}. super returns {x.returnType}, sub returns {meth.returnType}. {x.returnType.accept(meth.returnType)}"
            if (!x.returnType.accept(meth.returnType)) then {
                print "failing check at {x.name}!"
                return false
            }
        }
         
        return true
    }

    method hasMethod(name) {
        body.each { n ->
            if (n.name == name) then {
                return true
            }
        }
        return false
    }

    method getMethod(name) {
        body.each { n ->
            if (n.name == name) then {
                return n
            }
        }
    }

    method returnOf(name) { // Could just do getMethod(name).returnType
        body.each { n ->
            if (n.name == name) then {
                return n.returnType
            }
        }
        return false
    }

    method add(sig) {
        body := cons(sig, body)
    }

    method assert(subtype, at) { // Could send at.kind directly.
        if (!accept(subtype)) then {
            TypeError.raise "{subtype} is not a valid subtype of type {self}, at {at.kind}"
        }
    }

    method longString { // Very useful for debugging to have a verbose name.
        var ret := "interface \{"
        ret := ret ++ body.map { x -> 
                if (x.name == "self(0)") then {
                    "self(0) -> Self"
                } else {
                    x.asString
                }
            }.join("; ")
        ret := ret ++ "}"
        ret
    }

    method asString {
        if (shortName == "") then {
            return longString
        }
        shortName
    }
}

def unknownType = object {
    def kind is public = "unknown"
    def shortName is public = "Unknown"

    method accept(subtype) {
        true
    }

    method assert(subtype, at) {

    }

    method asString { "Unknown" }
    method longString { asString }

    method returnOf(name) {
        return self
    }
}

// Needs to compare expected type to actual or throw an error. TODO find a way to do this without needing a class.
class aMethod(nm, params, rType) {
    def name is public = nm
    def parameters is public = params
    def returnType is public = rType

    method asString {
        "{name}({params.map {x -> x.asString}.join(", ")}) -> {returnType}"
    }

    method acceptsArguments(args) {
        var p := parameters
        var a := args
        while { p.kind != "nil" } do {
            if (!p.head.accept(a.head)) then {
                return false
            }
            p := p.tail
        }
        return true
    }
}

// Define object versions of primitives. Saves space. Start with one of them comparing the name then make more.
def builtinNumber = aType
builtinNumber.shortName := "Number"
def builtinString = aType
builtinString.shortName := "String"
def builtinBoolean = aType
builtinBoolean.shortName := "Boolean"
def builtinDone = aType
builtinDone.shortName := "Done"

// When making my own you could make all AType have ==, != and asString by default. Reduces code duplication.
builtinNumber.add(aMethod("+(1)", o1N(builtinNumber), builtinNumber))
builtinNumber.add(aMethod("*(1)", o1N(builtinNumber), builtinNumber))
builtinNumber.add(aMethod("asString(0)", nil, builtinString))
builtinNumber.add(aMethod("==(1)", o1N(unknownType), builtinBoolean))
builtinNumber.add(aMethod("!=(1)", o1N(unknownType), builtinBoolean))

builtinString.add(aMethod("asString(0)", nil, builtinString))
builtinString.add(aMethod("size(0)", nil, builtinNumber))
builtinString.add(aMethod("++(1)", o1N(builtinString), builtinString))
builtinString.add(aMethod("==(1)", o1N(unknownType), builtinBoolean))
builtinString.add(aMethod("!=(1)", o1N(unknownType), builtinBoolean))

builtinBoolean.add(aMethod("asString(0)", nil, builtinString))
builtinBoolean.add(aMethod("==(1)", o1N(unknownType), builtinBoolean))
builtinBoolean.add(aMethod("!=(1)", o1N(unknownType), builtinBoolean))
builtinBoolean.add(aMethod("prefix!(0)", nil, builtinBoolean))

// Could use this down the line.
def absentType = object {
    def kind is public = "absent"
}


class NumberNode(v) {
    def kind is public = "number literal"
    def value is public = v
    
    method infer(env) {
        builtinNumber
    }

    method check(env, expected) {
        expected.assert(builtinNumber, self)
    }
}

method n0M(v) { NumberNode(v) }

class StringNode(v) {
    def kind is public = "string literal"
    def value is public = v

    method infer(env) {
        builtinString
    }

    method check(env, expected) {
        expected.assert(builtinString, self)
    }
}

method s0L(v) { StringNode(v) }

method s4F(pre, expr, post) {
    // (rec, nm, args, generics)
    DotRequestNode(
        DotRequestNode(pre, "++(1)", cons(DotRequestNode(expr, "asString(0)", nil, nil), nil), nil),
        "++(1)", cons(post, nil), nil)
}

class InterpolatedStringNode(pre, expr, post) {
    def kind is public = "string interpolation"
    def prefix is public = pre
    def expression is public = expr
    def next is public = post

    method infer(env) {
        expression.infer(env)
        builtinString
    }

    method check(env, expected) {
        expected.assert(builtinString, self)
    }
}

method i0S(a, b, c) { InterpolatedStringNode(a, b, c) }

class DefNode(nm, dType, anns, value) {
    def kind is public = "def declaration"
    def initialisation is public = value
    def declaredType is public = if (dType.kind == "nil") then { absentType } else { dType.head }
    def annotations is public = anns

    method infer(env) {
        def initType = initialisation.infer(env)
        if (declaredType.kind == "absent") then {
            return builtinDone
        }
        def myType = env.resolveType(declaredType)
        if (!myType.accept(initType)) then {
            TypeError.raise "def declaration '{nm}' initialised with wrong type: must be {myType} but is instead {initType}"
        }
        builtinDone
    }

    method check(env, expected) {
        expected.assert(builtinDone, self)
    }

    method addToEnvironment(env) {
        var myType
        if (declaredType.kind == "absent") then {
            myType := initialisation.infer(env)
        } else {
            myType := declaredType
        }
        def meth = aMethod(nm ++ "(0)", nil, myType)
        env.add(meth)
    }
}

method d3F(n, d, a, v) { DefNode(n, d, a, v) }


class VarNode(nm, dType, anns, value) {
    def kind is public = "var declaration"
    def initialisation is public = value
    def declaredType is public = if (dType.kind == "nil") then { unknownType } else { dType.head }
    def annotations is public = anns

    method infer(env) {
        var myType
        if (declaredType.kind == "absent") then {
            myType := unknownType
        } else {
            myType := env.resolveType(declaredType)
        }
        if (initialisation.kind != "nil") then {
            def initType = initialisation.head.infer(env)
            if (!myType.accept(initType)) then {
                TypeError.raise "var declaration '{nm}' initialised with wrong type: must be {myType} but is instead {initType}"
            }
        }
        builtinDone
    }

    method check(env, expected) {
        expected.assert(builtinDone, self) // This should use infer(env) not just builtInDone.
    }

    method addToEnvironment(env) {
        var myType
        if (declaredType.kind == "absent") then {
            myType := unknownType
        } else {
            myType := env.resolveType(declaredType)
        }
        def meth = aMethod(nm ++ "(0)", nil, myType)
        env.add(meth)
        def methAssign = aMethod(nm ++ ":=(1)", cons(myType, nil), builtinDone)
        env.add(methAssign)
    }
}

method v4R(n, d, a, v) { VarNode(n, d, a, v) }


class DotRequestNode(rec, nm, args, generics) {
    def kind is public = "dotted method request"
    def receiver is public = rec
    def name is public = nm
    def arguments is public = args
    def typeArguments is public = generics

    method infer(env) {
        def needed = aType
        def methSig = aMethod(name, args.map { x -> x.infer(env) }, unknownType)
        needed.add(methSig)
        receiver.check(env, needed)
        def recType = receiver.infer(env)
        recType.returnOf(name)
    }

    method check(env, expected) {
        expected.assert(infer(env), self)
    }

    method asString {
        "request of {name}"
    }
}

method d0R(rec, name, args, generics) { DotRequestNode(rec, name, args, generics) }


class LexicalRequestNode(nm, args, generics) {
    def kind is public = "lexical method request"
    def name is public = nm
    def arguments is public = args
    def typeArguments is public = generics

    method infer(env) {
        def meth = env.find(name)
        def argTypes = arguments.map { x -> x.infer(env) }
        if (!meth.acceptsArguments(argTypes)) then {
            TypeError.raise "invalid arguments in request of {name}, must be {meth.parameters.map { x -> x.asString }.join(", ")} and not {argTypes.map { x -> x.asString }.join(", ")}"
        }
        env.resolveType(meth.returnType)
    }

    method check(env, expected) {
        expected.assert(infer(env), self)
    }

    method asString {
        "request of {name}"
    }
}

method l0R(nm, args, generics) { LexicalRequestNode(nm, args, generics) }


method a5N(lhs, rhs) {
    if (lhs.kind == "lexical method request") then {
        // Local variable
        return LexicalRequestNode(lhs.name.substringFrom(1)to(lhs.name.size - 3) ++ ":=(1)",
            cons(rhs, nil))
    } elseif (lhs.kind == "dotted method request") then {
        def rec = lhs.receiver
        def name = lhs.name
        def newName = name.substringFrom(1)to(name.size - 3) ++ ":=(1)"
        DotRequestNode(receiver, newName, cons(rhs, nil))
    } else {
        TypeError.raise "invalid left-hand side of assignment"
    }
}


class ObjectNode(bd, anns) {
    def kind is public = "object constructor"
    def body is public = bd.without { x -> x.kind == "comment" }
    def annotations is public = anns

    method infer(env) {
        def newEnv = Environment(env)
        body.each { n ->
            if (n.kind == "var declaration") then {
                n.addToEnvironment(newEnv)
            } elseif {n.kind == "type declaration"} then {
                n.addToEnvironment(newEnv)
            } elseif { n.kind == "method declaration" } then {
                n.addToEnvironment(newEnv)
            }
        }
        def objectType = newEnv.asType // Investigate.
        newEnv.add(aMethod("self(0)", nil, objectType))
        body.each { n ->
            if (n.kind == "def declaration") then {
                n.addToEnvironment(newEnv)
                // This is a nasty hack:
                objectType.body := newEnv.items
            }
        }
        newEnv.replace(aMethod("self(0)", nil, newEnv.asType))
        body.each { n -> // Redundant, only for debugging?
            def it = n.infer(newEnv)
            // print "inferred type: {it.shortName} {it.longString}"
            n.check(newEnv, unknownType)
        }
        newEnv.asType
    }

    method check(env, expected) {
        def myType = infer(env)
        expected.accept(myType)
    }
}

method o0C(bd, anns) { ObjectNode(bd, anns) }


class MethodNode(pts, rType, anns, bd) {
    def kind is public = "method declaration"
    def name is public = pts.map { x -> "{x.name}({x.parameters.size})" }.join ""
    def parameters is public = pts.flatMap { x -> x.parameters }
    def returnType is public = rType
    def annotations is public = anns
    def body is public = bd.without { x -> x.kind == "comment" }

    method infer(env) { builtinDone }

    method check(env, _) {
        var retType
        if (returnType.kind == "nil") then {
            retType := unknownType
        } else {
            retType := env.resolveType(returnType.head)
        }
        def bodyEnv = Environment(env)
        bodyEnv.mustReturn(retType, name)
        parameters.each { prm ->
            if (prm.kind == "identifier declaration") then {
                if (prm.declaredType.kind == "nil") then {
                    env.add(aMethod(prm.name ++ "(0)", nil, unknownType))
                } else {
                    def pType = env.resolveType(prm.declaredType.head)
                    env.add(aMethod(prm.name ++ "(0)", nil, pType))
                }
            } else {
                env.add(aMethod(prm.name, nil, unknownType))
            }
        }
        addDeclarations(bodyEnv, body)
        var lastExpression := false
        body.each { n ->
            def nodeType = n.infer(bodyEnv)
            n.check(bodyEnv, unknownType)
            lastExpression := nodeType
        }
        if (lastExpression != false) then {
            if (!retType.accept(lastExpression)) then {
                TypeError.raise "method '{name}' must return {retType}, but last expression of body has type {lastExpression}"
            }
        }
    }

    method addToEnvironment(env) {
        env.add(asMethod(env))
    }

    method asMethod(env) {
        var retType
        if (returnType.kind == "nil") then {
            retType := unknownType
        } else {
            retType := env.resolveType(returnType.head)
        }
        aMethod(name, parameters.map { prm ->
            if (prm.kind == "identifier declaration") then {
                if (prm.declaredType.kind == "nil") then {
                    unknownType
                } else {
                    env.resolveType(prm.declaredType.head)
                }
            } else {
                unknownType
            }
        }, retType)
    }
}

method m0D(pts, retType, anns, bd) { MethodNode(pts, retType, anns, bd) }


class TypeDeclarationNode(nm, args, val) {
    def kind is public = "type declaration"
    def name is public = nm
    def typeParams is public = args
    def value is public = val

    method infer(env) {
        builtinDone
    }

    method check(env, at) {
        true
    }

    method addToEnvironment(env) {
        def theType = env.resolveType(val)
        if (theType.shortName == "") then {
            theType.shortName := name
        }
        env.addType(nm, theType)
    }
}

method t0D(n, g, v) { TypeDeclarationNode(n, g, v) }


class InterfaceNode(bd) {
    def kind is public = "interface"
    def body is public = bd

    method infer(env) {
        builtinDone
    }

    method check(env, at) {
        // Missing
    }

    method asType(env) {
        def ret = aType
        body.each { n ->
            ret.add(n.asMethod(env))
        }
        ret
    }
}

method i0C(bd) { InterfaceNode(bd) }


class MethodSignatureNode(pts, rType) {
    def kind is public = "method signature"
    def name is public = pts.map { x -> "{x.name}({x.parameters.size})" }.join ""
    def parameters is public = pts.flatMap { x -> x.parameters }
    def returnType = rType

    method asMethod(env) {
        var retType
        if (returnType.kind == "nil") then {
            retType := unknownType
        } else {
            retType := env.resolveType(returnType.head)
        }
        aMethod(name, parameters.map { prm ->
            if (prm.kind == "identifier declaration") then {
                if (prm.declaredType.kind == "nil") then {
                    unknownType
                } else {
                    env.resolveType(prm.declaredType.head)
                }
            } else {
                unknownType
            }
        }, retType)
    }
}

method m0S(pts, rType) { MethodSignatureNode(pts, rType) }


class IdentifierNode(id, dtype) {
    def kind is public = "identifier declaration"
    def name is public = id
    def declaredType is public = dtype
}

method i0D(i, t) { IdentifierNode(i, t) }

class PartNode(n, a, g) {
    def kind is public = "part"
    def name is public = n
    def parameters is public = a
    def typeParams is public = g
}

method p0T(n, a, g) { PartNode(n, a, g) }


class CommentNode(txt) {
    def kind is public = "comment"
    def text is public = txt

    method infer(_) { unknownType }
    method check(_, _) { true }
}

method c0M(text) { CommentNode(text) }


class ReturnNode(rv) {
    def kind is public = "return"
    def value is public = rv

    method infer(env) {
        value.infer(env) // I would remove this line since check is doing infer.
        unknownType
    }

    method check(env, expected) {
        def myType = value.infer(env) // Maybe this should be check to propagate the same function.
        if (!env.returnType.accept(myType)) then {
            TypeError.raise "invalid return from '{env.returnLabel}': must return {env.returnType}, but returned value has type {myType}"
        }
        true
    }
}

method r3T(rv) { ReturnNode(rv) }

class BlockNode(prms, bd) {
    def kind is public = "block"
    def parameters is public = prms
    def body is public = bd.without { x -> x.kind == "comment" }

    method infer(env) {
        def bodyEnv = Environment(env)
        def paramTypes = parameters.map { prm ->
            if (prm.kind == "identifier declaration") then {
                if (prm.declaredType.kind == "nil") then {
                    bodyEnv.add(aMethod(prm.name ++ "(0)", nil, unknownType))
                    unknownType
                } else {
                    def pType = env.resolveType(prm.declaredType.head)
                    bodyEnv.add(aMethod(prm.name ++ "(0)", nil, pType))
                    pType
                }
            } else {
                bodyEnv.add(prm.name, nil, unknownType)
                unknownType
            }
        }
        addDeclarations(bodyEnv, body)
        var lastType := builtinDone
        body.each { n ->
            lastType := n.infer(bodyEnv)
            n.check(bodyEnv, unknownType)
        }
        def ret = aType
        ret.add(aMethod("apply({parameters.size})", paramTypes, lastType))
        print "block type {ret}"
        ret
    }

    method check(env, expected) {
        def myType = infer(env)
        expected.assert(myType)
    }
}

method b1K(prms, bd) { BlockNode(prms, bd) }

def c9B = "\\"
def c9D = "$"
def c9S = "*"
def c9L = "\{"
def c9N = "\n"
def c9R = "\r"
def c9Q = "\""
def c9T = "~"
def c9C = "^"
def c9G = "`"
def c9A = "@"
def c9P = "%"
def c9H = "#"
def c9E = "!"


method addDeclarations(env, body) {
    body.each { n ->
        if (n.kind == "def declaration") then {
            n.addToEnvironment(env)
        } elseif {n.kind == "var declaration"} then {
            n.addToEnvironment(env)
        } elseif {n.kind == "type declaration"} then {
            n.addToEnvironment(env)
        } else {
            if ( n.kind == "method declaration" ) then {
                n.addToEnvironment(env)
            }
        }
    }
}

// Stores global and local variables. Mapping of variable names to values. What object is currently Self. 
class Environment(par) {
    def parent is public = par
    var items is public := nil // Should be called methods.
    var types is public := nil // For type declarations.
    var hasRequiredReturn := false
    var requiredReturnType := unknownType
    var returnScopeLabel := ""

    // Find method by name.
    method find(name) {
        items.each { x ->
            if (x.name == name) then {
                return x
            }
        }
        return parent.find(name)
    }

    method add(meth) {
        items := cons(meth, items)
    }

    method replace(meth) {
        items := items.map { m ->
            if (m.name == meth.name) then { meth } else { m }
        }
    }

    method addType(nm, val) {
        types := cons(object {
            def name is public = nm
            def value is public = val
            }, types)
    }

    method resolveType(expr) {
        if (expr.kind == "interface") then {
            return expr.asType(self)
        }
        if (expr.kind == "lexical method request") then {
            def name = expr.name.substringFrom(1)to(expr.name.size - 3)
            types.each { t ->
                if (t.name == name) then {
                    return t.value
                }
            }
        }
        parent.resolveType(expr)
    }

    method mustReturn(tp, scopeLabel) {
        hasRequiredReturn := true
        requiredReturnType := tp
        returnScopeLabel := scopeLabel
    }

    method returnType {
        if (hasRequiredReturn) then {
            requiredReturnType
        } else {
            parent.returnType
        }
    }

    method returnLabel {
        if (hasRequiredReturn) then {
            returnScopeLabel
        } else {
            parent.returnScopeLabel
        }
    }

    method asType {
        def ret = aType
        items.each { x ->
            ret.add(x)
        }
        ret
    }
}

class BaseEnvironment {
    method find(name) {
        // Add true/false here.
        // if("true(0)" == name) then {}

        TypeError.raise "no such name '{name}' in scope"
    }

    method returnType {
        TypeError.raise "invalid return statement at top level"
    }

    method resolveType(expr) {
        if (expr.kind == "basic") then {
            return expr
        }
        if (expr.kind == "unknown") then {
            return expr
        }
        if (expr.kind == "interface") then {
            return expr.asType
        }
        def name = expr.name
        if ("Number(0)" == name) then {
            return builtinNumber
        }
        if ("String(0)" == name) then {
            return builtinString
        }
        if ("Boolean(0)" == name) then {
            return builtinBoolean
        }
        
        if ("Unknown(0)" == name) then {
            return unknownType
        }
        TypeError.raise "unknown type {name}"
    }
}
