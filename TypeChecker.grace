// TODO delete info.
// COMPILE ONLY ONCE: javac -cp java/ java/nz/mwh/wg/Start.java
// java -cp java/ nz.mwh.wg.Start TypeChecker.grace
// Bidirectional because "inferType" deduces type bottom-up and "checkType" tests if an expression matches type top-down or throws error.

// Importing library collections.grace to use array lists instead of linked lists.
import "collections" as collections

// Defined custom exceptions. Usage: TypeError.raise "message"
def TypeError = Exception.refine "TypeError"
def FailedError = Exception.refine "FailedError"


//
// #### AST METHODS ####
//


// Other AST for special character constants.
def c9D = "$"
def c9B = "\\"
def c9Q = "\""
def c9N = "\n"
def c9R = "\r"
def c9L = "\{" // Right brace not needed, as it does not begin a string format interpolation.
def c9S = "*"
def c9T = "~"
def c9G = "`"
def c9C = "^"
def c9A = "@"
def c9P = "%"
def c9H = "#"
def c9E = "!"
// To check if special character is in SafeString.
def specialChars = collections.list [
    c9B, c9D, c9S, c9L, c9N, c9R, c9Q, 
    c9T, c9C, c9G, c9A, c9P, c9H, c9E]


// Short form AST to instantiate list nodes for the type checker.
method o1N(v) { collections.list [v] }
method c2N(a, b) { collections.list [a, b] }
method c0N(h, t) { // Like a linked list, head appends to the front of the tail list.
    t.add(h) at(1)
    return t
}

// Empty AST. Formerly used a class, but an empty list is sufficient.
method nil { return collections.list [] }
// Check if an object is nil (empty list).
method isNil(val) { 
    // If not list avoid running 'size' method.
    if (val.name != "list") then { return false }
    return val.size == 0
}

// Short form AST to instantiate the basic types.
method n0M(v) { LiteralNode("number value", v, numberType) }
method s0L(v) { LiteralNode("string value", v, stringType) }

// Interpolated string for formatting. e.g. "price is {y} dollars." -> prefix="price is ", expr=y, and suffix=" dollars."
method i0S(prefix, expr, suffix) { InterpolatedStringNode(prefix, expr, suffix) }

// SafeStr for formatting special characters with prefix/suffix. Intuition: prefix.++(expr).++(suffix)
method s4F(prefix, expr, suffix) {
    // Check if it is a special character, if so then we can use stringType directly.
    if (!specialChars.contains { c -> c == expr }) then { 
        TypeError.raise "'{expr}' is not a special character expression in the safe String" 
    }
    // Check that prefix ++ stringType ++ suffix are all valid types for those methods.
    return DotRequestNode(
        DotRequestNode(prefix, "++(1)", o1N(stringType), nil),
        "++(1)", 
        o1N(suffix), 
        nil)
}

// Block/lambda containing parameters and body to be executed with the apply method.
//method b1K(params, body) { BlockNode(params, body) }

// Declarations for def and var.
method d3F(name, dType, anns, value) { DefNode(name, dType, anns, value) }
method v4R(name, dType, anns, value) { VarNode(name, dType, anns, value) }

// Reassignment of a variable (defined by var). Uses lexical or dot request. No class needed.
//method a5N(lhs, rhs) {}

// Type and interface declaration.
//method t0D(name, genericParams, value) {}
//method i0C(body) {}

// Method signature (in interfaces).
//method m0S(parts, rType) { MethodSignatureNode(parts, rType) }
// Method declaration (includes annotations and body).
//method m0D(parts, rType, anns, body) { MethodNode(parts, rType, anns, body) }

// Implicit/lexical request of variable/methods.
method l0R(name, args, genericParams) { LexicalRequestNode(name, args, genericParams) }

// Explicit/dot request of variable/methods.
method d0R(receiver, name, args, genericParams) { DotRequestNode(receiver, name, args, genericParams) }

// Individual part of a method signature. e.g. foo(a) in: method foo(a) bar(b)
//method p0T(name, params, genericParams) { PartNode(name, params, genericParams) }

// The constructor of an object. Also used as the root node AST.
method o0C(body, anns) { ObjectNode(body, anns) }

// Return statement for a method.
method r3T(value) { ReturnNode(value) }

// Parameter type identifier for a method signature part. e.g. foo(a : String).
//method i0D(name, dType) { IdentifierNode(name, dType) }

// Comment. Gets excluded from the body of objects, methods and blocks.
method c0M(text) { CommentNode(text) }

// Import statement using source string. e.g. import "ast" as ast
//method i0M(source, binding) { ImportNode(source) }

// Dialect Statement that extends the Grace language using source string. e.g. dialect "name"
//method d0S(source) { DialectNode(source) }

// Lineup infers/executes each element between semicolons: 1+1; f(a); print 3
//method l0N(elems) { LineupNode(elems) }


//
// #### IMPLEMENTATION ####
//


// Create a method inside a type.
class NewMethod(nm, params, rType) {
    def name is public = nm
    def parameters is public = params
    def returnType is public = rType

    method asString {
        return name
    }

    // Check this methods parameters all subtype the sent args.
    method assignableArguments(args) {
        // Check the number of arguments matches the parameters.
        if (args.size != parameters.size) then {
            return false
        }
        parameters.zip(args) do { p, a ->
            // Checking if the param matches the argument type.
            if (!p.assignableFrom(a)) then {
                return false
            }
        }
        return true
    }

    // Reuses argument subtype check but throws error instead of returning false.
    method checkArguments(args) {
        if (!assignableArguments(args)) then {
            TypeError.raise "Method {name} ({parameters} -> {returnType}) arguments must be '{parameters.map { a -> a.name} }' not '{args.map { a -> a.name} }'"
        }
    }
}

// Helper to make method with one argument with same type as the return type.
method oneArgMeth(name, rType) {
    return NewMethod(name, o1N(rType), rType)
}
// Helper to make method with no arguments and specific return type.
method arglessMeth(name, rType) {
    return NewMethod(name, nil, rType)
}


// Node for all types to be built upon.
class AnyType(nm) {
    var name is public := nm
    var methods := nil

    method setupMethods(meths) {
        methods := meths
        // Methods that all basic types have (excluding done).
        methods.add(NewMethod("==(1)", o1N(unknownType), booleanType))
        methods.add(NewMethod("!=(1)", o1N(unknownType), booleanType))
        methods.add(NewMethod("asString(0)", nil, stringType))
        print "SETUP {name}: {methods.join(", ")}"
    }

    method addMethod(meth) {
        methods.add(meth)
    }

    // I want it to throw an error if a method doesn't exist usually. Only used for subtype checking.
    method hasMethod(name) {
        methods.contains { m -> m.name == name } 
    }

    method getMethod(nm) {
        methods.do { m -> 
            if (m.name == nm) then { return m } 
        }
        TypeError.raise "Method {nm} does not exist for {name}"
    }

    method asString {
        return name
    }
    

    // Compare another type with this type to check if matching/subtype.
    method assignableFrom(subtype) {
        // Unknown always subtypes and the same object subtypes.
        if ((subtype.name == "Unknown") || (self == subtype)) then {
            return true
        }

        methods.do { m ->
            // The subtype should at least have all methods of this type.
            if (!subtype.hasMethod(m.name)) then {
                return false
            }
            def subtypeMeth = subtype.getMethod(m.name)
            // Compare the method params with the subtype.
            if (!m.assignableArguments(subtypeMeth.parameters)) then {
                // print "{m.name} invalid arguments ({subtypeMeth.parameters.join(", ")})"
                return false
            }
            if (!m.returnType.assignableFrom(subtypeMeth.returnType)) then {
                // print "Invalid returnType {m.name}!"
                return false
            }
        }
        // Succeeded method check. Name doesn't have to match.
        return true
    }
}


// Singleton for unknown types that can assign to any variable or be used in a method like "==(1)" that compares to any type.
def unknownType = object {
    def name is public = "Unknown"

    method assignableFrom(subtype) { return true }
    method asString { return name }
    method addMethod(meth) { TypeError.raise "Unknown type cannot add methods"}
    method hasMethod(name) { return false }
    method getMethod(nm) { TypeError.raise "Unknown type has no methods" }
    method inferType(env) { return self }
    method checkType(env, expected) {}
}


// Basic literal types along with their unique methods ("==(1)", "!=(1)" and "asString(0)" created in setupMethods).
def numberType = AnyType("Number")
def stringType = AnyType("String")
def booleanType = AnyType("Boolean")
// Similar to void: def/var use this type when done.
def doneType = AnyType("Done")
numberType.setupMethods(c0N(oneArgMeth("+(1)", numberType), o1N(oneArgMeth("*(1)", numberType))))
stringType.setupMethods(c0N(oneArgMeth("++(1)", stringType), o1N(arglessMeth("size(0)", numberType))))
booleanType.setupMethods(o1N(arglessMeth("prefix!(0)", booleanType)))


// Node that stores a name and literal value and can compare types via structual subtyping.
class LiteralNode(nm, v, lit) {
    def name is public = nm
    def value is public = v
    def literal = lit
    
    method inferType(env) {
        return literal
    }

    method checkType(env, expected) { // Expected and Actual are AnyType literals.
        def actual = self.inferType(env)
        if (!expected.assignableFrom(actual)) then {
            TypeError.raise "'{actual.name} ({actual.methods.join(", ")})' is not a subtype of expected '{expected.name} ({expected.methods.join(", ")})'" 
        }
    }
}


// Def declarations are immutable. You can get but not assign them.
class DefNode(nm, decType, annotations, val) {
    def name is public = "def declaration"
    def declaredName is public = nm
    def declaredType is public = if (decType.size > 0) then { decType.first } else { unknownType }
    def value is public = if (isNil(val)) then { unknownType } elseif {val.name == "list"} then { val.first } else { val }

    method inferType(env) {
        // Def needs initial value, so if it is nil (uses unknownType), raise TypeError.
        if (value.name == "Unknown") then {
            TypeError.raise "Def needs initial value"
        }
        // Find and compare value with declared type.
        def expectedType = env.findType(declaredType)
        def valueType = value.inferType(env)
        if (!expectedType.assignableFrom(valueType)) then {
            TypeError.raise "Actual '{valueType.name}' is not a subtype of '{expectedType.name}'"
        }
        return doneType
    }

    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.assignableFrom(actual)) then {
            TypeError.raise "Def declaration invalid, expected '{expected.name}', actual '{actual.name}'"
        }
    }

    method addToEnvironment(env) {
        def varType = env.findType(declaredType)
        // Getter for the variable i.e. "x" or "x()", but no assignment like "x := 3" or "x = 3", after intially set.
        def meth = NewMethod(declaredName ++ "(0)", nil, varType)
        env.addMethod(meth)
    }
}


class VarNode(nm, decType, annotations, val) {
    def name is public = "var declaration"
    def declaredName is public = nm
    // Nil makes empty lists so declaredType and value become unknownType.
    def declaredType is public = if (decType.size > 0) then { decType.first } else { unknownType } 
    def value is public = if (isNil(val)) then { unknownType } else { val.first } // Always a list, unlike DefNode.

    method inferType(env) {
        // Infer environment types that are not unknown.
        def expectedType = env.findType(declaredType)

        // Check value is same type as declared. The unknownType always succeeds.
        def valueType = value.inferType(env)
        if (!expectedType.assignableFrom(valueType)) then {
            TypeError.raise "The var declaration '{declaredName}' needs type: '{expectedType.name}' was: '{valueType.name}'"
        }
        return doneType
    }

    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.assignableFrom(actual)) then {
            TypeError.raise "Var declarion invalid, expected: '{expected.name}', actual: '{actual.name}'"
        }
    }

    method addToEnvironment(env) {
        def varType = env.findType(declaredType)
        // Get and assign for the variable i.e. "x" or "x()", and "x := 3"
        def meth = NewMethod(declaredName ++ "(0)", nil, varType)
        def methAssign = NewMethod(declaredName ++ ":=(1)", o1N(varType), doneType)
        env.addMethod(meth)
        env.addMethod(methAssign)
    }
}


// Searches for a method in this environment and outer environments (until found or error thrown).
class LexicalRequestNode(meth, args, generics) {
    def name is public = "lexical request"
    def methodName is public = meth
    def arguments is public = args
    def genericParams is public = generics // Unused currently.

    // Checks the method exists in the environment and returns the return type of it.
    method inferType(env) {
        def meth = env.findMethod(methodName)
        // Check that the method takes the inferred arguments.
        def argumentTypes = arguments.map { a -> a.inferType(env) }
        meth.checkArguments(argumentTypes) // Check arguments are subtype or throw error.
        return meth.returnType
    }

    // Checking the lexical request searched object type (calculated in inferType).
    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.assignableFrom(actual)) then {
            TypeError.raise "Lexical request {methodName} was {expected.name} wanted {actual.name}"
        }
    }

    method asString {
        return name
    }
}


// Searches for a dotted method (e.g. 3.asString) in this environment and outer environments (until found or error thrown).
class DotRequestNode(rec, meth, args, generics) {
    def name is public = "dot request"
    def receiver is public = rec
    def methodName is public = meth
    def arguments is public = args
    def genericParams is public = generics // Unused currently.

    // Checks the method is in the receiver and returns the return type of it.
    method inferType(env) {
        def receiverType = receiver.inferType(env)
        def argumentTypes = arguments.map { a -> 
            a.inferType(env) 
        }

        // Directly check the receiver type has this method.
        if (!receiverType.hasMethod(methodName)) then {
            TypeError.raise "No method called '{methodName}' on {receiverType.asString}"
        }
        // Check the argument types match the method parameter types.
        def targetMethod = receiverType.getMethod(methodName)
        targetMethod.checkArguments(argumentTypes)
        return targetMethod.returnType
    }

    // Checking the dot request return type (calculated in inferType).
    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.assignableFrom(actual)) then {
            TypeError.raise "Dot request {receiver.name}.{methodName} was {expected.name} wanted {actual.name}"
        }
    }

    method asString {
        return name
    }
}


// Helper to add declarations for objects, methods and blocks.
method addDeclarations(env, body) {
    body.do { n ->
        if (((n.name == "var declaration") || (n.name == "def declaration")) || 
            ((n.name == "type declaration") || (n.name == "method declaration"))) then {
            n.addToEnvironment(env)
        } 
    }
}


class ObjectNode(bdy, anns) {
    def body is public = bdy.without { x -> x.name == "comment" }
    def annotations is public = anns

    method inferType(env) {
        def deeperEnv = Environment(env)
        addDeclarations(deeperEnv, body) // Method (or variable) declarations.
        // The self method of this object.
        deeperEnv.addMethod(NewMethod("self(0)", nil, deeperEnv.asType))

        body.do { m ->
            def methType = m.inferType(deeperEnv)
            m.checkType(deeperEnv, methType)
            // print "Object body Type: {bodyType.asString}"
        }

        return deeperEnv.asType
    }

    // For the root object it compares against unknown type (always true). Otherwise, compares objects.
    method checkType(env, expected) {
        def envType = inferType(env) 
        if (!expected.assignableFrom(envType)) then {
            TypeError.raise "ObjectNode is not valid"
        }
    }
}


// Excluded from the body of other types.
class CommentNode(txt) {
    def name is public = "comment"
    def text is public = txt

    method inferType(env) { return unknownType } // TODO May be unnecessary if all body lists remove comments.
    method checkType(env, expected) {} // Comments always valid, so never throws error.
}


// Return statement in method bodies.
class ReturnNode(val) {
    def name is public = "return"
    def value is public = val

    // Infer the type of the value after the 'return' keyword.
    method inferType(env) { 
        return value.inferType(env) 
    }

    // Check the type of the value against an expected type. TODO Send env.returnType as expected.
    method checkType(env, expected) {
        def valueType = inferType(env)
        if (!expected.assignableFrom(valueType)) then {
            TypeError.raise "Return statement value type: '{valueType.name}' not a subtype of expected type '{expected.name}'"
        }
    }
}


// Intepolate/format variables inside strings with curly brace notation. e.g. "x is {x}"
class InterpolatedStringNode(pre, expr, suff) {
    def name is public = "string interpolation"
    def prefix is public = pre
    def expression is public = expr
    def suffix is public = suff

    // The resulting type is a standard string.
    method inferType(env) {
        return stringType
    }

    method checkType(env, expected) {
        def actual = self.inferType(env)
        if (!expected.assignableFrom(actual)) then {
            TypeError.raise "'{actual.name} ({actual.methods.join(", ")})' is not a subtype of '{expected.name} ({expected.methods.join(", ")})' for interpolated string" 
        }
    }
}


class MethodNode(params, rType, anns, bdy) {
    def name is public = "method declaration"
    def parameters is public = params
    def rType is public = if (isNil(rType)) then { unknownType } else { rType } // Unknown if nil.
    def annotations is public = anns
    def body is public = bdy.without { x -> x.name == "comment" } // Remove comments from body.

    // The method declaration itself returns the done type.
    method inferType(env) { 
        return doneType 
    }

    method checkType(env, expected) {
        def returnT = env.findType(returnType)
        // Construct deeper environment which returns this rType.
        def deeperEnv = Environment(env)
        deeperEnv.methodReturn(rType)
        // Add the parameters to this environment
        // Add the body declarations to this environment
        // Check the last expression against the return type as it will be returned.
    }
}



// Stores global and local variables. Mapping of variable names to values. Builds upon other environemtns to determine what object is currently Self. 
// The top-most parent of any Environment is BaseEnvironment and is recusively reached when searching for variables/methods.
class Environment(par) {
    inherit BaseEnvironment
    def parent is public = par
    var methods is public := nil // Calling methods or variable getters.
    var types is public := nil // For type declaration nodes.
    // These two are used if this environment itself is a method.
    var returnType := nil
    var methodScope is public := ""

    // Add a method to the environment at the start of the list to mask outer methods with the same name.
    method addMethod(meth) is override {
        // TODO possibly handle throwing error if adding the same named method.
        methods.add(meth) at(1)
    }
    
    // Search for methods with matching name. Finds first (masking of outer environments).
    method findMethod(name) is override {
        methods.do { n ->
            if (n.name == name) then {
                return n
            }
        }
        // Could not find in its own environment, so check parent.
        return parent.findMethod(name)
    }

    // Setup method return type which is recursively looked up. Called inside MethodNode (m0D).
    method methodReturn(rType, scp) {
        returnType := rType
        methodScope := scp
    }

    // If this environment is within a method then it recursively finds the return type.
    method returnType is override {
        // Return type is set if not nil (can be unknownType).
        if (!isNil(returnType)) then {
            return returnType
        }
        return parent.returnType
    }

    method findType(expr) is override {
        // TODO
        //if (expr.name == "lexical request") then {
            //def methodName = expr.methodName
            //def name = methodName.substringFrom(1)to(methodName.size - 3)
            
            // Search through custom declared types.
            // TODO Need an addType() method that creates name, value pairs. Or better: use a dictionary instead of objects with two fields.
            //types.do { t ->
            //    if (t.name == name) then {
            //        return t.value
            //    }
            //}
        //}
        return parent.findType(expr)
    }

    // Make an AnyType representation of this environment for typechecking.
    method asType {
        def envType = AnyType("environment")
        methods.do { x ->
            envType.addMethod(x)
        }
        return envType
    }
}



// The base environment/scope of the script with no variables/methods, but can resolve basic types.
class BaseEnvironment {
    def baseTypes = collections.dictionary ["Unknown" :: unknownType, "Done" :: doneType, 
                        "Boolean" :: booleanType, "Number" :: numberType, "String" :: stringType]

    method addMethod(meth) {
        TypeError.raise "Cannot add '{meth.name}' to base environment"
    }

    method findMethod(name) {
        // Could make these a list or dictionary. TODO ifelse, loops.
        if (name == "print(1)") then { return NewMethod(name, o1N(unknownType), doneType) }
        if ((name == "false(0)") || (name == "true(0)")) then { return arglessMeth(name, booleanType) }
        TypeError.raise "No method called '{name}' in scope"
    }

    method returnType {
        TypeError.raise "Invalid return statement as there is no enclosing method"
    }

    // Find a literal type object via the name.
    method findType(expr) {
        var name := expr.name
        // Handle lexical requests.
        if (name == "lexical request") then {
            // Extract method name without argument count label e.g. "String" not "String(0)"
            def methName = expr.methodName
            name := methName.substringFrom(1)to(methName.size - 3)
        }
        // Gets literal for static types (Unknown, Done, Boolean, Number, String).
        if (baseTypes.containsKey(name)) then {
            return baseTypes.at(name)
        }
        // Boolean literals are lexically resolved (true -> "boolean value"). TODO ensure this is no longer necessary because of findMethod().
        //if (name == "boolean value") then {
        //    print("Boolean value")
        //    return booleanType
        //}
        TypeError.raise "Unexpected type: {name}"
    }
}


//
// #### AST TESTS ####
//

print("\n-----Tests-----")
var testNum := 1 // Increments after each test.

// Could compare test e.message but it is not robust.
// Even better I could use an error code that is different for each location. i.e. T21 then scan first 3 character of error message.

// Try-catch to run AST then throw a FailedError if it did not throw the expected TypeError exception.
method assertFails(ast) {
    try {
        ast.checkType(Environment(BaseEnvironment), unknownType)
        FailedError.raise "No TypeError"
    } catch { e : TypeError ->
        print "PASSED: Test{testNum} successfully threw -> {e}"
    } catch { e : FailedError ->
        print "-FAILED-: Test{testNum} did not throw a TypeError"
    }
    testNum := testNum + 1
}

// Opposite to succeed only if no TypeError thrown.
method assertPasses(ast) {
    try {
        ast.checkType(Environment(BaseEnvironment), unknownType)
        print "PASSED: Test{testNum} did not throw a TypeError"
    } catch { e : TypeError ->
        print "-FAILED-: Test{testNum} unexpectedly threw -> {e}"
    }
    testNum := testNum + 1
}


// Eventually make it parse files directly for the tests. Add new tests to the end to not mess up test number order.

// TEST 1
// 3
assertPasses(o0C(o1N(n0M(3)),nil))

// TEST 2
// "test"
assertPasses(o0C(o1N(s0L("test")),nil))

// TEST 3
// true
assertPasses(o0C(o1N(l0R("true(0)",nil,nil)),nil))

// TEST 4
// !false
assertPasses(o0C(o1N(d0R(l0R("false(0)",nil,nil),"prefix!(0)",nil,nil)),nil))

// TEST 5
// 3 + "hi"
assertFails(o0C(o1N(d0R(n0M(3),"+(1)",o1N(s0L("hi")),nil)),nil))

// TEST 6
// "hi" ++ 3
assertFails(o0C(o1N(d0R(s0L("hi"),"++(1)",o1N(n0M(3)),nil)),nil))

// TEST 7
// 3 + 11
assertPasses(o0C(o1N(d0R(n0M(3),"+(1)",o1N(n0M(11)),nil)),nil))

// TEST 8
// var x := 3
// var y : String := x
assertFails(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),v4R("y",o1N(l0R("String(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))),nil))

// TEST 9
// var x : Boolean := true
// var y : String := x
assertFails(o0C(c2N(v4R("x",o1N(l0R("Boolean(0)",nil,nil)),nil,o1N(l0R("true(0)",nil,nil))),v4R("y",o1N(l0R("String(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))),nil))

// TEST 10
// var y : String := 3
assertFails(o0C(o1N(v4R("y",o1N(l0R("String(0)",nil,nil)),nil,o1N(n0M(3)))),nil))

// Undefined variable and method.

// TEST 11
// a
assertFails(o0C(o1N(l0R("a(0)",nil,nil)),nil))

// TEST 12
// a.b
assertFails(o0C(o1N(d0R(l0R("a(0)",nil,nil),"b(0)",nil,nil)),nil))

// TODO finish custom types so these tests gives the correct error message.
// TEST 13
// def x = 3
// x.test
assertFails(o0C(c2N(d3F("x",nil,nil,n0M(3)),d0R(l0R("x(0)",nil,nil),"test(0)",nil,nil)),nil))

// TEST 14
// def x = 3
// 1 + x.test(1)
assertFails(o0C(c2N(d3F("x",nil,nil,n0M(3)),d0R(n0M(1),"+(1)",o1N(d0R(l0R("x(0)",nil,nil),"test(1)",o1N(n0M(1)),nil)),nil)),nil))

// TEST 15
// var x : String := "test"
assertPasses(o0C(o1N(v4R("x",o1N(l0R("String(0)",nil,nil)),nil,o1N(s0L("test")))),nil))

// The only current standard library method: print(1). TODO if(1)else(1), for/while loops.

// TEST 16
// print "Hello, world"
assertPasses(o0C(o1N(l0R("print(1)",o1N(s0L("Hello, world")),nil)),nil))

// TEST 17
// print "Hello! world"
assertPasses(o0C(o1N(l0R("print(1)",o1N(s0L(s4F("Hello",c9E," world"))),nil)),nil))

// TEST 18
// print 3
assertPasses(o0C(o1N(l0R("print(1)",o1N(n0M(3)),nil)),nil))

// TEST 19
// print(3, 3)
assertFails(o0C(o1N(l0R("print(2)",c2N(n0M(3),n0M(3)),nil)),nil))

// TEST 20
// print
assertFails(o0C(o1N(l0R("print(0)",nil,nil)),nil))

// TEST 21
// var x := 3
assertPasses(o0C(o1N(v4R("x",nil,nil,o1N(n0M(3)))),nil))

// You can make a method (no parameter variable) without a value.

// TEST 22
// var x
assertPasses(o0C(o1N(v4R("x",nil,nil,nil)),nil))

// TEST 23
// var x
// var y : String
assertPasses(o0C(c2N(v4R("x",nil,nil,nil),v4R("y",o1N(l0R("String(0)",nil,nil)),nil,nil)),nil))

// TEST 24
// var x := 3
// x := 4
//assertPasses(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),a5N(l0R("x(0)",nil,nil),n0M(4))),nil))

// TEST 25
// var x := 3
// x := true
//assertFails(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),a5N(l0R("x(0)",nil,nil),l0R("true(0)",nil,nil))),nil))

// TEST 26
// def x : Boolean = true
assertPasses(o0C(o1N(d3F("x",o1N(l0R("Boolean(0)",nil,nil)),nil,l0R("true(0)",nil,nil))),nil))

// TEST 27
// if (3 == 3) then { print("equal") }
//assertPasses(o0C(o1N(l0R("if(1)then(1)",c2N(d0R(n0M(3),"==(1)",o1N(n0M(3)),nil),b1K(nil,o1N(l0R("print(1)",o1N(s0L("equal")),nil)))),nil)),nil))

// TEST 28
// if ("hi") then {}
//assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(s0L("hi"),b1K(nil,nil)),nil)),nil))

// TEST 29
// if (7) then {}
//assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(n0M(7),b1K(nil,nil)),nil)),nil))

// TEST 30
// if (true) then {}
//assertPasses(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(nil,nil)),nil)),nil))

// TEST 31
// if (true) then {}
//assertPasses(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(nil,nil)),nil)),nil))

// If statements don't take an input in their block.
// TEST 32
// if (true) then { b -> 1 }
// assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(o1N(i0D("b",nil)),o1N(n0M(1)))),nil)),nil))

// TEST 32
// if (true) then { b : Number -> 1 }
//assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(o1N(i0D("b",o1N(l0R("Number(0)",nil,nil)))),o1N(n0M(1)))),nil)),nil))

// TODO make many more method tests for all edge cases.

// TEST 33
// method test {}
// assertPasses(o0C(o1N(m0D(o1N(p0T("t",nil,nil)),nil,nil,nil)),nil))




// I think this should fail because Test refers to both.
// def Test = object {}
// class Test {}
//assertFails(o0C(c2N(d3F("Test",nil,nil,o0C(nil,nil)),m0D(o1N(p0T("Test",nil,nil)),nil,nil,o1N(o0C(nil,nil)))),nil))


// Very complex test: sample.grace
//assertPasses(o0C(c0N(i0M("ast",i0D("ast",nil)),c0N(c0M(" This file makes use of all AST nodes"),c2N(d3F("x",nil,nil,o0C(c2N(v4R("y",o1N(l0R("Number(0)",nil,nil)),nil,o1N(n0M(1))),m0D(c2N(p0T("foo",o1N(i0D("arg",o1N(l0R("Action(0)",nil,nil)))),nil),p0T("bar",o1N(i0D("n",nil)),nil)),o1N(l0R("String(0)",nil,nil)),nil,c2N(a5N(d0R(l0R("self(0)",nil,nil),"y(0)",nil,nil),d0R(d0R(l0R("arg(0)",nil,nil),"apply(0)",nil,nil),"+(1)",o1N(l0R("n(0)",nil,nil)),nil)),r3T(i0S(s4F("y ",c9A," "),l0R("y(0)",nil,nil),s0L(s4F("",c9E,""))))))),nil)),l0R("print(1)",o1N(d0R(l0R("x(0)",nil,nil),"foo(1)bar(1)",c2N(b1K(nil,o1N(n0M(2))),n0M(3)),nil)),nil)))),nil))

// TODO make more complex tests that should pass or fail.
