// Importing library collections.grace to use array lists and dictionaries instead of linked lists.
import "collections" as collections

// Defined custom exceptions. Usage: TypeError.raise "message"
def TypeError = Exception.refine "TypeError"
def FailedError = Exception.refine "FailedError"

// All the nodes that can throw errors in checkType use different errors deriving from typeerror for specific tests.
def MethodError = TypeError.refine "MethodError"
def ObjectError = TypeError.refine "ObjectError"
def LiteralError = TypeError.refine "LiteralError"
def DefError = TypeError.refine "DefError"
def VarError = TypeError.refine "VarError"
def LexicalReqError = TypeError.refine "LexicalReqError"
def DotReqError = TypeError.refine "DotReqError"
def ReturnError = TypeError.refine "ReturnError"
def StringError = TypeError.refine "StringError"
def BlockError = TypeError.refine "BlockError"
def InterfaceError = TypeError.refine "InterfaceError"
def LineupError = TypeError.refine "LineupError"
def ImportError = TypeError.refine "ImportError"
def TypeDeclError = TypeError.refine "TypeDeclError"
def EnvError = TypeError.refine "EnvError"


//
// #### AST METHODS ####  
// TODO: the annotations/generics parameters in many of these nodes are unused. It is an advanced feature and my focus is on completing structural typing and then generics, so it may not be completed.
//


// AST for special character constants.
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
def specialChars = collections.list [c9B, c9D, c9S, c9L, c9N, c9R, c9Q, c9T, c9C, c9G, c9A, c9P, c9H, c9E]


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

// Interpolated string for formatting, e.g. "price is {y} dollars" -> prefix="price is ", expr={y}, and suffix=" dollars"
method i0S(prefix, expr, suffix) { InterpolatedStringNode(prefix, expr, suffix) }

// SafeStr for formatting special characters with prefix/suffix.
method s4F(prefix, expr, suffix) {
    // Check if it is a special character.
    if (!specialChars.contains { char -> char == expr }) then { 
        StringError.raise "'{expr}' is not a special character expression in the safe String" 
    }
    // Prefix is normal string, expr is special character as string, suffix is string (or s4F returning string).
    return prefix ++ expr ++ suffix
}

// Block/lambda containing parameters and body to be executed with the apply method.
method b1K(params, body) { BlockNode(params, body) }

// Declarations for def and var.
method d3F(name, dType, anns, value) { DefNode(name, dType, anns, value) }
method v4R(name, dType, anns, value) { VarNode(name, dType, anns, value) }

// Reassignment of a variable (defined by var). Uses lexical or dot request. No class needed.
method a5N(lhs, rhs) {
    if (lhs.name == "lexical request") then {
        def methName = lhs.cleanName ++ ":=(1)"
        // If rhs does not match the old type of the variable this fails to typecheck in LexicalRequestNode.
        return LexicalRequestNode(methName, o1N(rhs), doneType) 
    } elseif {lhs.name == "dot request"} then {
        def methName = lhs.cleanName ++ ":=(1)"
        return DotRequestNode(lhs.receiver, methName, o1N(rhs), doneType)
    } else {
        VarError.raise "Invalid left side of variable assignment: '{lhs.name}'"
    }
}

// Type and interface declaration, e.g. type A = interface {}
method t0D(name, genericParams, value) { TypeNode(name, genericParams, value) } 
method i0C(body) { InterfaceNode(body) }

// Method signature (without a body, used in interfaces).
method m0S(parts, rType) { MethodSignatureNode(parts, rType) }
// Method declaration (includes annotations and body).
method m0D(parts, rType, anns, body) { MethodNode(parts, rType, anns, body) }

// Implicit/lexical request of variable/methods.
method l0R(name, args, genericParams) { LexicalRequestNode(name, args, genericParams) }

// Explicit/dot request of variable/methods.
method d0R(receiver, name, args, genericParams) { DotRequestNode(receiver, name, args, genericParams) }

// Individual part of a method signature, e.g. foo(a) in "method foo(a) bar(b)"
method p0T(name, params, genericParams) { PartNode(name, params, genericParams) }

// Parameter type identifier in a method signature part, e.g. "a : String" within "method foo(a : String, b : Number)".
method i0D(name, dType) { IdentifierNode(name, dType) }

// The constructor of an object. Also used as the root node AST.
method o0C(body, anns) { ObjectNode(body, anns) }

// Return statement for a method.
method r3T(value) { ReturnNode(value) }

// Comment. Gets excluded from the body of objects, methods and blocks.
method c0M(text) { CommentNode(text) }

// Lineup infers each element between square brackets "[1, 2, 3]"
method l0N(elems) { LineupNode(elems) }

// Import statement using source string, e.g. import "ast" as ast
method i0M(source, binding) { ImportNode(source, binding) }

// Dialect Statement that extends the Grace language using source string, e.g. dialect "name"
method d0S(source) { DialectNode(source) }


//
// #### IMPLEMENTATION ####
// Bidirectional because "inferType" deduces type bottom-up and "checkType" tests if an expression matches type top-down or throws error.
//


// Create a method inside a type.
class NewMethod(nm, params, rType) {
    def name is public = nm
    // Has to be mutable for a5N to work.
    var paramTypes is public := params
    var returnType is public := rType
    def paramNames = "{ params.map { p -> p.name }.join(", ") }"
    def fullName is public = "{name}({paramNames}) -> {returnType.name}"

    // Check the sent args subtype the declared methods parameters.
    method argumentsSubtype(argTypes) {
        // Check the number of arguments matches the parameters.
        if (argTypes.size != paramTypes.size) then {
            return false
        }
        paramTypes.zip(argTypes) do { param, arg ->
            // Checking if the param matches the argument type.
            if (!param.acceptsSubtype(arg)) then {
                return false
            }
        }
        return true
    }

    // Reuses argument subtype check but throws error instead of returning false.
    method checkArguments(argTypes) {
        def argNames = "{ argTypes.map { a -> a.name }.join(", ") }"
        if (!argumentsSubtype(argTypes)) then {
            MethodError.raise "The method arguments for '{fullName}' must be [{paramNames}] not [{argNames}]"
        }
    }

    method asString {
        // TODO I want this nice format but it makes some tests fail?
        return "{name}->{returnType.declaredName}"
    }
}


// Helper to make method with one argument with same type as the return type.
method sameArgMeth(name, rType) {
    return NewMethod(name, o1N(rType), rType)
}
// Helper to make method with no arguments and specific return type.
method arglessMeth(name, rType) {
    return NewMethod(name, nil, rType)
}


// Node for all literal types to be built upon.
class AnyType(nm) {
    def name is public = nm
    var declaredName is public := nm // "String" or "A" in "type A = interface {...}".
    var methods is public := nil

    // Setup multiple methods alongside some default ones.
    method setupMethods(meths) {
        methods := meths
        // Methods that all basic types have (excluding done).
        addMethod(NewMethod("==(1)", o1N(unknownType), booleanType))
        addMethod(NewMethod("!=(1)", o1N(unknownType), booleanType))
        addMethod(NewMethod("asString(0)", nil, stringType))
        // Methods for variants, unions and intersections between types.
        addMethod(NewMethod("|(1)", o1N(unknownType), unknownType))
        addMethod(NewMethod("&(1)", o1N(unknownType), unknownType))
        addMethod(NewMethod("+(1)", o1N(unknownType), unknownType))
    }

    // Checks if the type is an Interface or Environment (instead of just Interface, so that self can assign to a custom type).
    method selfOrInterface -> Boolean {
        return (name == "Interface") || (name == "Environment")
    }

    // Add a method to the methods list.
    method addMethod(meth) {
        methods.add(meth)
    }

    // I want it to throw an error if a method doesn't exist usually. Only used for subtype checking.
    method hasMethod(methName) {
        return methods.contains { meth -> meth.name == methName } 
    }

    // Get a method using a string name.
    method getMethod(methName) {
        methods.do { meth -> 
            if (meth.name == methName) then { return meth } 
        }
        TypeError.raise "Method {methName} does not exist for {name}"
    }

    // Format name with methods for easy debugging.
    method asString {
        return "{name} ['{methods.join("', '")}']"
    }

    // If any methods are not subtyped from parent via a certain function then return false.
    method compareMethods(parent, subtype, block) {
        var result : Boolean := true
        parent.methods.do { meth ->
            result := result && block.apply(parent, subtype, meth)
        }
        return result
    }

    // Reset trail which is used to track coinductive pairs for subtyping.
    method acceptsSubtype(subtype) {
        // Check all methods. If any are not subtyped in the subtype object then return false.
        return compareMethods(self, subtype, { p, s, n -> acceptsCoinductive(p, s, n, nil) })
    }

    // Compare another type with this type accounting for possible coinductive relationships (interacting infinte dependencies).
    method acceptsCoinductive(parent, subtype, meth, prevTrail) { // TODO send a list copy as parameter so it doesn't see other branches.
        // TODO check if the unknown checking here is correct. I want the parent == subtype to stay, because if they are the same type, no need to use coinduction.
        if ((subtype.name == "Unknown") || (parent.name == "Unknown") || (parent == subtype)) then {
            return true
        }

        // Neither are custom types, fallback to base subtype comparison for all methods. Recursion lets this get checked at any point.
        if (!subtype.selfOrInterface && !parent.selfOrInterface) then {
            return compareMethods(parent, subtype, { p, s, n -> acceptsBase(p, s, n) })
        }
        // If one is not an interface but the other one is, it is not a coinductive or a normal subtype as the structures differ.
        if (!(subtype.selfOrInterface && parent.selfOrInterface)) then {
            return false
        }

        // Copy the trail so it does not mutate other branches.
        def trail = collections.list(prevTrail)
        def pair = "({parent.declaredName}, {subtype.declaredName})"
        // If result pair is in the trail, then it has already seen this pair in a dependency loop, hence it is an equivalent coinductive structure.
        if (trail.contains { p -> p == pair }) then {
            return true
        }
        trail.add(pair)

        // If the subtype does not have a matching method, then it is not equivalent.
        if (!subtype.hasMethod(meth.name)) then {
            return false
        }
        def subtypeMeth = subtype.getMethod(meth.name)
        // If the parameter arity is different between parent and subtype, then they are not equivalent.
        def paramsParent = meth.paramTypes
        def paramsSubtype = subtypeMeth.paramTypes
        if (paramsParent.size != paramsSubtype.size) then {
            return false
        }
        // Also get the return type of parent and subtype for this method.
        def returnParent = meth.returnType
        def returnSubtype = subtypeMeth.returnType
        
        // For the parent and child return types of this method, coinductively recurse on all the methods within them. Uses current trail.
        var result : Boolean := compareMethods(returnParent, returnSubtype, { p, s, n -> acceptsCoinductive(p, s, n, trail) })
        // Do the same coinductive recursion as the return types, but for every pair of parameter types.
        paramsParent.zip(paramsSubtype) do { par, sub ->
            result := result && compareMethods(par, sub, { p, s, n -> acceptsCoinductive(p, s, n, trail) })
        }
        return result
    }

    // Compare another type with this type to check if matching/subtype.
    method acceptsBase(parent, subtype, meth) {
        // Unknown always subtypes and the same object subtypes.
        if ((subtype.name == "Unknown") || (parent.name == "Unknown") || (parent == subtype)) then {
            return true
        }

        // The subtype should at least have the methods of the parent type.
        if (!subtype.hasMethod(meth.name)) then {
            return false
        }
        def subtypeMeth = subtype.getMethod(meth.name)

        // Compare the method params with the subtype.
        if (!meth.argumentsSubtype(subtypeMeth.paramTypes)) then {
            return false
        }
        if (!meth.returnType.acceptsSubtype(subtypeMeth.returnType)) then {
            return false
        }
        // Succeeded method check. Name doesn't have to match.
        return true
    }
}


// Singleton for unknown types that can assign to any variable or be used in a method like "==(1)" that compares to any type.
def unknownType = object {
    def name is public = "Unknown"
    def declaredName is public = "Unknown"
    def methods is public = nil

    method compareMethods(_, _, _) { return true }
    method acceptsSubtype(_) { return true }
    method asString { return name }
    method addMethod(_) { TypeError.raise "Unknown type cannot add methods"}
    method hasMethod(_) { return false }
    method getMethod(_) { TypeError.raise "Unknown type has no methods" }
    method inferType(_) { return self }
    method checkType(_, _) {}
}


// Basic literal types along with their unique methods ("==(1)", "!=(1)" and "asString(0)" created in setupMethods).
def numberType = AnyType("Number")
def stringType = AnyType("String")
def booleanType = AnyType("Boolean")
def doneType = AnyType("Done") // Similar to void, def/var/methods without return/types return this when done.
def importType = AnyType("Import") // Special case for imported types as they have unknown methods (always succeeds).
def arglessBlockType = AnyType("Block") // Block without args, such as for an if statement.

numberType.setupMethods(c0N(sameArgMeth("+(1)", numberType), c2N(sameArgMeth("*(1)", numberType), sameArgMeth("..(1)", numberType))))
stringType.setupMethods(c2N(sameArgMeth("++(1)", stringType), arglessMeth("size(0)", numberType)))
booleanType.setupMethods(o1N(arglessMeth("prefix!(0)", booleanType)))
arglessBlockType.addMethod(NewMethod("apply(0)", nil, unknownType))
doneType.addMethod(NewMethod("asString(0)", nil, stringType)) // DoneType only has asString(0).



// Node that stores a name and literal value and can compare types via structual subtyping.
class LiteralNode(nm, v, lit) {
    def name is public = nm
    def value is public = v
    def literal is public = lit

    method asString { 
        return name 
    }
    
    method inferType(env) {
        return literal
    }

    method checkType(env, expected) { // Expected and Actual are AnyType literals.
        def actual = self.inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            LiteralError.raise "Actual type '{actual}' is not a subtype of expected '{expected}' for {name}" 
        }
    }
}


// Def declarations are immutable. You can get but not assign them.
class DefNode(nm, decType, annotations, val) {
    def name is public = "def declaration"
    def declaredName is public = nm
    def declaredType is public = if (decType.size > 0) then { decType.first } else { unknownType }
    def value is public = if (isNil(val)) then { unknownType } else { val }

    method inferType(env) {
        return doneType
    }

    method checkType(env, expected) {
        // Def needs initial value, so if it is nil (uses unknownType), raise error.
        if (value.name == "Unknown") then {
            DefError.raise "{name} needs initial value"
        }
        // Find and compare value with declared type.
        def expectedType = env.findType(declaredType)
        def valueType = value.inferType(env)
        if (!expectedType.acceptsSubtype(valueType)) then {
            DefError.raise "For {name} '{declaredName}' inferred value '{valueType}' is not a subtype of '{expectedType}'"
        }
        
        // Check expected is doneType.
        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            DefError.raise "{name} invalid, expected type '{expected}', actual '{actual}'"
        }
    }

    method addToEnvironment(env) {
        // If there is no declared type, use the inferred value instead.
        def varType = if (declaredType.name == "Unknown") then { value.inferType(env) } else { env.findType(declaredType) }
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
        return doneType
    }

    method checkType(env, expected) {
        // Check value is same type as declared. The unknownType always succeeds.
        def expectedType = env.findType(declaredType)
        def valueType = value.inferType(env)
        if (!expectedType.acceptsSubtype(valueType)) then {
            VarError.raise "The var declaration '{declaredName}' inferred value '{valueType}' is not a subtype of '{expectedType}'"
        }

        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            VarError.raise "Var declarion invalid, expected '{expected}', Actual type '{actual}'"
        }
    }

    method addToEnvironment(env) {
        // If there is no declared type, use the inferred value instead.
        def varType = if (declaredType.name == "Unknown") then { value.inferType(env) } else { env.findType(declaredType) }
        // Get and assign for the variable i.e. "x" or "x()", and "x := 3"
        def meth = NewMethod(declaredName ++ "(0)", nil, varType)
        def methAssign = NewMethod(declaredName ++ ":=(1)", o1N(varType), doneType)
        env.addMethod(meth)
        env.addMethod(methAssign)
    }
}


// Detect a reassignment to change the type of potentially unknown variable.
method reassignChangesType(env, cleanName, args, getter, setter) {
    if (args.size != 1) then {
        LexicalReqError.raise "A reassignment lexical request must take 1 argument"
    }
    def argType = args.first.inferType(env)
    // If it initially was unknown, make getter and setter now use the argument type (which could still be unknown).
    if (getter.returnType.name == "Unknown") then {
        getter.returnType := argType
        setter.paramTypes := o1N(argType)
    }
}


// Searches for a method in this environment and outer environments (until found or error thrown).
class LexicalRequestNode(meth, args, generics) {
    def name is public = "lexical request"
    def methodName is public = meth
    def cleanName is public = meth.substringFrom(1)to(methodName.size - 3) // No arguments e.g. "foo(1)" becomes "foo".
    def arguments is public = args
    def genericParams is public = generics // Unused currently.

    // Checks the method exists in the environment and returns the return type of it.
    method inferType(env) {
        def targetMethod = env.findMethod(methodName) // Throws error if not found.
        // Check that the method takes the inferred arguments.
        def argumentTypes = arguments.map { a -> a.inferType(env) }
        targetMethod.checkArguments(argumentTypes) // Check arguments are subtype or throw error.

        // If the variable was called test, looks up "test(0)" getter from "test:=(1)".
        if (cleanName.size > 2) then {
            if (cleanName.substringFrom(cleanName.size - 1)to(cleanName.size) == ":=") then {
                def getter = env.findMethod("{cleanName.substringFrom(1)to(cleanName.size - 2)}(0)")
                // Handle reassignments changing type from unknown.
                reassignChangesType(env, cleanName, arguments, getter, targetMethod)
            }
        }
        return targetMethod.returnType
    }

    // Checking the lexical request method return type (calculated in inferType).
    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            LexicalReqError.raise "Lexical request '{methodName}' inferred '{actual}' is not a subtype of '{expected}'"
        }
    }

    method asString {
        return name
    }
}


// Searches for a dotted method (e.g. 3.asString or x.y) by finding the reciever in this environment or search outer environments (until found or throw error).
class DotRequestNode(rec, meth, args, generics) {
    def name is public = "dot request"
    def receiver is public = rec
    def methodName is public = meth
    def cleanName is public = meth.substringFrom(1)to(meth.size - 3) // No arguments e.g. in x its method "foo(1)" becomes "foo".
    def arguments is public = args
    def genericParams is public = generics // Unused currently.

    // Checks the method is in the receiver and returns the return type of it.
    method inferType(env) {
        def receiverType = receiver.inferType(env)
        // If it is an imported type, it is unknown whether the method exists or what it returns.
        if ((receiverType.name == "Import") || (receiverType.name == "Unknown")) then {
            return unknownType
        }
        def argumentTypes = arguments.map { a -> 
            a.inferType(env)
        }

        // Directly check the receiver type has this method.
        if (!receiverType.hasMethod(methodName)) then {
            DotReqError.raise "No method called '{methodName}' on {receiverType}"
        }
        // Check the argument types match the method parameter types.
        def targetMethod = receiverType.getMethod(methodName)
        targetMethod.checkArguments(argumentTypes)

        if (cleanName.size > 2) then {
            if (cleanName.substringFrom(cleanName.size - 1)to(cleanName.size) == ":=") then {
                def getter = receiverType.getMethod("{cleanName.substringFrom(1)to(cleanName.size - 2)}(0)")
                // Handle reassignments changing type from unknown.
                reassignChangesType(env, cleanName, arguments, getter, targetMethod)
            }
        }
        return targetMethod.returnType
    }

    // Checking the dot request return type (calculated in inferType).
    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            DotReqError.raise "Dot request {receiver.name}.{methodName} expected '{expected}', but actually returned '{actual}'"
        }
    }

    method asString {
        return name
    }
}


// Helper to add declarations for objects, methods and blocks. Uses three passes to work with coinduction and self.
method addDeclarations(env, body) {
    // Register every type before resolving any declaration that may use it.
    body.do { expr ->
        if (expr.name == "type declaration") then {
            expr.addToEnvironment(env)
        }
    }

    // Lexically resolve type dependencies without changing their references.
    body.do { expr ->
        if (expr.name == "type declaration") then {
            env.resolveTypeDecl(expr.declaredName)
        }
    }

    // Adds the methods and value types to the environment after the coinductive types have been lexically resolved.
    body.do { expr ->
        if ((expr.name == "var declaration") || (expr.name == "def declaration") ||
            (expr.name == "method declaration") || (expr.name == "import statement")) then {
            expr.addToEnvironment(env)
        }
    }
}


// Either a singleton object (def x = object {}), a class (class x {}) or the outer-most scope of the entire program.
class ObjectNode(bdy, anns) {
    def name is public = "object"
    def body is public = bdy.without { x -> x.name == "comment" }
    def annotations is public = anns
    var outer is public := true

    method inferType(env) {
        def deeperEnv = Environment(env)
        addDeclarations(deeperEnv, body) // Method (or variable) declarations.
        // The self method of this object.
        deeperEnv.addMethod(NewMethod("self(0)", nil, deeperEnv.asType))

        // Recursively checktype the body elements. The actual type is unknown.
        body.do { expr ->
            expr.checkType(deeperEnv, unknownType)
        }
        return deeperEnv.asType
    }

    // For the root object it compares against unknown type (always true). Otherwise, compares objects.
    method checkType(env, expected) {
        def envType = inferType(env) 
        if (!expected.acceptsSubtype(envType)) then {
            ObjectError.raise "ObjectNode is not valid"
        }
    }
}


// Excluded from the body of other types.
class CommentNode(txt) {
    def name is public = "comment"
    def text is public = txt

    // Since all method/block body lists remove comments to not interfere with return types, throw error if not removed.
    method inferType(env) { TypeError.raise "Cannot infer comment type" }
    method checkType(env, _) { TypeError.raise "Cannot check comment type" }
}


// Return statement in method bodies.
class ReturnNode(val) {
    def name is public = "return statement"
    def value is public = val

    // Infer the type of the value after the 'return' keyword.
    method inferType(env) { 
        return value.inferType(env) 
    }

    // Check the type of the value against an expected type.
    method checkType(env, _) {
        def expected = env.getReturnType // Recursive lookup.
        def valueType = inferType(env)
        if (!expected.acceptsSubtype(valueType)) then {
            ReturnError.raise "Return statement value type '{valueType}' is not a subtype of expected type '{expected}'"
        }
    }
}


// Intepolate/format variables inside strings with curly brace notation, e.g. "x equals {x}"
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
        // To check expr method exists in environment.
        expr.checkType(env, unknownType)

        // This node should always be a stringType.
        def actual = self.inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            StringError.raise "Actual type '{actual}' is not a subtype of '{expected}' for {name}" 
        }
    }
}


// Actual method declaration in the parsed script (as AST).
class MethodNode(parts, rType, anns, bdy) {
    def name is public = "method declaration"
    // Merge all the method parts, e.g. "method foo(a) bar(b, c)" turns into "foo(1)bar(2)"
    def declaredName is public = parts.map { part -> "{part.declaredName}({part.parameters.size})" }.join("")
    def parameters is public = parts.flatMap { part -> part.parameters } // Merges into single list of of identifier nodes.

    // Store return type, unknownType if nil.
    def lexicalReturnType is public = if (isNil(rType)) then { unknownType } else { rType.first } 
    def annotations is public = anns
    def body is public = bdy.without { x -> x.name == "comment" } // Remove comments from body.

    // The method declaration itself returns the done type.
    method inferType(env) { 
        // Find the return type, either unknown or lexically searched.
        def returnType = env.findType(lexicalReturnType)
        // Construct deeper environment which returns this rType.
        def deeperEnv = Environment(env)
        deeperEnv.methodReturn(declaredName, returnType)

        // Add the method parameter getters to this environment
        parameters.do { param ->
            if (param.name != "parameter identifier") then {
                MethodError.raise "Invalid case of method parameter not wrapped by identifier" 
            }
            def paramType = env.findType(param.declaredType) // unknownType handled in findType.
            deeperEnv.addMethod(arglessMeth(param.declaredName ++ "(0)", paramType))
        }
        // Add the body declarations to this environment
        addDeclarations(deeperEnv, body)

        // Copy the body to exclude the final expression for ensuring no early return statements.
        def bodyCopy = collections.list(body)
        def finalExpr = if (body.size == 0) then { unknownType } else { bodyCopy.removeAt(bodyCopy.size) }
        // Still need to typecheck it, even though it is excluded from bodyCopy.
        finalExpr.checkType(deeperEnv, unknownType)
        bodyCopy.do { expr ->
            // Propagate typechecks down the expression children. Also finds nested return statements.
            expr.checkType(deeperEnv, unknownType) 

            // Throw error if 'return' statement before final expression without being inside a block. 
            // Could extend to detect unconditional blocks with returns, but unnecessary.
            if (expr.name == "return statement") then { 
                ReturnError.raise "Unreachable code in method body after return" 
            }
        }
        // Check the last body expression (regardless of explicit return) against the environment return type.
        def finalType = finalExpr.inferType(deeperEnv)
        if (!returnType.acceptsSubtype(finalType)) then {
            ReturnError.raise "Method expected return type '{returnType}', actually got '{finalType}' as final expression"
        }

        return doneType 
    }

    method checkType(env, expected) {
        // Check expected is doneType.
        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            MethodError.raise "Method expected result '{expected}', actually got '{actual}'"
        }
    }

    // Add this method expression to the current environment formatted as a NewMethod object.
    method addToEnvironment(env) {
        // Lexically finds the return type literal. Special case for classes as they make a method with one body element, the class itself.
        def returnType = if ((lexicalReturnType.name == "Unknown") && (body.size == 1)) then {
            if (body.first.name == "object") then { body.first.inferType(env) } else { lexicalReturnType }
        } else {
            env.findType(lexicalReturnType)
        }
        // CheckType already enforces all params are IdentifierNodes. Lexically finds the param types.
        def paramTypes = parameters.map { param -> env.findType(param.declaredType) }
        // Add a NewMethod object to environment.
        def methodFormat = NewMethod(declaredName, paramTypes, returnType)
        env.addMethod(methodFormat) 
    }
}


// The named parts of a method, e.g. "foo(x)" in "method foo(x) bar(y) {}"
class PartNode(nm, params, generics) {
    def name is public = "method part"
    def declaredName is public = nm
    def parameters is public = params // List of parameter identifiers, e.g. (name : type).
    def genericParams is public = generics // Unused currently.
}


// A parameters declared name and type (potentially absent) or generic name, e.g. "x : T" or "[[T]]" in "method test[[T]](x : T)"
class IdentifierNode(nm, decType) {
    def name is public = "parameter identifier"
    def declaredName is public = nm
    def declaredType is public = if (isNil(decType)) then { unknownType } else { decType.first } // Either unknownType or lexical request.
}


// Block for if statement, loop, lambda etc. Expressions within two braces "{}".
class BlockNode(params, bdy) {
    def name is public = "block declaration"
    def parameters = params // Can have parameters for a lambda block.
    def body = bdy

    method inferType(env) {
        // Construct deeper environment for this block.
        def deeperEnv = Environment(env)

        // Add the parameter getters to this environment and gets their types.
        def paramTypes = parameters.map { param ->
            if (param.name != "parameter identifier") then { 
                BlockError.raise "Invalid case of block parameter not wrapped by identifier" 
            }
            def paramType = env.findType(param.declaredType) // unknownType handled in findType.
            deeperEnv.addMethod(arglessMeth(param.declaredName ++ "(0)", paramType))
            paramType
        }
        // Add the body declarations to this environment
        addDeclarations(deeperEnv, body)

        // TODO could flag early returns as making unreachable code inside a block.
        // The block return type is unknown by default. Updates to the final element.
        var returnType := unknownType
        body.do { expr ->
            returnType := expr.inferType(deeperEnv)
            expr.checkType(deeperEnv, unknownType) // Propagate typechecks on children.
        }
        // Constructs the block type with a method called "apply" that takes the parameters and returns the last expr type.
        def blockType = AnyType("Block")
        blockType.addMethod(NewMethod("apply({parameters.size})", paramTypes, returnType))
        return blockType
    }

    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            BlockError.raise "Actual type '{actual}' is not a subtype of '{expected}' for {name}"
        }
    }
}


// Type declaration for a new type (used like String/Number/Boolean) that can be found in the current environment (or children of it).
class TypeNode(nm, generics, val) {
    def name is public = "type declaration"
    def declaredName is public = nm
    def genericParams is public = generics
    def value is public = if (isNil(val)) then { unknownType } else { val }

    method inferType(env) {
        return doneType
    }

    method checkType(env, expected) {
        // Type declarations need initial value (like def), so if it is nil (uses unknownType), raise error.
        if (value.name == "Unknown") then {
            TypeDeclError.raise "{name} needs initial value"
        }
        value.checkType(env, unknownType) // Typecheck the value (typically an interface).

        def actual = self.inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            TypeDeclError.raise "Actual type '{actual}' is not a subtype of '{expected}' for {name}" 
        }
    }

    // Add type to environment for typechecking new types from findType.
    method addToEnvironment(env) {
        // Either it gets the type representation of an interface, or it looks up an already defined type. e.g. String (or with union/intersection).
        def valueType = if (value.name == "interface declaration") then { value.asType(env) } else { env.findType(value) }
        valueType.declaredName := declaredName
        env.addType(declaredName, valueType)
    }
}


class InterfaceNode(bdy) {
    def name is public = "interface declaration"
    def body is public = bdy

    method inferType(env) {
        return doneType
    }

    method checkType(env, expected) {
        // Ensure the interface body only has method signatures.
        body.do { expr ->
            if (expr.name != "method signature") then {
                InterfaceError.raise "Only method signatures can be in an interface body, not '{expr.name}'"
            }
        }
        // Check it still expects doneType.
        def actual = self.inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            InterfaceError.raise "Actual type '{actual}' is not a subtype of '{expected}' for {name}" 
        }
    }

    // Convert to AnyType format to allow typechecking by adding to environment in TypeNode.
    method asType(env) {
        def interfaceType = AnyType("Interface")
        body.do { sig ->
            // Convert method signatures into NewMethod format then add to the environment.
            def meth = sig.asMethod(env)
            interfaceType.addMethod(meth)
        }
        return interfaceType
    }
}


// Method signature within an interface, e.g. interface { foo(a : String) -> String }
class MethodSignatureNode(parts, rType) {
    def name is public = "method signature"
    def declaredName is public = parts.map { part -> "{part.declaredName}({part.parameters.size})" }.join("")
    // Does not resolve lexical requests immediately (to handle depdendencies later) for return type and parameters.
    def lexicalParams is public = parts.flatMap { part -> part.parameters }.map { param -> param.declaredType } // Merges parts into single list.
    def lexicalReturnType is public = if (isNil(rType)) then { unknownType } else { rType.first } // unknownType if nil.

    // Convert this method signature into a NewMethod object for typechecking interfaces.
    method asMethod(env) {
        return NewMethod(declaredName, lexicalParams, lexicalReturnType)
    }
}


// Lineup of elements, such as [1, 2, 3].
class LineupNode(elems) {
    def name is public = "lineup"
    def elements is public = elems

    method inferType(env) {
        return unknownType // TODO
        // Typecheck the declared type against the common element type e.g. def x : List[[String]] = ["hi", "bye"]
        // Has to be a type that encompasses all other types, potentially needing to be Object if incompatable like: [3, "hi"]
        // So return some lineupType representation that stores String, then var/def can compare to that generic type.
    }

    method checkType(env, expected) {
        // TODO
        // LineupError.raise ""
    }
}


class ImportNode(src, bind) {
    def name is public = "import statement"
    def source is public = src
    def declaredName is public = bind.declaredName
    def declaredType is public = bind.declaredType // TODO if this is an interface then at least those methods are imported (check their params and return type when used).

    method inferType(env) {
        return doneType
    }

    method checkType(env, expected) {
        // Check doneType matching expected.
        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            ImportError.raise "Actual type '{actual}' is not a subtype of '{expected}' for {name}" 
        }
    }

    method addToEnvironment(env) {
        // TODO Is it likely possible to minimally typecheck the declaredType.
        //def decType = env.findType(declaredType)
        // For each method in declared type add "declaredName.method"

        def meth = NewMethod(declaredName ++ "(0)", nil, importType)
        env.addMethod(meth)
    }
}


class DialectNode(src) {
    def name is public = "dialect statement"
    def source is public = src

    method inferType(env) {
        return doneType
    }

    method checkType(env, expected) {
        def actual = inferType(env)
        if (!expected.acceptsSubtype(actual)) then {
            ImportError.raise "Actual type '{actual}' is not a subtype of '{expected}' for {name}"
        }
    }
}


// Stores global and local variables. Mapping of variable names to values. Builds upon other environemtns to determine what object is currently Self. 
// The top-most parent of any Environment is BaseEnvironment and is recusively reached when searching for variables/methods to terminate if not found.
class Environment(par) {
    inherit BaseEnvironment
    def parent = par
    var methods := nil // Storing methods and variable getters.
    var types := collections.dictionary [] // For type declaration nodes.
    // These two are used if this environment itself is a method.
    var returnType := nil
    var declaredName := nil

    // Add a method to the environment at the start of the list to mask outer methods with the same name.
    method addMethod(meth) is override {
        // Throws error if same method in the same environment. May miss some invalid cases between environments but allows shadowing.
        if (methods.contains { m -> m.fullName == meth.fullName }) then { 
            EnvError.raise "Method that already exists in current scope: '{meth}'"
        }
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

    // Setup method return type which is recursively looked up and method name. Called inside MethodNode (m0D).
    method methodReturn(methName, rType) {
        declaredName := methName
        returnType := rType
    }

    // If this environment is within a method then it recursively finds the return type.
    method getReturnType is override {
        // Return type is set if not nil (can be unknownType).
        if (!isNil(returnType)) then {
            return returnType
        }
        return parent.getReturnType
    }

    method getDeclaredName is override {
        // Like return type it gets the method declared name.
        if (!isNil(declaredName)) then {
            return declaredName
        }
        return parent.getDeclaredName
    }

    // Add a type declaration.
    method addType(nm, val) {
        // TODO could recursively lookup types. And check matching method names for conflicts as well.
        // while loop parent.parent types.containsKey

        if (types.containsKey(nm)) then {
            EnvError.raise "Same name {nm} used for a type declaration already"
        }
        types.at(nm) put(val)
    }

    method findType(expr) is override {
        // Lexical request for types, BaseEnvironment has the default types such as String, Number and Boolean.
        if (expr.name == "lexical request") then {
            def name = expr.cleanName
            
            // Search through declared types.
            if (types.containsKey(name)) then {
                return types.at(name)
            }
        }
        return parent.findType(expr)
    }

    method resolveTypeDecl(typeName) {
        if (types.containsKey(typeName)) then {
            def placeholderType = types.at(typeName)
            def newMethods = nil
            placeholderType.methods.do { meth ->
                def resolvedParams = meth.paramTypes.map { param -> findType(param) }
                def resolvedReturn = findType(meth.returnType)
                def resolvedMethod = NewMethod(meth.name, resolvedParams, resolvedReturn)
                newMethods.add(resolvedMethod) 
            }
            // Mutate the methods but don't replace the type, so all the references in the 'types' list are preseved.
            placeholderType.setupMethods(newMethods)
        } else {
            // Shouldn't occur if the two-pass system is working in the addDeclarations method.
            EnvError.raise "This type was not detected despite being declared in this environment: '{typeName}'"
        }
    }

    // Make an AnyType representation of this environment for typechecking ObjectNode.
    method asType {
        def envType = AnyType("Environment")
        envType.setupMethods(methods)
        return envType
    }
}



// The base environment/scope of the script with no variables/methods, but can resolve basic types.
class BaseEnvironment {
    def baseTypes = collections.dictionary ["Unknown" :: unknownType, "Done" :: doneType, 
                        "Boolean" :: booleanType, "Number" :: numberType, "String" :: stringType]
    def standardMethods = collections.dictionary ["print(1)" :: NewMethod("print(1)", o1N(unknownType), doneType),
                    "false(0)" :: arglessMeth("false(0)", booleanType), 
                    "true(0)" :: arglessMeth("true(0)", booleanType),
                    "for(1)do(1)" :: NewMethod("for(1)do(1)", c2N(unknownType, unknownType), doneType), 
                    "if(1)then(1)" :: createIfElse(0, false),
                    "if(1)then(1)else(1)" :: createIfElse(0, true),
                    "if(1)then(1)elseif(1)then(1)" :: createIfElse(1, false),
                    "if(1)then(1)elseif(1)then(1)else(1)" :: createIfElse(1, true),
                    "if(1)then(1)elseif(1)then(1)elseif(1)then(1)" :: createIfElse(2, false),
                    "if(1)then(1)elseif(1)then(1)elseif(1)then(1)else(1)" :: createIfElse(2, true)] 
    // TODO - could make generic function if method name starts with "if(1)then(1)" then it looks for 0+ "elseif(1)then(1)"* and optional "else(1)" at end.
    //      -  "for(1)do(1)" Takes a lineup and block. Perhaps those could be specific types instead of unknownType.
    //      -  A block needs to have an apply method. It could be unknown for now, or eventually setup generics and structural typing to work with it.

    // Helper to make standard library if/elseif/else cases.
    method createIfElse(elseifCount : Number, hasElse : Boolean) is private {
        var name := "if(1)then(1)"
        var params := c2N(booleanType, arglessBlockType)
        for (1..elseifCount) do {
            name := name ++ "elseif(1)then(1)"
            // Add the condition and block.
            params.add(booleanType)
            params.add(arglessBlockType)
        }
        if (hasElse) then { 
            name := name ++ "else(1)"
            params.add(arglessBlockType)
        }
        return NewMethod(name, params, unknownType)
    }

    method addMethod(meth) {
        EnvError.raise "Cannot add method '{meth.name}' to base environment"
    }

    method findMethod(name) {
        // Lookup standard library methods.
        if (standardMethods.containsKey(name)) then {
            return standardMethods.at(name)
        }
        EnvError.raise "No method called '{name}' in scope"
    }

    method getReturnType {
        EnvError.raise "Invalid return statement as there is no enclosing method"
    }

    method getDeclaredName {
        EnvError.raise "Invalid declared name as there is no enclosing method"
    }

    // Find a literal type object via the name.
    method findType(expr) {
        var name := expr.name
        // Extracting method name without parameter counts e.g. "foo" not "foo(0)"
        if (name == "lexical request") then {
            name := expr.cleanName
        }
        if (name == "dot request") then {
            EnvError.raise "Cannot resolve dot request in the environment: '{expr.methodName}'"
        }
        // Gets literal for static types (Unknown, Done, Boolean, Number, String).
        if (baseTypes.containsKey(name)) then {
            return baseTypes.at(name)
        }
        EnvError.raise "Unexpected type not present in the environment: '{name}'"
    }
}


//
// #### AST TESTS ####
//

print("\n-----Tests-----")
var testNumber := 1 // Increments after each test.
var succeededTests := 0 // Increments each success to print out of total.

// Default error is TypeError, but specific errors can be checked to ensure the correct node threw the error.
method assertFails(ast) {
    assertFails(ast, TypeError)
}

// Try-catch to run AST then throw a FailedError if it did not throw the expected TypeError (or derivative) exception.
method assertFails(ast, error) {
    try {
        ast.checkType(Environment(BaseEnvironment), unknownType)
        FailedError.raise "No '{error}' thrown"
    } catch { e : error ->
        print "(AF) PASSED: Test{testNumber} successfully threw -> {e}"
        succeededTests := succeededTests + 1
    } catch { e : FailedError ->
        print "(AF) -FAILED-: Test{testNumber} did not throw any error"
    } catch { e -> 
        // Caused by coding mistakes or wrong specific error.
        print "(AF) -FAILED CRITICAL-: Test{testNumber} threw the wrong error of -> {e} -:- instead of -> {error}"
    }
    testNumber := testNumber + 1
}

// Tests that succeed only if no TypeError thrown. Specific error types not needed as none should occur.
method assertPasses(ast) {
    try {
        ast.checkType(Environment(BaseEnvironment), unknownType)
        print "(AP) PASSED: Test{testNumber} did not throw 'TypeError'"
        succeededTests := succeededTests + 1
    } catch { e : TypeError ->
        print "(AP) -FAILED-: Test{testNumber} unexpectedly threw -> {e}"
    } catch { e -> 
        // Unexpected error caused by coding mistakes.
        print "(AP) -FAILED CRITICAL-: Test{testNumber} unexpectedly threw -> {e}"
    }
    testNumber := testNumber + 1
}

// I could eventually make it parse files directly for the tests. Add new tests to the end to not mess up test number order.

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
assertFails(o0C(o1N(d0R(n0M(3),"+(1)",o1N(s0L("hi")),nil)),nil), MethodError)

// TEST 6
// "hi" ++ 3
assertFails(o0C(o1N(d0R(s0L("hi"),"++(1)",o1N(n0M(3)),nil)),nil), MethodError)

// TEST 7
// 3 + 11
assertPasses(o0C(o1N(d0R(n0M(3),"+(1)",o1N(n0M(11)),nil)),nil))

// TEST 8
// var x := 3
// var y : String := x
assertFails(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),v4R("y",o1N(l0R("String(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))),nil), VarError)

// TEST 9
// var x : Boolean := true
// var y : String := x
assertFails(o0C(c2N(v4R("x",o1N(l0R("Boolean(0)",nil,nil)),nil,o1N(l0R("true(0)",nil,nil))),v4R("y",o1N(l0R("String(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))),nil), VarError)

// TEST 10
// var y : String := 3
assertFails(o0C(o1N(v4R("y",o1N(l0R("String(0)",nil,nil)),nil,o1N(n0M(3)))),nil), VarError)

// Undefined variable and method.

// TEST 11
// a
assertFails(o0C(o1N(l0R("a(0)",nil,nil)),nil), EnvError)

// TEST 12
// a.b
assertFails(o0C(o1N(d0R(l0R("a(0)",nil,nil),"b(0)",nil,nil)),nil), EnvError)

// TEST 13
// def x = 3
// x.test
assertFails(o0C(c2N(d3F("x",nil,nil,n0M(3)),d0R(l0R("x(0)",nil,nil),"test(0)",nil,nil)),nil), DotReqError)

// TEST 14
// def x = 3
// 1 + x.test(1)
assertFails(o0C(c2N(d3F("x",nil,nil,n0M(3)),d0R(n0M(1),"+(1)",o1N(d0R(l0R("x(0)",nil,nil),"test(1)",o1N(n0M(1)),nil)),nil)),nil), DotReqError)

// TEST 15
// var x : String := "test"
assertPasses(o0C(o1N(v4R("x",o1N(l0R("String(0)",nil,nil)),nil,o1N(s0L("test")))),nil))

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
assertFails(o0C(o1N(l0R("print(2)",c2N(n0M(3),n0M(3)),nil)),nil), EnvError)

// TEST 20
// print
assertFails(o0C(o1N(l0R("print(0)",nil,nil)),nil), EnvError)

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
assertPasses(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),a5N(l0R("x(0)",nil,nil),n0M(4))),nil))

// TEST 25
// var x := 3
// x := true
assertFails(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),a5N(l0R("x(0)",nil,nil),l0R("true(0)",nil,nil))),nil), MethodError)

// TEST 26
// def x : Boolean = true
assertPasses(o0C(o1N(d3F("x",o1N(l0R("Boolean(0)",nil,nil)),nil,l0R("true(0)",nil,nil))),nil))

// TEST 27
// if (3 == 3) then { print("equal") }
assertPasses(o0C(o1N(l0R("if(1)then(1)",c2N(d0R(n0M(3),"==(1)",o1N(n0M(3)),nil),b1K(nil,o1N(l0R("print(1)",o1N(s0L("equal")),nil)))),nil)),nil))

// TEST 28
// if ("hi") then {}
assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(s0L("hi"),b1K(nil,nil)),nil)),nil), MethodError)

// TEST 29
// if (7) then {}
assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(n0M(7),b1K(nil,nil)),nil)),nil), MethodError)

// TEST 30
// if (true) then {}
assertPasses(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(nil,nil)),nil)),nil))

// TEST 31
// if (true) then {}
assertPasses(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(nil,nil)),nil)),nil))

// If statements don't take an input in their block.
// TEST 32
// if (true) then { b -> 1 }
assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(o1N(i0D("b",nil)),o1N(n0M(1)))),nil)),nil))

// TEST 33
// if (true) then { b : Number -> 1 }
assertFails(o0C(o1N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(o1N(i0D("b",o1N(l0R("Number(0)",nil,nil)))),o1N(n0M(1)))),nil)),nil))

// TEST 34
// method test {}
assertPasses(o0C(o1N(m0D(o1N(p0T("test",nil,nil)),nil,nil,nil)),nil))

// TEST 35
// method test(x : String, y: Number) when(z : Boolean) {}
assertPasses(o0C(o1N(m0D(c2N(p0T("test",c2N(i0D("x",o1N(l0R("String(0)",nil,nil))),i0D("y",o1N(l0R("Number(0)",nil,nil)))),nil),p0T("when",o1N(i0D("z",o1N(l0R("Boolean(0)",nil,nil)))),nil)),nil,nil,nil)),nil))

// TEST 36
// method test(x : String, y: Number) when(z : Boolean) { "x: {x}, y: {y}, z: {z}" }
assertPasses(o0C(o1N(m0D(c2N(p0T("test",c2N(i0D("x",o1N(l0R("String(0)",nil,nil))),i0D("y",o1N(l0R("Number(0)",nil,nil)))),nil),p0T("when",o1N(i0D("z",o1N(l0R("Boolean(0)",nil,nil)))),nil)),nil,nil,o1N(i0S("x: ",l0R("x(0)",nil,nil),i0S(", y: ",l0R("y(0)",nil,nil),i0S(", z: ",l0R("z(0)",nil,nil),s0L(""))))))),nil))

// TEST 37
// method test(x : String, y: Number) when(z : Boolean) { x+y+z }
assertFails(o0C(o1N(m0D(c2N(p0T("test",c2N(i0D("x",o1N(l0R("String(0)",nil,nil))),i0D("y",o1N(l0R("Number(0)",nil,nil)))),nil),p0T("when",o1N(i0D("z",o1N(l0R("Boolean(0)",nil,nil)))),nil)),nil,nil,o1N(d0R(d0R(l0R("x(0)",nil,nil),"+(1)",o1N(l0R("y(0)",nil,nil)),nil),"+(1)",o1N(l0R("z(0)",nil,nil)),nil)))),nil), DotReqError)

// TEST 38
// method test(x : String, y: Number) { x + y }
assertFails(o0C(o1N(m0D(o1N(p0T("test",c2N(i0D("x",o1N(l0R("String(0)",nil,nil))),i0D("y",o1N(l0R("Number(0)",nil,nil)))),nil)),nil,nil,o1N(d0R(l0R("x(0)",nil,nil),"+(1)",o1N(l0R("y(0)",nil,nil)),nil)))),nil), DotReqError)

// TEST 39
// method test(x : String, y: Number) { print(x)
//    print(y) }
assertPasses(o0C(o1N(m0D(o1N(p0T("test",c2N(i0D("x",o1N(l0R("String(0)",nil,nil))),i0D("y",o1N(l0R("Number(0)",nil,nil)))),nil)),nil,nil,c2N(l0R("print(1)",o1N(l0R("x(0)",nil,nil)),nil),l0R("print(1)",o1N(l0R("y(0)",nil,nil)),nil)))),nil))

// TEST 40
// method test(x : String, y: Number) { x ++ " y {y}" }
assertPasses(o0C(o1N(m0D(o1N(p0T("test",c2N(i0D("x",o1N(l0R("String(0)",nil,nil))),i0D("y",o1N(l0R("Number(0)",nil,nil)))),nil)),nil,nil,o1N(d0R(l0R("x(0)",nil,nil),"++(1)",o1N(i0S(" y ",l0R("y(0)",nil,nil),s0L(""))),nil)))),nil))

// TEST 41
// method test -> String { return "Test" }
assertPasses(o0C(o1N(m0D(o1N(p0T("test",nil,nil)),o1N(l0R("String(0)",nil,nil)),nil,o1N(r3T(s0L("Test"))))),nil))

// TEST 42
// method test -> String { return 4 }
assertFails(o0C(o1N(m0D(o1N(p0T("test",nil,nil)),o1N(l0R("String(0)",nil,nil)),nil,o1N(r3T(n0M(4))))),nil), ReturnError)

// TEST 43 (implicit return)
// method test -> String { 4 }
assertFails(o0C(o1N(m0D(o1N(p0T("test",nil,nil)),o1N(l0R("String(0)",nil,nil)),nil,o1N(n0M(4)))),nil), ReturnError)

// TEST 44
// method test -> String { 4
//    "returned" }
// def x : String = test
assertPasses(o0C(c2N(m0D(o1N(p0T("test",nil,nil)),o1N(l0R("String(0)",nil,nil)),nil,c2N(n0M(4),s0L("returned"))),d3F("x",o1N(l0R("String(0)",nil,nil)),nil,l0R("test(0)",nil,nil))),nil))

// TEST 45
// "\\$\"\n\r\{*~`^@%#!" ++ "test"
assertPasses(o0C(o1N(d0R(s0L(s4F("", c9B, s4F("", c9D, s4F("",c9Q,s4F("",c9N,s4F("",c9R,s4F("",c9L,s4F("",c9S,s4F("",c9T,s4F("",c9G,s4F("",c9C,s4F("",c9A,s4F("",c9P,s4F("",c9H,s4F("",c9E,""))))))))))))))),"++(1)",o1N(s0L("test")),nil)),nil))

// TEST 46
// def x = { a -> 1 }
assertPasses(o0C(o1N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(n0M(1))))),nil))

// TEST 47
// def x = { a -> a + 1 }
// x.apply(1)
assertPasses(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(n0M(1)),nil)))),d0R(l0R("x(0)",nil,nil),"apply(1)",o1N(n0M(1)),nil)),nil))

// TEST 48
// def x = { a -> a + 1 }
// x.apply("test")
assertFails(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(n0M(1)),nil)))),d0R(l0R("x(0)",nil,nil),"apply(1)",o1N(s0L("test")),nil)),nil))

// TEST 49
// def x : String = {a -> a}
assertFails(o0C(o1N(d3F("x",o1N(l0R("String(0)",nil,nil)),nil,b1K(o1N(i0D("a",nil)),o1N(l0R("a(0)",nil,nil))))),nil), DefError)

// TEST 50
// def x = {a -> a} 
// def y : String = x
assertFails(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(l0R("a(0)",nil,nil)))),d3F("y",o1N(l0R("String(0)",nil,nil)),nil,l0R("x(0)",nil,nil))),nil), DefError)

// TEST 51
// def x = {a : Number -> a + "Test"}
assertFails(o0C(o1N(d3F("x",nil,nil,b1K(o1N(i0D("a",o1N(l0R("Number(0)",nil,nil)))),o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(s0L("Test")),nil))))),nil), MethodError)

// TEST 52
// def x = {a : Number -> a ++ "Test"}
assertFails(o0C(o1N(d3F("x",nil,nil,b1K(o1N(i0D("a",o1N(l0R("Number(0)",nil,nil)))),o1N(d0R(l0R("a(0)",nil,nil),"++(1)",o1N(s0L("Test")),nil))))),nil), DotReqError)

// TEST 53
//type A = interface {}
//type B = interface {}
//var x : A
//var y : B := x
assertPasses(o0C(c0N(t0D("A",nil,i0C(nil)),c0N(t0D("B",nil,i0C(nil)),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil))

// TEST 54 (Inductive)
// type A = interface { foo -> A }
assertPasses(o0C(o1N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil))))))),nil))

// TEST 55 (Implemented)
// type A = interface { foo -> A }
// class Aclass { method foo -> A { return self } }
// def x : A = Aclass
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil)))))),c2N(m0D(o1N(p0T("Aclass",nil,nil)),nil,nil,o1N(o0C(o1N(m0D(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil)),nil,o1N(r3T(l0R("self(0)",nil,nil))))),nil))),d3F("x",o1N(l0R("A(0)",nil,nil)),nil,l0R("Aclass(0)",nil,nil)))),nil))

// TEST 56 (Coinductive loops)
// type A = interface { foo -> A }
// type B = interface { foo -> B }
// var x : A
// var y : B := x
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil))

// TEST 57 (Coinductive different)
// class Test {
//     type A = interface { foo -> B }
//     type B = interface { foo -> A }
//     var x : A
//     var y : B := x
// }
assertPasses(o0C(o1N(m0D(o1N(p0T("Test",nil,nil)),nil,nil,o1N(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil)))),nil))

// TEST 58
// type A = interface { foo -> A }
// type B = interface { foo -> String }
// var x : A
// var y : B := x
assertFails(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("String(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil))

// TEST 59 (Indirect)
// type A = interface { foo -> String }
// type B = interface { foo -> String }
// type C = interface { foo(_ : A) -> A }
// type D = interface { foo(_ : B) -> B }
// var x : C
// var y : D := x
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("String(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("String(0)",nil,nil)))))),c0N(t0D("C",nil,i0C(o1N(m0S(o1N(p0T("foo",o1N(i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),o1N(l0R("A(0)",nil,nil)))))),c0N(t0D("D",nil,i0C(o1N(m0S(o1N(p0T("foo",o1N(i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("C(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("D(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))))),nil))

// Test 60 (Not structurally equivalent)
// type A = interface { foo -> String }
// type B = interface { foo -> C }
// type C = interface { foo -> String }
// var a : A
// def b : B = a
assertFails(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("String(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("C(0)",nil,nil)))))),c0N(t0D("C",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("String(0)",nil,nil)))))),c2N(v4R("a",o1N(l0R("A(0)",nil,nil)),nil,nil),d3F("b",o1N(l0R("B(0)",nil,nil)),nil,l0R("a(0)",nil,nil)))))),nil))

// Test 61 (Coinductive structurally equivalent, both infinite foo dependencies but A -> A... vs B -> C -> D -> B...)
// type A = interface { foo -> A }
// type B = interface { foo -> C }
// type C = interface { foo -> D }
// type D = interface { foo -> B }
// var a : A
// def b : B = a
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("C(0)",nil,nil)))))),c0N(t0D("C",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("D(0)",nil,nil)))))),c0N(t0D("D",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("a",o1N(l0R("A(0)",nil,nil)),nil,nil),d3F("b",o1N(l0R("B(0)",nil,nil)),nil,l0R("a(0)",nil,nil))))))),nil))

// TEST 62 (Refined subtype) TODO gives dot request error because '|' operator does not exist.
// type A = interface { foo -> A }
// type B = interface { foo -> B }
// type X = interface { bar(_ : String) }
// type X2 = interface { bar(_ : String) -> String | A }
// type Y = interface { bar(_ : String | A) }
// type Z = interface { bar(_ : String | B) } // B is equivalent to A.
// var x2 : X2
// var y : Y
// var z : Z
// def test1 : X = x2 // X2 subtypes X, but not vice versa because explicit return type.
// def test2 : X = y
// def test3 : X = z
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("A(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("X",nil,i0C(o1N(m0S(o1N(p0T("bar",o1N(i0D("_",o1N(l0R("String(0)",nil,nil)))),nil)),nil)))),c0N(t0D("X2",nil,i0C(o1N(m0S(o1N(p0T("bar",o1N(i0D("_",o1N(l0R("String(0)",nil,nil)))),nil)),o1N(d0R(l0R("String(0)",nil,nil),"|(1)",o1N(l0R("A(0)",nil,nil)),nil)))))),c0N(t0D("Y",nil,i0C(o1N(m0S(o1N(p0T("bar",o1N(i0D("_",o1N(d0R(l0R("String(0)",nil,nil),"|(1)",o1N(l0R("A(0)",nil,nil)),nil)))),nil)),nil)))),c0N(t0D("Z",nil,i0C(o1N(m0S(o1N(p0T("bar",o1N(i0D("_",o1N(d0R(l0R("String(0)",nil,nil),"|(1)",o1N(l0R("B(0)",nil,nil)),nil)))),nil)),nil)))),c0N(c0M(" B is equivalent to A."),c0N(v4R("x2",o1N(l0R("X2(0)",nil,nil)),nil,nil),c0N(v4R("y",o1N(l0R("Y(0)",nil,nil)),nil,nil),c0N(v4R("z",o1N(l0R("Z(0)",nil,nil)),nil,nil),c0N(d3F("test1",o1N(l0R("X(0)",nil,nil)),nil,l0R("x2(0)",nil,nil)),c0N(c0M(" X2 subtypes X, but not vice versa because explicit return type."),c2N(d3F("test2",o1N(l0R("X(0)",nil,nil)),nil,l0R("y(0)",nil,nil)),d3F("test3",o1N(l0R("X(0)",nil,nil)),nil,l0R("z(0)",nil,nil))))))))))))))),nil))

// TEST 63
// type A = interface {}
// type B = A
// var x : A := 1
// var y : B := x
assertPasses(o0C(c0N(t0D("A",nil,i0C(nil)),c0N(t0D("B",nil,l0R("A(0)",nil,nil)),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,o1N(n0M(1))),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil))

// TEST 64 (nested dot requests on unknown import type).
// import "test" as test
// test.y.z
assertPasses(o0C(c2N(i0M("test",i0D("test",nil)),d0R(d0R(l0R("test(0)",nil,nil),"y(0)",nil,nil),"z(0)",nil,nil)),nil))

// TEST 65
// import "test" as test
// def x : String = test.x(1).y(1) z("a", "b")
assertPasses(o0C(c2N(i0M("test",i0D("test",nil)),d3F("x",o1N(l0R("String(0)",nil,nil)),nil,d0R(d0R(l0R("test(0)",nil,nil),"x(1)",o1N(n0M(1)),nil),"y(1)z(2)",c0N(n0M(1),c2N(s0L("a"),s0L("b"))),nil))),nil))

// TEST 66
// var x : Done := print(1)
assertPasses(o0C(o1N(v4R("x",o1N(l0R("Done(0)",nil,nil)),nil,o1N(l0R("print(1)",o1N(n0M(1)),nil)))),nil))

// TEST 67
// var x := 3
// x := 7
assertPasses(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),a5N(l0R("x(0)",nil,nil),n0M(7))),nil))

// TEST 68
// var x := 3
// x := "Test"
assertFails(o0C(c2N(v4R("x",nil,nil,o1N(n0M(3))),a5N(l0R("x(0)",nil,nil),s0L("Test"))),nil), MethodError)

// TEST 69
// var x : String := "Test String"
// x := 7
assertFails(o0C(c2N(v4R("x",o1N(l0R("String(0)",nil,nil)),nil,o1N(s0L("Test String"))),a5N(l0R("x(0)",nil,nil),l0R("false(0)",nil,nil))),nil), MethodError)

// TEST 70 (Unreachable code)
// method test { 
//     return 4
//     5
// }
assertFails(o0C(o1N(m0D(o1N(p0T("test",nil,nil)),nil,nil,c2N(r3T(n0M(4)),n0M(5)))),nil), ReturnError)

// TEST 71 (False negative, unreachable past unconditional block)
// method test {
//     if (true) then { return 4 }
//     5
// } 
assertPasses(o0C(o1N(m0D(o1N(p0T("test",nil,nil)),nil,nil,c2N(l0R("if(1)then(1)",c2N(l0R("true(0)",nil,nil),b1K(nil,o1N(r3T(n0M(4))))),nil),n0M(5)))),nil))

// TEST 72
// def y = "hi"
// def x : Number = y ++ "bye"
assertFails(o0C(c2N(d3F("y",nil,nil,s0L("hi")),d3F("x",o1N(l0R("Number(0)",nil,nil)),nil,d0R(l0R("y(0)",nil,nil),"++(1)",o1N(s0L("bye")),nil))),nil), DefError)

// TEST 73 (Should fail because they both make a method with the same name)
// def Test = object {}
// class Test {}
assertFails(o0C(c2N(d3F("Test",nil,nil,o0C(nil,nil)),m0D(o1N(p0T("Test",nil,nil)),nil,nil,o1N(o0C(nil,nil)))),nil), EnvError)

// TEST 74 (Non-terminating D)
// type A = interface { foo (_ : C) -> C }
// type B = interface { foo (_ : D) -> D }
// type C = interface { foo -> String }
// type D = interface { foo -> D }
// var a : A
// def b : B = a
assertFails(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",o1N(i0D("_",o1N(l0R("C(0)",nil,nil)))),nil)),o1N(l0R("C(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",o1N(i0D("_",o1N(l0R("D(0)",nil,nil)))),nil)),o1N(l0R("D(0)",nil,nil)))))),c0N(t0D("C",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("String(0)",nil,nil)))))),c0N(t0D("D",nil,i0C(o1N(m0S(o1N(p0T("foo",nil,nil)),o1N(l0R("D(0)",nil,nil)))))),c2N(v4R("a",o1N(l0R("A(0)",nil,nil)),nil,nil),d3F("b",o1N(l0R("B(0)",nil,nil)),nil,l0R("a(0)",nil,nil))))))),nil), DefError)

// TEST 75 (Valid multi param methods) The next few tests check all possible permutations of this.
// type A = interface { foo (_ : String, _ : B) -> B }
// type B = interface { foo (_ : String, _ : A) -> B }
// var x : A
// var y : B := x
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil))

// TEST 76 (Non-terminating param compared to terminating)
// type A = interface { foo (_ : String, _ : B) -> B }
// type B = interface { foo (_ : A, _ : A) -> B }
// var x : A
// var y : B := x
assertFails(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("A(0)",nil,nil))),i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil), VarError)

// TEST 77 (Different terminating type)
// type A = interface { foo (_ : String, _ : B) -> B }
// type B = interface { foo (_ : Number, _ : A) -> B }
// var x : A
// var y : B := x
assertFails(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("Number(0)",nil,nil))),i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil), VarError)

// TEST 78 (Different method name)
// type A = interface { foo (_ : String, _ : B) -> B }
// type B = interface { bar (_ : String, _ : A) -> B }
// var x : A
// var y : B := x
assertFails(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("bar",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil), VarError)

// TEST 79 (Unknown return)
// type A = interface { foo (_ : String, _ : B) -> B }
// type B = interface { foo (_ : String, _ : A) }
// var x : A
// var y : B := x
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),nil)))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil))

// TEST 80 (Unknown params, subtype works in both directions)
// type A = interface { foo (_ : String, _ : B) -> B }
// type B = interface { foo (_, _) }
// var x : A
// var y : B := x
// var x2 : B
// var y2 : A := x2
assertPasses(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",nil),i0D("_",nil)),nil)),nil)))),c0N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),c0N(v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil))),c2N(v4R("x2",o1N(l0R("B(0)",nil,nil)),nil,nil),v4R("y2",o1N(l0R("A(0)",nil,nil)),nil,o1N(l0R("x2(0)",nil,nil)))))))),nil))

// TEST 81 (Different number of params)
// type A = interface { foo (_ : String, _ : B) -> B }
// type B = interface { foo (_ : String) -> B }
// var x : A
// var y : B := x
assertFails(o0C(c0N(t0D("A",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",o1N(i0D("_",o1N(l0R("String(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil), VarError)


// TEST 82 (Multiple methods success)
// type A = interface { 
//     foo (_ : String, _ : B) -> B
//     bar (_ : String) -> A
// }
// type B = interface { 
//     foo (_ : String, _ : A) -> B 
//     bar (_ : String) -> B
// }
// var x : A
// var y : B := x
assertPasses(o0C(c0N(t0D("A",nil,i0C(c2N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil))),m0S(o1N(p0T("bar",o1N(i0D("_",o1N(l0R("String(0)",nil,nil)))),nil)),o1N(l0R("A(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(c2N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil))),m0S(o1N(p0T("bar",o1N(i0D("_",o1N(l0R("String(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil))

// TEST 83 (Multiple methods failure)
// type A = interface { 
//     foo (_ : String, _ : B) -> B
//     bar (_ : String) -> A
// }
// type B = interface { foo (_ : String, _ : A) -> B }
// var x : A
// var y : B := x
assertFails(o0C(c0N(t0D("A",nil,i0C(c2N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("B(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil))),m0S(o1N(p0T("bar",o1N(i0D("_",o1N(l0R("String(0)",nil,nil)))),nil)),o1N(l0R("A(0)",nil,nil)))))),c0N(t0D("B",nil,i0C(o1N(m0S(o1N(p0T("foo",c2N(i0D("_",o1N(l0R("String(0)",nil,nil))),i0D("_",o1N(l0R("A(0)",nil,nil)))),nil)),o1N(l0R("B(0)",nil,nil)))))),c2N(v4R("x",o1N(l0R("A(0)",nil,nil)),nil,nil),v4R("y",o1N(l0R("B(0)",nil,nil)),nil,o1N(l0R("x(0)",nil,nil)))))),nil), VarError)

// TEST 84 (vartiant, union, intersection)
// type A = String & Number
// type B = String + Number
// var x : String | Number
// var y : String & Number
// var z : String + Number
assertPasses(o0C(c0N(t0D("A",nil,d0R(l0R("String(0)",nil,nil),"&(1)",o1N(l0R("Number(0)",nil,nil)),nil)),c0N(t0D("B",nil,d0R(l0R("String(0)",nil,nil),"+(1)",o1N(l0R("Number(0)",nil,nil)),nil)),c0N(v4R("x",o1N(d0R(l0R("String(0)",nil,nil),"|(1)",o1N(l0R("Number(0)",nil,nil)),nil)),nil,nil),c2N(v4R("y",o1N(d0R(l0R("String(0)",nil,nil),"&(1)",o1N(l0R("Number(0)",nil,nil)),nil)),nil,nil),v4R("z",o1N(d0R(l0R("String(0)",nil,nil),"+(1)",o1N(l0R("Number(0)",nil,nil)),nil)),nil,nil))))),nil))

// TEST 85 (Cannot use variant in type declaration)
// type A = String | Number
assertFails(o0C(o1N(t0D("A",nil,d0R(l0R("String(0)",nil,nil),"|(1)",o1N(l0R("Number(0)",nil,nil)),nil))),nil), EnvError)

// TEST 86 (Now uses value type after reassignment even if no declared type or intiial value)
// var test
// test := 3
// def z : String = test
assertFails(o0C(c0N(v4R("test",nil,nil,nil),c2N(a5N(l0R("test(0)",nil,nil),n0M(3)),d3F("z",o1N(l0R("String(0)",nil,nil)),nil,l0R("test(0)",nil,nil)))),nil), DefError)

// TEST 87 (Same as previous, but with dot requests and var z)
// class test { var y }
// def x = test
// x.y := 3
// var z : String := x.y
assertFails(o0C(c0N(m0D(o1N(p0T("test",nil,nil)),nil,nil,o1N(o0C(o1N(v4R("y",nil,nil,nil)),nil))),c0N(d3F("x",nil,nil,l0R("test(0)",nil,nil)),c2N(a5N(d0R(l0R("x(0)",nil,nil),"y(0)",nil,nil),n0M(3)),v4R("z",o1N(l0R("String(0)",nil,nil)),nil,o1N(d0R(l0R("x(0)",nil,nil),"y(0)",nil,nil)))))),nil), VarError)



// TEST ? put after lineups.
// def x = { a -> a + 1 }
// [1, 2, 3].map { e -> x.apply(e) }
// assertPasses(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(n0M(1)),nil)))),d0R(l0N(c0N(n0M(1),c2N(n0M(2),n0M(3)))),"map(1)",o1N(b1K(o1N(i0D("e",nil)),o1N(d0R(l0R("x(0)",nil,nil),"apply(1)",o1N(l0R("e(0)",nil,nil)),nil)))),nil)),nil))
// TEST ? put after lineups.
// def x = { a -> a + 1 }
// ["test", 2, "yes"].map { e -> x.apply(e) }
// assertFails(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(n0M(1)),nil)))),d0R(l0N(c0N(s0L("test"),c2N(n0M(2),s0L("yes")))),"map(1)",o1N(b1K(o1N(i0D("e",nil)),o1N(d0R(l0R("x(0)",nil,nil),"apply(1)",o1N(l0R("e(0)",nil,nil)),nil)))),nil)),nil))

// TEST ?
// def x = { a -> print(a + 1) }
// [1, 2, 3].do { e -> x.apply(e) }
// assertPasses(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(l0R("print(1)",o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(n0M(1)),nil)),nil)))),d0R(l0N(c0N(n0M(1),c2N(n0M(2),n0M(3)))),"do(1)",o1N(b1K(o1N(i0D("e",nil)),o1N(d0R(l0R("x(0)",nil,nil),"apply(1)",o1N(l0R("e(0)",nil,nil)),nil)))),nil)),nil))
// TEST ?
// def x = { a -> print(a + 1) }
// ["test", 2, "yes"].do { e -> x.apply(e) }
// assertFails(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(l0R("print(1)",o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(n0M(1)),nil)),nil)))),d0R(l0N(c0N(s0L("test"),c2N(n0M(2),s0L("yes")))),"do(1)",o1N(b1K(o1N(i0D("e",nil)),o1N(d0R(l0R("x(0)",nil,nil),"apply(1)",o1N(l0R("e(0)",nil,nil)),nil)))),nil)),nil))


// TEST ?
// def x = { a -> a + 1 }
// x.apply(["test", "str"])
// assertFails(o0C(c2N(d3F("x",nil,nil,b1K(o1N(i0D("a",nil)),o1N(d0R(l0R("a(0)",nil,nil),"+(1)",o1N(n0M(1)),nil)))),d0R(l0R("x(0)",nil,nil),"apply(1)",o1N(l0N(c2N(s0L("test"),s0L("str")))),nil)),nil))


// Complex test in file: sample.grace
// assertPasses(o0C(c0N(i0M("ast",i0D("ast",nil)),c0N(c0M(" This file makes use of many AST nodes"),c2N(d3F("x",nil,nil,o0C(c2N(v4R("y",o1N(l0R("Number(0)",nil,nil)),nil,o1N(n0M(1))),m0D(c2N(p0T("foo",o1N(i0D("arg",o1N(l0R("Action(0)",nil,nil)))),nil),p0T("bar",o1N(i0D("n",nil)),nil)),o1N(l0R("String(0)",nil,nil)),nil,c2N(a5N(d0R(l0R("self(0)",nil,nil),"y(0)",nil,nil),d0R(d0R(l0R("arg(0)",nil,nil),"apply(0)",nil,nil),"+(1)",o1N(l0R("n(0)",nil,nil)),nil)),r3T(i0S(s4F("y ",c9A," "),l0R("y(0)",nil,nil),s0L(s4F("",c9E,""))))))),nil)),l0R("print(1)",o1N(d0R(l0R("x(0)",nil,nil),"foo(1)bar(1)",c2N(b1K(nil,o1N(n0M(2))),n0M(3)),nil)),nil)))),nil))

// TODO make more longer tests.


// Print summary
def totalTests = testNumber - 1
print "Tests passed: {succeededTests}/{totalTests}"