// java -cp wg/java/ nz.mwh.wg.Start TESTING.grace
// EVERYTHING THAT DOESN'T WORK WITH MY CURRENT PARSER HAS BEEN COMMENTED.
// Source: http://www.cs.pdx.edu/~grace/


// The class keyword is just a method that creates and object, so it can have multiple parts.
class A(a) B(b) {}

// The object keyword is for a singleton (constant, unchanging) object.
def C = object {}

// import "io" as transput     // import module — the imported object is named transput
//transput.output.write "Hello World!\n" // request method on imported module

// Comments to end of line 

// Definitions and Variables
def one = 1 // constant
def two:Number = 2  // constant with type annotation

var i:Number := 13  // variable with type - note := 
var x := 4 // variable, dynamically typed
var z is readable, writable := "Z" // annotated to give public access (from another file, public get: readable, public set: writable)
// This is for the AST anns I need to investigate this further.

//Literals
1
//16xF00F00
//2x10110100
//0xdeadbeef // Radix zero treated as 16 
true
false      // Booleans
"Hello World!"
"fruit\tcost"       // string with escape for tab 
"1 + 2 = {1 + 2}"   // string with interpolation
{two.something}     // block (lambda expression) without parameters
{ j -> print(j)}    // blocks with parameters

// Requests
self.z   // explicit self request, without arguments
z // implicit self-request, without arguments
print "Hello world" // implicit self-request, single string argument
"Hello".size // request of method size with string "Hello" as receiver
"abcdefghi".substringFrom 3 to 6 
    // request of method substringFrom(_)to(_); parens optional on literal arguments
1 + (2 * 3) // operators are also requests
! false // unary prefix operators!   Can't do that in Smalltalk.

"ab" ++ "cd" // operator ++ for string concatenation
1..10        // operator .. constructs a range of numbers
(true || false) && true // only + - * and / have precedence
x := 24 // assignment request


// Control Structures — block bodies are indented when the
// block spans multiple lines
if (x == 22) then {
    print "YES"
} elseif {x == 23} then {
    print "Maybe"
} else {
    print "...nope..."
}
// It appears that grace (or more likely the provided parser) doesn't support 3 elseif in a row so you have to do else { if ()... } and nest deeper.


for (2 .. 4) do {
    j -> print(j)
} // prints 2, 3, and 4

x := 10
while { x < 20 } do {
    print(x)
    x := x + 3
} 

//match (x) // match(_)case(_)... can match on both values and types
//  case { 0 -> print "zero" }   // literal — matches when x == 0 
//  case { n : Number -> print "Number {n}" }  // type matches 
//  case { s : String -> print "String {s}" }
//  else { print "who knows?" }     // all other cases 
 
 
// Methods
// Grace methods can be at the "top level"
method pi {3.141592634} //simple method 
method add (other) { other + self } // binary operator 
method prefix- {print "bing!"} //prefix unary operator
method from(n : Number) steps(s: Number) -> Number { 
    // method with multi-part name, each part with an argument list
    print "from {n} steps {s}"
    return s
} 
method fromsteps(n: Number, s: Number) -> Number { 
    // method with multiple arguments 
    print "from {n} steps {s}"
    return s  } 
 
// Objects
def fergus = object {  // make a new object 
    def colour is readable = "Tabby" 
    def name is readable = "Fergus"
    var miceEaten := 0
    method eatMouse { miceEaten := miceEaten + 1 }
    method miaow { print "{name}({colour}) has eaten {miceEaten} mice" }
}
 
fergus.eatMouse
fergus.eatMouse
fergus.miaow
 
// Classes
class cat(name') colour(colour') {   // class is a factory method
    def colour is readable = colour' // note primes on names
    def name is readable = name'
    var miceEaten := 0
    method eatMouse {miceEaten := miceEaten + 1}
    method miaow {
        print "{name}({colour}) has eaten {miceEaten} mice"
    }
}
(cat "Amelia" colour "Tortoiseshell").miaow
 
// Inheritance
class pedigreeCat(aName) colour(aColour) {  
    //inherit cat(aName) colour("Pedigree " ++ aColour)
        // call superclass's factory
        //alias catMiaow = miaow
    var prizes := 0    // initialize an instance variable
    method winner {prizes := prizes + 1}
    //method miaow is override {
    //    catMiaow
    //    print "and won {prizes} prizes"
    //}
}
 
def woopert = pedigreeCat "Woopert" colour "Siamese"
woopert.winner
woopert.winner
woopert.winner
//woopert.miaow
 
 
// Exceptions (make a new kind of exception)
def ProgrammingError = Exception.refine "Programming Error"
def NegativeError = ProgrammingError.refine "Negative Error"  


// Catch exceptions.
try {
    NegativeError.raise "-1 < 0"  // raise (throw) an exception
} catch { e: ProgrammingError -> 
    print "Exception -> {e}"
} catch { e: Exception -> 
    print "An unexpected exception -> {e}" 
} //finally {} // Finally appears to not work. You could do a catchAll instead (e: Exception) and make something happen in each of try and catchAll.

// Lineup
[ print 1, print 2, print 3 ]


//type A = interface {
//    foo -> Number
//}

//type B = interface {
//    bar -> String
//}

// There is A | B not A & B that has to be done manually like this:
//type AandB = interface {
//    foo -> Number
//    bar -> String
//}

var x : AandB := object {
    method foo { 1 }
    method bar { "hi" }
}

// Wouldn't work.
var x : A | B := object {
    method foo { 1 }
    method bar { "hi" }
}
