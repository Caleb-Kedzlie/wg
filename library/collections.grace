
def OutOfRange = Exception.refine "OutOfRange"

// Usage:
// list [1,2,3]    -- creates a list with initial elements 1, 2, and 3
// l.at 3          -- returns the element at index 3 (1-based)
// l.add 4         -- adds 4 to the end of the list
// l.add "X" at 2  -- adds "X" at index 2, moving others
// list(iterable)  -- copies elements from iterable into a new list
class list(elems) {
    var arraySize := if (elems.size > 5) then { elems.size * 2} else { 10 }
    var elements := primitiveArray.new(arraySize)
    var itemSize := 0
    for (elems) do { elem ->
        elements.at(itemSize) put(elem)
        itemSize := itemSize + 1
    }

    method size { itemSize }

    method at(index) {
        if ((index < 1) || (index > itemSize)) then {
            OutOfRange.raise "List index must be in range 1..{itemSize}, not {index}"
        }
        elements.at(index - 1)
    }

    method at(index) put(value) {
        if ((index < 1) || (index > itemSize)) then {
            OutOfRange.raise "List index must be in range 1..{itemSize}, not {index}"
        }
        ensureCapacity(index)
        elements.at(index - 1) put(value)
    }

    method add(value) {
        ensureCapacity(itemSize + 1)
        elements.at(itemSize) put(value)
        itemSize := itemSize + 1
    }

    method add(value) at(index) {
        ensureCapacity(itemSize + 1)
        for (itemSize..index..(-1)) do { i ->
            elements.at(i) put(elements.at(i - 1))
        }
        itemSize := itemSize + 1
        elements.at(index - 1) put(value)
    }

    method removeAt(index) {
        if ((index < 1) || (index > itemSize)) then {
            OutOfRange.raise "List index must be in range 1..{itemSize}, not {index}"
        }
        var removed := elements.at(index - 1)
        for (index..(itemSize - 1)) do { i ->
            elements.at(i - 1) put(elements.at(i))
        }
        itemSize := itemSize - 1
        removed
    }

    method do(blk) {
        for (0..(itemSize - 1)) do { i ->
            blk.apply(elements.at(i))
        }
    }

    method asString {
        var result := "list ["
        var later := false
        self.do { elem ->
            if (later) then {
                result := result ++ ", "
            }
            result := result ++ elem
            later := true
        }
        result ++ "]"
    }

    method ensureCapacity(sz) {
        if (sz >= arraySize) then {
            def newSize = arraySize * 2
            def newElements = primitiveArray.new(newSize)
            for (0..(itemSize - 1)) do { i ->
                newElements.at(i) put(elements.at(i))
            }
            elements := newElements
            arraySize := newSize
        }
    }

    method reversed {
        var result := list []
        for (itemSize..1..(-1)) do { i ->
            result.add(elements.at(i - 1))
        }
        result
    }

    method reversed(_) {
        self.reversed
    }

    method map(transform) {
        var result := list []
        do { elem ->
            result.add(transform.apply(elem))
        }
        result
    }

    method map(transform) combine(combiner) {
        if (itemSize == 0) then {
            return list []
        }
        var result := transform.apply(elements.at(0))
        for (1..(itemSize - 1)) do { i ->
            result := combiner.apply(result, transform.apply(elements.at(i)))
        }
        result
    }

    method zip(otherList) do(block) {
        if (size != otherList.size) then {
            Exception.raise("Cannot zip lists of different sizes: " ++ size ++ " and " ++ otherList.size)
        }
        var index := 0
        otherList.do { elem ->
            block.apply(elements.at(index), elem)
            index := index + 1

        }
    }

    method first {
        if (itemSize == 0) then {
            Exception.raise("List is empty")
        }
        elements.at(0)
    }

    method flatMap(transform) {
        var result := list []
        do { elem ->
            var transformed := transform.apply(elem)
            transformed.do { item ->
                result.add(item)
            }
        }
        result
    }

    method join(sep) {
        var result := ""
        var later := false
        do { elem ->
            if (later) then { result := result ++ sep }
            later := true
            result := result ++ elem.asString
        }
        result
    }

    method contains(pred) {
        do { elem ->
            if (pred.apply(elem)) then { return true }
        }
        return false
    }

    method without(pred) {
        var result := list []
        do { elem ->
            if (!pred.apply(elem)) then { result.add(elem) }
        }
        result
    }

}


class binding(k, v) {
    def key is public = k
    def value is public = v

    method asString {
        "{key.asDebugString}::{value.asDebugString}"
    }
}

// Usage:
// dictionary ["key1" :: value1, "key2" :: value2]  -- creates a dictionary with initial key-value pairs
// d.at "key1"                                      -- returns the value for "key1"
// d.at "key3" put(value3)                          -- sets the value for "key3" to value3
class dictionary(elems) {
    var arraySize := if (elems.size > 5) then { elems.size * 3} else { 16 }
    var elements := primitiveArray.new(arraySize)
    var itemSize := 0
    def MAX_FILL = 2 / 3

    for (0..(arraySize-1)) do { i ->
        elements.at(i) put(vacant)
    }

    for (elems) do { elem ->
        self.at(elem.key) put(elem.value)
    }

    method size { itemSize }

    method at(key) put(value) {
        var index := findIndex(key)
        def cur = elements.at(index)
        if ((cur == vacant) || (cur == tombstone)) then {
            itemSize := itemSize + 1
            ensureCapacity(itemSize)
            index := findIndex(key)
        }
        elements.at(index) put(binding(key, value))
        self
    }

    method at(key) {
        def index = findIndex(key)
        if (elements.at(index).key == key) then {
            elements.at(index).value
        } else {
            OutOfRange.raise "key not found in dictionary: {key}"
        }
    }

    method containsKey(key) {
        def index = findIndex(key)
        elements.at(index).key == key
    }

    method asString {
        var result := "dictionary ["
        var later := false
        for (0..(arraySize - 1)) do { i ->
            def atSpot = elements.at(i)
            if ((atSpot != vacant) && (atSpot != tombstone)) then {
                if (later) then {
                    result := result ++ ", "
                }
                later := true
                result := result ++ atSpot.asString
            }
        }

        result ++ "]"
    }

    method keys {
        makeIterator { bd -> bd.key }
    }

    method values {
        makeIterator { bd -> bd.value }
    }

    method bindings {
        makeIterator { bd -> bd }
    }

    method makeIterator(transform) is confidential {
        object {
            method size {
                itemSize
            }
            method do(blk) {
                for (0..(arraySize - 1)) do { i ->
                    def atSpot = elements.at(i)
                    if ((atSpot != vacant) && (atSpot != tombstone)) then {
                        blk.apply(transform.apply(atSpot))
                    }
                }
            }
        }
    }

    method findIndex(key) {
        // Would use key.hash if that were defined everywhere; degenerate linear search.
        def hash = key.hash
        var spot := hash % arraySize
        while {
                def atSpot = elements.at(spot)
                (atSpot.key != key) && (atSpot != tombstone) && (atSpot != vacant)
            } do {
                spot := (spot + 1) % arraySize
        }
        return spot
    }

    method ensureCapacity(sz) {
        if (sz > (arraySize * MAX_FILL)) then {
            def newSize = arraySize * 2
            def newElements = primitiveArray.new(newSize)
            for (0..(newSize-1)) do { i ->
                newElements.at(i) put(vacant)
            }
            for (0..(arraySize - 1)) do { i ->
                def atSpot = elements.at(i)
                if ((atSpot != vacant) && (atSpot != tombstone)) then {
                    var index := atSpot.key.hash % newSize
                    while { newElements.at(index) != vacant } do {
                        index := (index + 1) % newSize
                    }
                    newElements.at(index) put(atSpot)
                }
            }
            elements := newElements
            arraySize := newSize
        }
    }
}

def vacant = binding(object {var vacant}, "vacant")
def tombstone = binding(object {var tombstone}, "tombstone")
