Last modified: 2013-10-09 20:48:28 tkych

Version: 0.1.01


cl-intset
=========

cl-intset is a library for non-negative integer sets.
It implements sets using bitsets.


Depends-on
----------

 - NIL


Installation
------------

 0. SHELL$   `git clone https://github.com/tkych/cl-intset`
 1. CL-REPL> `(push #p"/path-to-cl-intset/cl-intset/" asdf:*central-registry*)`
 2. CL-REPL> `(ql:quickload :cl-intset)` or `(asdf:load-system :cl-intset)`


Examples
--------

`intset` and `iset` are nicknames of `cl-intset` package.


    CL-REPL> (defvar iset1 (iset:make-intset))
    ISET1
    
    CL-REPL> (iset:intset->list iset1)
    NIL

    CL-REPL> (iset:insert 42 iset1)
    #<CL-INTSET:INTSET {1007AC3EB3}>
    
    CL-REPL> (iset:intset->list iset1)
    (42)
    
    CL-REPL> (dotimes (_ 10) (iset:insert 42 iset1))
    NIL
    
    CL-REPL> (iset:intset->list iset1)
    (42)
    
    CL-REPL> (defvar iset2 (iset:list->intset '(43 41 42 43 41 42)))
    ISET2

    CL-REPL> (iset:intset->list iset2)
    (41 42 43)
    
    CL-REPL> (iset:map 'iset:intset #'1+ iset2)
    #<CL-INTSET:INTSET {1008049063}>
    
    CL-REPL> (iset:map 'list #'1+ iset2)
    (42 43 44)


Manual
------

#### [Type] INTSET

#### [Function] MAKE-INTSET => _new-intset_

#### [Function] INTSETP _object_ => _boolean_

#### [Function] EMPTYP _intset_ => _boolean_

#### [Function] SINGLETONP _intset_ => _boolean_

#### [Function] COPY _intset_ => _new-intset_

#### [Function] CLEAR _intset_ => _modified-intset_

#### [Function] SIZE _intset_ => _non-negative-integer_

#### [Function] MEMBERP _non-negative-integer_ _intset_ => _boolean_

#### [Function] SET-EQUAL _intset1_ _intset2_ => _boolean_

#### [Function] cl-intset:SUBSETP _intset1_ _intset2_ => _boolean_

#### [Function] PROPER-SUBSET-P _intset1_ _intset2_ => _boolean_

#### [Function] ADD _non-negative-integer_ _intset_ => _new-intset_

#### [Function] INSERT _non-negative-integer_ _intset_ => _modified-intset_

#### [Function] cl-intset:REMOVE _non-negative-integer_ _intset_ => _new-intset_

#### [Function] cl-intset:DELETE _non-negative-integer_ _intset_ => _modified-intset_

#### [Function] LIST->INTSET _non-negative-integer-list_ => _new-intset_

#### [Function] INTSET->LIST _intset_ _&key_ _desc?_ => _non-negative-integer-list_

#### [Function] cl-intset:INTERSECTION _intset1_ _intset2_ => _new-intset_

#### [Function] cl-intset:NINTERSECTION _intset1_ _intset2_ => _modified-intset1_

#### [Function] cl-intset:SET-DIFFERENCE _intset1_ _intset2_ => _new-intset_

#### [Function] cl-intset:NSET-DIFFERENCE _intset1_ _intset2_ => _modified-intset1_

#### [Function] cl-intset:UNION _intset1_ _intset2_ => _new-intset_

#### [Function] cl-intset:NUNION _intset1_ _intset2_ => _modified-intset1_

#### [Function] cl-intset:SET-EXCLUSIVE-OR _intset1_ _intset2_ => _new-intset_

#### [Function] cl-intset:NSET-EXCLUSIVE-OR _intset1_ _intset2_ => _modified-intset1_

#### [Function] cl-intset:RANDOM _intset_ => _non-negative-integer/null_

#### [Function] cl-intset:MAP _result-type_ _function_ _intset_ _&key_ _desc?_ => _new-object_

#### [Function] FOR-EACH _function_ _intset_ _&key_ _desc?_ => _non-modified-intset_

#### [Function] cl-intset:MAX _intset_ => _non-negative-integer/null_

#### [Function] cl-intset:MIN _intset_ => _non-negative-integer/null_


Author, License, Copyright
--------------------------

 - Takaya OCHIAI  <#.(reverse "moc.liamg@lper.hcykt")>

 - MIT License

 - Copyright (C) 2013 Takaya OCHIAI
