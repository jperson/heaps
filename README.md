#HEAPS  

Common lisp implementation of min/max heaps.  Uses a dlambda (see let over lambda) interface for heap objects.

Loading and using a max-heap:  

    CL-USER> (ql:quickload "heaps")  
    CL-USER> (heaps:max-heap 'h :contents '(0 1 2 3 4 5 6 7 8 9))
    CL-USER> (h :get-max)
    CL-USER> 9
    CL-USER> (h :get-size)
    CL-USER> 10
    CL-USER> (h :extract-max)
    CL-USER> 9
    CL-USER> (h :get-size)
    CL-USER> 9
    CL-USER> (h :insert 15)
    CL-USER> 15
    CL-USER> (h: get-size)
    CL-USER> 10
    CL-USER> (h :get-max)
    CL-USER> 15  
    
####Max-heap interface

####(max-heap n &key size contents)  
Creates a new max-heap object named n of size size or with contents contents.  The value of n must be a symbol. 

    CL-USER> (heaps:max-heap 'my-heap :contents '(1 2 3 4 5 6 7 8 9 10))
Creates the max-heap my-heap with values 1..10.

    CL-USER> (heaps:max-heap 'my-heap :size 10)
Creates the max-heap my-heap with size 10 and contents '().

    CL-USER> (heaps:max-heap 'my-heap)  
Creates the empty max-heap my-heap.



####:get-max
Returns the max value for the heap object :get-max is invoked upon.

    CL-USER> (heaps:max-heap 'my-heap :contents '(0 1 2 3 4 5 6 7 8 9))
    CL-USER> (my-heap :get-max)
    CL-USER> 9  
    
    
####:get-size
Returns the size of the heap object :get-size is invoked upon.

    CL-USER> (heaps:max-heap 'my-heap :contents '(0 1 2 3 4 5 6 7 8 9))
    CL-USER> (my-heap :get-size)
    CL-USER> 10

####:insert v
Inserts value v into the heap object :insert is invoked upon.  Returns the value inserted.

    CL-USER> (heaps:max-heap 'my-heap :contents '(0 1 2 3 4 5 6 7 8 9))
    CL-USER> (my-heap :insert 15)
    CL-USER> 15
    
####:extract-max
Removes and returns the max value for the heap object :extract-max is invoked upon.

    CL-USER> (heaps:max-heap 'my-heap :contents '(0 1 2 3 4 5 6 7 8 9))
    CL-USER> (my-heap :extract-max)
    CL-USER> 9
    
