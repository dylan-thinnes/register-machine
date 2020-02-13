l1 : decjz r0 l2
      inc r-1
      inc r-2
      decjz r-4 l1
l2 : decjz r-1 l3
l4 :   decjz r-2 l5
        inc r0
        inc r-3
        decjz r-4 l4
l5 :   decjz r-3 l2
        inc r-2
        decjz r-4 l5
l3 : decjz r-2 end
      decjz r-4 l3
