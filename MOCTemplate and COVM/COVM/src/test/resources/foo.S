-- The example from the attachment of milestone 1a

--  current instruction   stack
--  Code of MAIN
    pushint 1             -- stack: []
    pushint 5             -- stack: 1 : []
    pushint 3             -- stack: 5 : 1 : []
    pushaddr 7            -- stack: 3 : 5 : 1 : []
    call                  -- stack: @7 : 3 : 5 : 1 []
    slide 3               -- stack: 3 : 3 : 5 : 1 : []
    stop                  -- stack: 3 : []

--  Code of foo
    push 3                -- stack: @5 : 3 : 5 : 1 : []
    push 3                -- stack: 1 : @5 : 3 : 5 : 1 : []
    push 3                -- stack: 5 : 1 : @5 : 3 : 5 : 1 : []
    sub                   -- stack: 3 : 5 : 1 : @5 : 3 : 5 : 1 : []
    add                   -- stack: 2 : 1 : @5 : 3 : 5 : 1 : []
    ret                   -- stack: 3 : @5 : 3 : 5 : 1 : []
