' Binary Search Tree.
' It's a simple implementation for test/demonstration purposes of the pointer support;
' no balancing is done and memory is not freed when elements are removed.

IMPORT textio
ZEROPAGE basicsafe

MODULE main
    SUB start()
        FOR EACH cx16.r0 IN [321, 719, 194, 550, 187, 203, 520, 562, 221, 676, 97, 852, 273, 326, 589, 606, 275, 794, 63, 716] : btree.add(cx16.r0) : NEXT

        txt.print_ub(btree.size())
        txt.print(" sorted values: ")
        btree.print_tree_inorder()
        txt.print("'tree' form:\n")
        btree.print_tree_preorder()

        txt.print("203 in tree? ")
        txt.print_bool(btree.contains(203))
        txt.print("\n204 in tree? ")
        txt.print_bool(btree.contains(204))
        txt.print("\n605 in tree? ")
        txt.print_bool(btree.contains(605))
        txt.print("\n606 in tree? ")
        txt.print_bool(btree.contains(606))
        txt.nl()

        txt.print("removing some numbers.\n")
        btree.remove(9999)
        btree.remove(97)
        btree.remove(187)
        btree.remove(203)
        btree.remove(275)
        btree.remove(321)
        btree.remove(520)
        btree.remove(562)
        btree.remove(606)
        btree.remove(719)
        btree.remove(794)

        txt.print_ub(btree.size())
        txt.print(" sorted values: ")
        btree.print_tree_inorder()
        txt.print("'tree' form:\n")
        btree.print_tree_preorder()
    END SUB
END MODULE

MODULE btree
    TYPE Node
        left AS ^^Node
        right AS ^^Node
        value AS UWORD
    END TYPE

    DIM root AS ^^Node = 0

    SUB add(value AS UWORD)
        DIM node AS ^^Node = arena.alloc(SIZEOF(Node))
        node.value = value
        node.left = node.right = 0  'Testing a comment

        IF root = 0 THEN
            root = node
        ELSE
            DIM parent AS ^^Node = root
            REPEAT
                IF parent.value >= value THEN
                    IF parent.left <> 0 THEN
                        parent = parent.left
                    ELSE
                        parent.left = node
                        RETURN
                    END IF
                ELSE
                    IF parent.right <> 0 THEN
                        parent = parent.right
                    ELSE
                        parent.right = node
                        RETURN
                    END IF
                END IF
            END REPEAT
        END IF
    END SUB

    FUNCTION contains(value AS UWORD) AS BOOL
        DIM r AS ^^Node = root
        WHILE r <> 0
            IF r.value = value THEN RETURN TRUE
            IF r.value > value THEN
                r = r.left
            ELSE
                r = r.right
            END IF
        WEND
        RETURN FALSE
    END FUNCTION

    FUNCTION size() AS UBYTE
        DIM count AS UBYTE

        IF root <> 0 THEN count_node(root)

        RETURN count

        SUB count_node(r AS ^^Node)
            count++
            IF r.left <> 0 THEN
                sys.pushw(r)
                count_node(r.left)
                r = sys.popw()
            END IF
            IF r.right <> 0 THEN
                sys.pushw(r)
                count_node(r.right)
                r = sys.popw()
            END IF
        END SUB
    END FUNCTION

    SUB remove(value AS UWORD)
        ' note: we don't deallocate the memory from the node, for simplicity sake
        DIM n AS ^^Node = root
        DIM parent AS ^^Node = 0
        WHILE n <> 0
            IF n.value = value THEN
                IF n.left = 0 THEN
                    replacechild(parent, n, n.right)
                ELSEIF n.right = 0 THEN
                    replacechild(parent, n, n.left)
                ELSE
                    ' Both left & right subtrees are present.
                    ' N = node to delete.
                    '    Find N's successor S. (N's right subtree's minimum element)
                    '    Attach N's left subtree to S.left (S doesn't have a left child)
                    '    Attach N's right subtree to Parent in place of N.
                    DIM successor AS ^^Node = find_successor(n)
                    successor.left = n.left
                    replacechild(parent, n, n.right)
                END IF
                RETURN
            END IF
            parent = n
            IF n.value > value THEN
                n = n.left
            ELSE
                n = n.right
            END IF
        WEND

        FUNCTION find_successor(p AS ^^Node) AS ^^Node
            DIM succ AS ^^Node = p
            p = p.right
            WHILE p <> 0
                succ = p
                p = p.left
            WEND
            RETURN succ
        END FUNCTION

        SUB replacechild(p AS ^^Node, child AS ^^Node, newchild AS ^^Node)
            IF p.left = child THEN
                p.left = newchild
            ELSE
                p.right = newchild
            END IF
        END SUB
    END SUB


    SUB print_tree_inorder()
        IF root <> 0 THEN print_tree(root)
        txt.nl()

        SUB print_tree(r AS ^^Node)
            IF r.left <> 0 THEN
                sys.pushw(r)
                print_tree(r.left)
                r = sys.popw()
            END IF
            txt.print_uw(r.value)
            txt.print(", ")
            IF r.right <> 0 THEN
                sys.pushw(r)
                print_tree(r.right)
                r = sys.popw()
            END IF
        END SUB
    END SUB


    SUB print_tree_preorder()
        IF root <> 0 THEN print_tree(root, 0)
        txt.nl()

        SUB print_tree(r AS ^^Node, depth AS UBYTE)
            REPEAT depth
                txt.print("  ")
            END REPEAT
            txt.print_uw(r.value)
            txt.nl()
            IF r.left <> 0 THEN
                sys.pushw(r)
                sys.push(depth)
                print_tree(r.left, depth + 1)
                depth = sys.pop()
                r = sys.popw()
            END IF
            IF r.right <> 0 THEN
                sys.pushw(r)
                sys.push(depth)
                print_tree(r.right, depth + 1)
                depth = sys.pop()
                r = sys.popw()
            END IF
        END SUB
    END SUB
END MODULE


MODULE arena
    ' extremely trivial arena allocator (that never frees)
    DIM buffer AS UWORD = memory("arena", 2000, 0)
    DIM nextItem AS UWORD = buffer

    FUNCTION alloc(size AS UBYTE) AS UWORD
        DEFER nextItem += size
        RETURN nextItem
    END FUNCTION
END MODULE
