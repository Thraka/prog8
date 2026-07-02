%zeropage basicsafe

main {
    ubyte[4][3] grid
    ubyte @shared result

    sub start() {
        grid[1][2] = 9
        result = grid[1][2]
    }
}
